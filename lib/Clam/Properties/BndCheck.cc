#include "clam/CrabIREmitter.hh"
#include "clam/Support/Debug.hh"

#include "../CfgBuilderUtils.hh"
#include "BndCheck.hh"
#include "MemoryCheckUtils.hh"

#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"

#include "crab/support/debug.hpp"

using namespace llvm;

namespace clam {

class EmitBndChecksImpl {
  const CrabBuilderParams &m_params;
  crabLitFactory &m_lfac;
  uint32_t &m_assertionId;
  uint32_t m_addedChecks;
  const DataLayout * m_dl;
  using ref_bnd_map_t = llvm::DenseMap<llvm::Value *, std::pair<var_t, var_t>>;
  // map llvm pointer to pair of object size and offset
  std::unique_ptr<ref_bnd_map_t> m_ref_bnd_map;

  // Private function here:
  // Get datalayout by given an instruction
  const DataLayout *getInsDatalayout(Instruction &I) {
    if (!m_dl) {
      const llvm::Module &m = *I.getParent()->getParent()->getParent();
      m_dl = &m.getDataLayout();
    }
    return m_dl;
  }

  // Add bound check assertions with the following pattern:
  // assert(offset >= 0)
  // assert(size > offset)
  // assert(size >= offset + size_of(value))
  // where offset indicates the offset of current pointer
  // the size means the total size of allcation
  // sizeof(value) gets the bit-width of the element
  void addBndCheckAssertion(const var_t &reg, const var_t &size,
                            const var_t &offset, basic_block_t &bb,
                            Instruction &I, unsigned size_of) {
    if (m_params.check_only_typed_regions &&
        reg.get_type().is_unknown_region()) {
      return;
    }
    // We ignore for now the option
    // m_params.check_only_noncyclic_regions.  To consider it the
    // easiest way is to extend CrabStoreRefOps with information
    // about the HeapAbstraction Region such as whether
    // isSequence(), IsCyclic(), etc.

    bb.assertion(offset >= number_t(0), getDebugLoc(&I, m_assertionId++));
    bb.assertion(size >= offset + size_of, getDebugLoc(&I, m_assertionId++));
    m_addedChecks++;
    return;
  }

  void addSelectRefBndCheck(SelectInst &I, basic_block_t &bb) {
    const Value &vtrue = *(I.getTrueValue());
    const Value &vfalse = *(I.getFalseValue());
    const Value &condV = *I.getCondition();
    crab_lit_ref_t c = m_lfac.getLit(condV);
    assert(c);
    crab_lit_ref_t op1 = m_lfac.getLit(vtrue);
    assert(op1);
    crab_lit_ref_t op2 = m_lfac.getLit(vfalse);
    assert(op2);
    Type *elemTy = vtrue.getType()->getPointerElementType();
    const llvm::DataLayout *dl = getInsDatalayout(I);
    unsigned width = dl->getTypeSizeInBits(elemTy);
    var_t offset_op1 = m_lfac.mkIntVar(width);
    var_t size_op1 = m_lfac.mkIntVar(width);
    var_t offset_op2 = m_lfac.mkIntVar(width);
    var_t size_op2 = m_lfac.mkIntVar(width);
    if (m_lfac.isRefNull(op1)) {
      bb.assign(offset_op1, number_t(0));
      bb.assign(size_op1, number_t(0));
    } else {
      auto it = m_ref_bnd_map->find(&vtrue);
      if (it == m_ref_bnd_map->end()) {
        CLAM_ERROR("Could not find from select ptr "
                   << vtrue << " from reference map \n Stop at" << I);
        return;
      } else {
        offset_op1 = it->second.first;
        size_op1 = it->second.second;
      }
    }
    if (m_lfac.isRefNull(op2)) {
      bb.assign(offset_op2, number_t(0));
      bb.assign(size_op2, number_t(0));
    } else {
      auto it = m_ref_bnd_map->find(&vfalse);
      if (it == m_ref_bnd_map->end()) {
        CLAM_ERROR("Could not find from select ptr "
                   << vfalse << " from reference map \n Stop at" << I);
        return;
      } else {
        offset_op2 = it->second.first;
        size_op2 = it->second.second;
      }
    }
    var_t lhs_obj_sz = m_lfac.mkIntVar(width);
    var_t lhs_offset = m_lfac.mkIntVar(width);
    bb.select(lhs_offset, c->getVar(), offset_op1, offset_op2);
    bb.select(lhs_obj_sz, c->getVar(), size_op1, size_op2);
    m_ref_bnd_map->insert({&I, {lhs_obj_sz, lhs_offset}});
  }

public:
  EmitBndChecksImpl(const CrabBuilderParams &params, crabLitFactory &lfac,
                    uint32_t &assertionId)
      : m_params(params), m_lfac(lfac), m_assertionId(assertionId),
        m_addedChecks(0), m_dl(nullptr), m_ref_bnd_map(std::make_unique<ref_bnd_map_t>()) {}

  ~EmitBndChecksImpl() {
    llvm::errs() << "-- Inserted " << m_addedChecks;
    llvm::errs() << " bound checks\n";
  }

  void visitBeforeStore(llvm::StoreInst &I, CrabStoreRefOps &s) {
    Value *key = I.getPointerOperand();
    basic_block_t &bb = s.getBasicBlock();
    auto it = m_ref_bnd_map->find(key);
    if (it == m_ref_bnd_map->end()) {
      CLAM_WARNING("Could not find store ptr from map " << *key);
    } else {
      CRAB_LOG("cfg-bnd-check",
               llvm::errs()
                   << "store: check size and offset are in bounds: " << I
                   << "\n");
      var_t ptr_obj_sz = it->second.first;
      var_t ptr_offset = it->second.second;
      Type *elemTy = key->getType()->getPointerElementType();
      const llvm::DataLayout *dl = getInsDatalayout(I);
      unsigned size_of = dl->getTypeSizeInBits(elemTy) / 8;
      addBndCheckAssertion(s.getRegion(), ptr_obj_sz, ptr_offset, bb, I, size_of);
    }
  }

  void visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
    Value *key = I.getPointerOperand();
    basic_block_t &bb = s.getBasicBlock();
    auto it = m_ref_bnd_map->find(key);
    if (it == m_ref_bnd_map->end()) {
      CLAM_WARNING("Could not find load ptr from map " << *key);
    } else {
      CRAB_LOG("cfg-bnd-check",
               llvm::errs()
                   << "load: check size and offset are in bounds: " << I
                   << "\n");
      var_t ptr_obj_sz = it->second.first;
      var_t ptr_offset = it->second.second;
      Type *elemTy = key->getType()->getPointerElementType();
      const llvm::DataLayout *dl = getInsDatalayout(I);
      unsigned size_of = dl->getTypeSizeInBits(elemTy) / 8;
      addBndCheckAssertion(s.getRegion(), ptr_obj_sz, ptr_offset, bb, I, size_of);
      crab_lit_ref_t lhs = m_lfac.getLit(I);
      if (lhs->getVar().get_type().is_reference()) {
        var_t ref_obj_sz =
            m_lfac.mkIntVar(ptr_obj_sz.get_type().get_integer_bitwidth());
        // obtain an overappriximation of deref
        // using havoc to initialize size and offset
        bb.havoc(ref_obj_sz, "approx size when loading a ptr of ptr");
        var_t ref_offset =
            m_lfac.mkIntVar(ptr_offset.get_type().get_integer_bitwidth());
        bb.havoc(ref_offset, "approx offset when loading a ptr of ptr");

        // add pair to the map by llvm pointer
        Value *ref_v = llvm::cast<llvm::Value>(&I);
        m_ref_bnd_map->insert({ref_v, {ref_obj_sz, ref_offset}});
      }
    }
  }

  // -- alloc site ptr bound utilities construction
  // From LLVM IR to crab IR we perform the following transformation:
  // p := alloc(sz) => p := make_ref(region, sz)
  // We use the idea from fat pointer like by adding two additional variables:
  // p_offset := 0;
  // p_size := sz;
  // where offset indicates the offset of current pointer
  // the size means the total size of allcation
  void visitAfterAlloc(llvm::Instruction &I, const llvm::TargetLibraryInfo &tli,
                       CrabMakeRefOps &s) {
    // llvm::errs() << I << "\n";
    // llvm::errs() << *I.getOperand(0) << "\n";
    basic_block_t &bb = s.getBasicBlock();
    CRAB_LOG("cfg-bnd-check",
             llvm::errs() << "alloc: add size and offset for " << I << "\n");
    llvm::Value *op;
    unsigned sz;
    if (isa<AllocaInst>(I)) {
      AllocaInst *allocaI = dyn_cast<AllocaInst>(&I);
      op = allocaI->getArraySize();
      const llvm::DataLayout *dl = getInsDatalayout(*allocaI);
      sz = dl->getTypeSizeInBits(allocaI->getAllocatedType()) / 8;
    } else {
      op = I.getOperand(0);
    }
    llvm::Value &operand = *op;
    crab_lit_ref_t opd = m_lfac.getLit(operand);
    if (!opd->isInt()) {
      CLAM_WARNING("Unexpected type of alloc size operand");
      return;
    }
    assert(opd->isInt());
    auto Iopd = std::static_pointer_cast<const crabIntLit>(opd);
    unsigned width = Iopd->getBitwidth();
    var_t ref_obj_sz = m_lfac.mkIntVar(width);
    if (isa<AllocaInst>(I))
      bb.assign(ref_obj_sz, m_lfac.getExp(opd) * sz);
    else
      bb.assign(ref_obj_sz, m_lfac.getExp(opd));
    // create a var_t for offset and assign it to zero
    var_t ref_offset = m_lfac.mkIntVar(width);
    bb.assign(ref_offset, number_t(0));

    // add pair to the map by llvm pointer
    m_ref_bnd_map->insert({&I, {ref_obj_sz, ref_offset}});
  }

  // -- get element ptr additional utilities construction
  // From LLVM IR to crab IR we perform the following transformation:
  // p := gep(ptr, offset) => p := gep_ref(region, ptr + offset)
  // We use the idea from fat pointer like by adding two additional variables:
  // p_offset := ptr_offset + offset;
  // p_size := ptr_size;
  // where p_offset indicates the offset of element pointer
  // the p_size means the total size of allcation
  // the element ptr should refere the same size as the original one
  void visitAfterGep(llvm::Instruction &I, CrabGepRefOps &s) {
    if (!isa<PHINode>(I) && !isa<BitCastInst>(I) &&
        !isa<GetElementPtrInst>(I)) {
      return;
    }
    lin_exp_t offset = s.getOffset();
    basic_block_t &bb = s.getBasicBlock();
    CRAB_LOG("cfg-bnd-check",
             llvm::errs() << "Gep: add two extra variable for " << I << "\n");
    Value *vlhs = llvm::cast<llvm::Value>(&I);
    Value *vptr = I.getOperand(0);
    auto it = m_ref_bnd_map->find(vptr);
    if (it == m_ref_bnd_map->end()) {
      CLAM_ERROR("Could not find gep ptr "
                 << *vptr << " from reference map \n Stop at" << I);
    } else {
      var_t ptr_obj_sz = it->second.first;
      var_t ptr_offset = it->second.second;
      var_t lhs_obj_sz =
          m_lfac.mkIntVar(ptr_obj_sz.get_type().get_integer_bitwidth());
      var_t lhs_offset =
          m_lfac.mkIntVar(ptr_offset.get_type().get_integer_bitwidth());
      bb.assign(lhs_obj_sz, ptr_obj_sz);
      bb.assign(lhs_offset, offset);
      m_ref_bnd_map->insert({&I, {lhs_obj_sz, lhs_offset}});
    }
  }
};

EmitBndChecks::EmitBndChecks(const CrabBuilderParams &params,
                             crabLitFactory &lfac, uint32_t &assertionId) {
  this->m_impl = std::make_unique<EmitBndChecksImpl>(params, lfac, assertionId);
}

EmitBndChecks::~EmitBndChecks() {}

void EmitBndChecks::visitBeforeStore(llvm::StoreInst &I, CrabStoreRefOps &s) {
  m_impl->visitBeforeStore(I, s);
}

void EmitBndChecks::visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
  m_impl->visitBeforeLoad(I, s);
}

void EmitBndChecks::visitAfterAlloc(llvm::Instruction &I,
                                    const llvm::TargetLibraryInfo &tli,
                                    CrabMakeRefOps &s) {
  m_impl->visitAfterAlloc(I, tli, s);
}

void EmitBndChecks::visitAfterGep(llvm::Instruction &I, CrabGepRefOps &s) {
  m_impl->visitAfterGep(I, s);
}

/* Begin empty implementations */
void EmitBndChecks::visitBeforeBasicBlock(llvm::BasicBlock &BB) {}
void EmitBndChecks::visitAfterBasicBlock(llvm::BasicBlock &BB) {}
void EmitBndChecks::visitBeforeAlloc(llvm::Instruction &I,
                                     const llvm::TargetLibraryInfo &tli,
                                     CrabMakeRefOps &s) {}
void EmitBndChecks::visitBeforeFree(llvm::Instruction &I,
                                    const llvm::TargetLibraryInfo &tli,
                                    CrabRemoveRefOps &s) {}
void EmitBndChecks::visitAfterFree(llvm::Instruction &I,
                                   const llvm::TargetLibraryInfo &tli,
                                   CrabRemoveRefOps &s) {}
void EmitBndChecks::visitBeforeGep(llvm::Instruction &I, CrabGepRefOps &s) {}
void EmitBndChecks::visitAfterStore(llvm::StoreInst &I, CrabStoreRefOps &s) {}
void EmitBndChecks::visitAfterLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {}
void EmitBndChecks::visitBeforeMemset(llvm::MemSetInst &I, CrabMemsetOps &s) {}
void EmitBndChecks::visitAfterMemset(llvm::MemSetInst &I, CrabMemsetOps &s) {}
void EmitBndChecks::visitBeforeMemTransfer(llvm::MemTransferInst &I,
                                           CrabMemTransferOps &s) {}
void EmitBndChecks::visitAfterMemTransfer(llvm::MemTransferInst &I,
                                          CrabMemTransferOps &s) {}
/* End empty implementations */

} // end namespace clam
