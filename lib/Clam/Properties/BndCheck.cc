#include "clam/config.h"
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
  uint32_t m_trivialChecks;
  // to avoid to instrument the same check
  SmallSet<Value *, 16> m_seen;
  const DataLayout * m_dl;
  using ref_bnd_map_t = llvm::DenseMap<llvm::Value *, std::pair<var_t, var_t>>;
  // map llvm pointer to pair of object size and offset
  ref_bnd_map_t m_ref_bnd_map;

  // Private function here:
  // Decide if the check is relevant
  bool isRelevantCheck(Value *Ptr) {
    auto BasePair = memory_check_utils::getBasePtr(Ptr);
    if (Value *BasePtr = BasePair.getPointer()) {
      if (BasePair.getInt() == 1) {
	CRAB_LOG("cfg-bnd-check-trivial",
		 llvm::errs() << "Skipped " << *BasePtr
		 << " because it is dereferenceable!\n";);
        return false;
      }
      // We've checked BasePtr in the current BB.
      if (!m_seen.insert(BasePtr).second) {
	CRAB_LOG("cfg-bnd-check-trivial",
		 llvm::errs() << "Skipped " << *BasePtr << " because already checked!\n";);
        return false;
      }
    }
    return true;
  }

  // Get datalayout by given an instruction
  const DataLayout *getInsDatalayout(Instruction &I) {
    if (!m_dl) {
      const llvm::Module &m = *I.getParent()->getParent()->getParent();
      m_dl = &m.getDataLayout();
    }
    return m_dl;
  }

  // Get maximum width in bits through datalayout
  unsigned getMaxWidthInBits(Instruction &I) {
    if (!m_dl) {
      m_dl = getInsDatalayout(I);
    }
    return m_dl->getPointerSizeInBits();
  }

  // Add bound check assertions:
  //   assert(offset >= 0)
  //   assert(size >= offset + size_of(value))
  // where offset indicates the offset of current pointer
  // the size means the total size of allocation
  // sizeof(value) is the number of bytes being accessed
  void addBndCheckAssertion(Instruction &I, const var_t &reg, basic_block_t &bb,
                            bool is_load_ref) {
    if (m_params.check_only_typed_regions &&
        reg.get_type().is_unknown_region()) {
      return;
    }
    // We ignore for now the option
    // m_params.check_only_noncyclic_regions.  To consider it the
    // easiest way is to extend CrabStoreRefOps with information
    // about the HeapAbstraction Region such as whether
    // isSequence(), IsCyclic(), etc.

    llvm::Value *vptr;
    if (LoadInst *loadI = dyn_cast<LoadInst>(&I))
      vptr = loadI->getPointerOperand();
    else if (StoreInst *storeI = dyn_cast<StoreInst>(&I))
      vptr = storeI->getPointerOperand();
    else
      CLAM_ERROR(
          "Expect adding bound check assertions at load/store instrucitons");
    auto it = m_ref_bnd_map.find(vptr);
    if (it == m_ref_bnd_map.end()) {
      CLAM_WARNING("Could not find " << (is_load_ref ? "load"
                                                    : "store")
                                                          << " ptr from map "
                                                          << *vptr);
    } else {
      CRAB_LOG("cfg-bnd-check",
               llvm::errs() << (is_load_ref
                   ? "load"
                   : "store")
                         << ": check size and offset are in bounds: " << I
                         << "\n");
      var_t size = it->second.first;
      var_t offset = it->second.second;
      Type *elemTy = vptr->getType()->getPointerElementType();
      const llvm::DataLayout *dl = getInsDatalayout(I);
      unsigned size_of = dl->getTypeSizeInBits(elemTy) / 8;
      bb.assertion(offset >= number_t(0), getDebugLoc(&I, m_assertionId++));
      bb.assertion(size >= offset + size_of, getDebugLoc(&I, m_assertionId++));
      m_addedChecks++;
    }
  }

public:
  EmitBndChecksImpl(const CrabBuilderParams &params, crabLitFactory &lfac,
                    uint32_t &assertionId)
      : m_params(params), m_lfac(lfac), m_assertionId(assertionId),
        m_addedChecks(0), m_dl(nullptr) {
    assert(m_params.add_bounds_checks || m_params.add_is_deref);
  }

  ~EmitBndChecksImpl() {
    if (m_params.add_bounds_checks) {    
      llvm::errs() << "-- Inserted " << m_addedChecks;
      llvm::errs() << " bound checks\n";
      llvm::errs() << " (skipped " << m_trivialChecks << " trivial checks).\n";
    }
  }

  void visitBeforeBasicBlock(llvm::BasicBlock &BB) {
    if (m_params.add_bounds_checks) {        
      // We cache checks only in the same basic block
      m_seen.clear();
    }
  }

  void visitBeforeStore(llvm::StoreInst &I, CrabStoreRefOps &s) {
    if (m_params.add_bounds_checks) {
      if (!isRelevantCheck(I.getOperand(1))) {
	m_trivialChecks++;
      } else {
	addBndCheckAssertion(I, s.getRegion(), s.getBasicBlock(), false);
      }
    }
  }

  void visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
    if (m_params.add_bounds_checks) {    
      if (!isRelevantCheck(I.getOperand(0))) {
	m_trivialChecks++;
      } else {
	addBndCheckAssertion(I, s.getRegion(), s.getBasicBlock(), true);
      }
    }
  }

  void visitAfterLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
    var_t lhs = s.getLhs();
    basic_block_t &bb = s.getBasicBlock();
    if (lhs.get_type().is_reference()) {
      var_t ref_obj_sz = m_lfac.mkIntVar(getMaxWidthInBits(I));
      // obtain an overappriximation of deref
      // using havoc to initialize size and offset
      bb.havoc(ref_obj_sz, "approx size when loading a ptr of ptr");
      var_t ref_offset = m_lfac.mkIntVar(getMaxWidthInBits(I));
      bb.havoc(ref_offset, "approx offset when loading a ptr of ptr");

      // add pair to the map by llvm pointer
      m_ref_bnd_map.insert({&I, {ref_obj_sz, ref_offset}});
    }
  }

  // Add the CrabIR:
  //   p_offset := 0;
  //   p_size := sz;
  // where p_offset indicates the offset of current pointer, the and
  // p_size means the total size of allocation
  void visitAfterAlloc(llvm::Instruction &I, const llvm::TargetLibraryInfo &tli,
                       CrabMakeRefOps &s) {
    
    basic_block_t &bb = s.getBasicBlock();
    CRAB_LOG("cfg-bnd-check",
             llvm::errs() << "alloc: add size and offset for " << I << "\n");
    var_or_cst_t size = s.getSize();
    if (!size.get_type().is_integer()) {
      CLAM_WARNING("Unexpected type of alloc size operand");
      return;
    }
    unsigned width = size.get_type().get_integer_bitwidth();
    var_t ref_obj_sz = m_lfac.mkIntVar(getMaxWidthInBits(I));
    if (getMaxWidthInBits(I) != width) {
      CLAM_ERROR("Unexpected bit width from allocation "
                 << width << " the ghost variable requires "
                 << getMaxWidthInBits(I));
    }
    if (size.is_variable()) {
      bb.assign(ref_obj_sz, size.get_variable());
    } else {
      bb.assign(ref_obj_sz, size.get_constant());
    }
    // create a var_t for offset and assign it to zero
    var_t ref_offset = m_lfac.mkIntVar(getMaxWidthInBits(I));
    bb.assign(ref_offset, number_t(0));

    // add pair to the map by llvm pointer
    m_ref_bnd_map.insert({&I, {ref_obj_sz, ref_offset}});
  }

  // Add the CrabIR:
  //   p_offset := ptr_offset + offset;
  //   p_size := ptr_size;
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
    Value *vptr = I.getOperand(0);
    unsigned width = getMaxWidthInBits(I);
    if (isa<ConstantPointerNull>(*vptr)) { // Get a constant null ptr
      var_t lhs_obj_sz = m_lfac.mkIntVar(width);
      var_t lhs_offset = m_lfac.mkIntVar(width);
      bb.assign(lhs_obj_sz, number_t(0));
      bb.assign(lhs_offset, number_t(0));
      m_ref_bnd_map.insert({&I, {lhs_obj_sz, lhs_offset}});
    }
    else {
      auto it = m_ref_bnd_map.find(vptr);
      if (it == m_ref_bnd_map.end()) {
        CLAM_WARNING("Could not find gep ptr "
                  << *vptr << " from reference map" << I);
      } else {
        var_t ptr_obj_sz = it->second.first;
        var_t ptr_offset = it->second.second;
	if (offset.is_constant() && (offset.constant() == number_t(0))) {
	  m_ref_bnd_map.insert({&I, {ptr_obj_sz, ptr_offset}});
	} else {
	  var_t lhs_offset = m_lfac.mkIntVar(width);
	  bb.assign(lhs_offset, offset + ptr_offset);
	  m_ref_bnd_map.insert({&I, {ptr_obj_sz, lhs_offset}});
	} 
        // var_t lhs_obj_sz = m_lfac.mkIntVar(width);
        // var_t lhs_offset = m_lfac.mkIntVar(width);
        // bb.assign(lhs_obj_sz, ptr_obj_sz);
        // bb.assign(lhs_offset, offset + ptr_offset);
        // m_ref_bnd_map.insert({&I, {lhs_obj_sz, lhs_offset}});
      }
    }
  }

  // Given p = select_ref (cond, q, r), add the CrabIR: 
  //   p.offset := select(cond, q.offset, r.offset)
  //   p.size := select(cond, q.size, r.size)
  // where p.offset indicates the offset of selected pointer
  // the p.size means the total size of allocation
  void visitAfterRefSelect(SelectInst &I, CrabSelectRefOps &s) {
    const Value &vtrue = *(I.getTrueValue());
    const Value &vfalse = *(I.getFalseValue());
    basic_block_t &bb = s.getBasicBlock();
    unsigned width = getMaxWidthInBits(I);
    var_t inst_cond = s.getRefCond();
    var_t cond = m_lfac.mkIntVar(width);
    bb.zext(inst_cond, cond); // extend bool into int
    var_t offset_op1 = m_lfac.mkIntVar(width);
    var_t size_op1 = m_lfac.mkIntVar(width);
    var_t offset_op2 = m_lfac.mkIntVar(width);
    var_t size_op2 = m_lfac.mkIntVar(width);
    if (!s.getOp1().hasValue()) { // op1 is nullptr
      bb.assign(offset_op1, number_t(0));
      bb.assign(size_op1, number_t(0));
    } else {
      assert(s.getOp1().hasValue());
      auto it = m_ref_bnd_map.find(&vtrue);
      if (it == m_ref_bnd_map.end()) {
        CLAM_WARNING("Could not find from select ptr "
                     << vtrue << " from reference map" << I);
        return;
      } else {
        size_op1 = it->second.first;
        offset_op1 = it->second.second;
      }
    }
    if (!s.getOp2().hasValue()) { // op2 is nullptr
      bb.assign(offset_op2, number_t(0));
      bb.assign(size_op2, number_t(0));
    } else {
      assert(s.getOp2().hasValue());
      auto it = m_ref_bnd_map.find(&vfalse);
      if (it == m_ref_bnd_map.end()) {
        CLAM_WARNING("Could not find from select ptr "
                     << vfalse << " from reference map" << I);
        return;
      } else {
        size_op2 = it->second.first;
        offset_op2 = it->second.second;
      }
    }
    var_t lhs_obj_sz = m_lfac.mkIntVar(width);
    var_t lhs_offset = m_lfac.mkIntVar(width);
    bb.select(lhs_offset, cond, offset_op1, offset_op2);
    bb.select(lhs_obj_sz, cond, size_op1, size_op2);
    m_ref_bnd_map.insert({&I, {lhs_obj_sz, lhs_offset}});
  }

  // Given b = is_deref(reg, ref, offset), add the CrabIR:
  //   check1 := ref.offset >= 0
  //   check2 := ref.offset + offset <= ref.size
  //   b := check1 && check2
  // where ref.offset indicates the offset of argument pointer
  // the ref.size means the total size of allocation
  void visitAfterIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) {

    if (!m_params.add_is_deref) {
      return;
    }
    
    Value *vRef = I.getArgOperand(0);
    basic_block_t &bb = s.getBasicBlock();
    auto it = m_ref_bnd_map.find(vRef);
    if (it == m_ref_bnd_map.end()) {
      CLAM_WARNING("Could not find arg of is_deref ptr "
                   << *vRef << " from reference map" << I);
    } else {
      var_t ptr_obj_sz = it->second.first;
      var_t ptr_offset = it->second.second;
      var_t first_check = m_lfac.mkBoolVar();
      var_t second_check = m_lfac.mkBoolVar();
      var_or_cst_t size_of = s.getSize();
      bb.bool_assign(first_check, ptr_offset >= number_t(0));
      if (size_of.is_variable())
        bb.bool_assign(second_check,
                       ptr_obj_sz >= ptr_offset + size_of.get_variable());
      else
        bb.bool_assign(second_check,
                       ptr_obj_sz >= ptr_offset + size_of.get_constant());
      bb.bool_and(s.getLhs(), first_check, second_check);
#ifdef CLAM_IS_TOPLEVEL
      // If clam is called from SeaHorn then we don't want to add this
      // assertion. Note that seahorn is the only client that adds
      // calls to sea.is_dereferenceable.
      bb.bool_assert(s.getLhs(),getDebugLoc(&I, m_assertionId++));
#endif       
    }
  }
};

EmitBndChecks::EmitBndChecks(const CrabBuilderParams &params,
                             crabLitFactory &lfac, uint32_t &assertionId) {
  this->m_impl = std::make_unique<EmitBndChecksImpl>(params, lfac, assertionId);
}

EmitBndChecks::~EmitBndChecks() {}

void EmitBndChecks::visitBeforeBasicBlock(llvm::BasicBlock &BB) {
  // We cache checks only in the same basic block
  m_impl->visitBeforeBasicBlock(BB);
}

void EmitBndChecks::visitBeforeStore(llvm::StoreInst &I, CrabStoreRefOps &s) {
  m_impl->visitBeforeStore(I, s);
}

void EmitBndChecks::visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
  m_impl->visitBeforeLoad(I, s);
}

void EmitBndChecks::visitAfterLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
  m_impl->visitAfterLoad(I, s);
}

void EmitBndChecks::visitAfterAlloc(llvm::Instruction &I,
                                    const llvm::TargetLibraryInfo &tli,
                                    CrabMakeRefOps &s) {
  m_impl->visitAfterAlloc(I, tli, s);
}

void EmitBndChecks::visitAfterGep(llvm::Instruction &I, CrabGepRefOps &s) {
  m_impl->visitAfterGep(I, s);
}

void EmitBndChecks::visitAfterRefSelect(llvm::SelectInst &I,
                                        CrabSelectRefOps &s) {
  m_impl->visitAfterRefSelect(I, s);
}

void EmitBndChecks::visitAfterIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) {
  m_impl->visitAfterIsDeref(I, s);
}

/* Begin empty implementations */
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
void EmitBndChecks::visitBeforeMemset(llvm::MemSetInst &I, CrabMemsetOps &s) {}
void EmitBndChecks::visitAfterMemset(llvm::MemSetInst &I, CrabMemsetOps &s) {}
void EmitBndChecks::visitBeforeMemTransfer(llvm::MemTransferInst &I,
                                           CrabMemTransferOps &s) {}
void EmitBndChecks::visitAfterMemTransfer(llvm::MemTransferInst &I,
                                          CrabMemTransferOps &s) {}
void EmitBndChecks::visitBeforeRefSelect(llvm::SelectInst &I,
                                         CrabSelectRefOps &s) {}
void EmitBndChecks::visitBeforeIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) {}
/* End empty implementations */

} // end namespace clam
