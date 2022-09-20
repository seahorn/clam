#include "clam/CrabIREmitter.hh"

#include "../CfgBuilderUtils.hh"
#include "MemoryCheckUtils.hh"
#include "NullCheck.hh"

#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

namespace clam {

/*
  Before:
       store_to_ref(reg, ref, val);
  After:
       assert_ref(ref > null);
       store_to_ref(reg, ref, val);
*/

class EmitNullDerefChecksImpl {
  const CrabBuilderParams &m_params;
  crabLitFactory &m_lfac;
  uint32_t &m_assertionId;
  uint32_t m_addedChecks;
  uint32_t m_trivialChecks;
  // to avoid to instrument the same check
  SmallSet<Value *, 16> m_seen;

  // Decide if the check is relevant
  bool isRelevantCheck(Value *Ptr) {    
    auto BasePair = memory_check_utils::getBasePtr(Ptr);
    if (Value *BasePtr = BasePair.getPointer()) {
      if (BasePair.getInt() == 1) {
        // errs() << "Skipped " << *BasePtr << " because it is
        // dereferenceable!\n";
        return false;
      }
      // We've checked BasePtr in the current BB.
      if (!m_seen.insert(BasePtr).second) {
        // errs() << "Skipped " << *BasePtr << " because already checked!\n";
        return false;
      }
    }
    return true;
  }

  void addCrabAssertion(const var_t &reg, const var_t &ref, basic_block_t &bb,
                        Instruction &I) {
    if (m_params.check_only_typed_regions &&
        reg.get_type().is_unknown_region()) {
      return;
    }
    // We ignore for now the option
    // m_params.check_only_noncyclic_regions.  To consider it the
    // easiest way is to extend CrabStoreRefOps with information
    // about the HeapAbstraction Region such as whether
    // isSequence(), IsCyclic(), etc.
    
    // Note that we add ref > null rather than ref != null
    bb.assert_ref(ref_cst_t::mk_gt_null(ref), getDebugLoc(&I, m_assertionId++));
    m_addedChecks++;
  }

public:
  EmitNullDerefChecksImpl(const CrabBuilderParams &params, crabLitFactory &lfac,
                          uint32_t &assertionId)
      : m_params(params), m_lfac(lfac), m_assertionId(assertionId),
        m_addedChecks(0), m_trivialChecks(0) {
    (void) m_lfac; // to silent compiler warning
  }

  ~EmitNullDerefChecksImpl() {
    llvm::errs() << "-- Inserted " << m_addedChecks;
    llvm::errs() << " null dereference checks ";
    llvm::errs() << " (skipped " << m_trivialChecks << " trivial checks).\n";
  }

  void visitBeforeBasicBlock(llvm::BasicBlock &BB) {
    // We cache checks only in the same basic block
    m_seen.clear();
  }

  void visitBeforeStore(llvm::StoreInst &I, CrabStoreRefOps &s) {
    if (!isRelevantCheck(I.getOperand(1))) {
      m_trivialChecks++;
    } else {
      addCrabAssertion(s.getRegion(), s.getRef(), s.getBasicBlock(), I);
    }
  }

  void visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
    if (!isRelevantCheck(I.getOperand(0))) {
      m_trivialChecks++;
    } else {
      addCrabAssertion(s.getRegion(), s.getRef(), s.getBasicBlock(), I);
    }
  }

  void visitBeforeMemset(llvm::MemSetInst &I, CrabMemsetOps &s) {
    if (!isRelevantCheck(I.getDest())) {
      m_trivialChecks++;
    } else {
      addCrabAssertion(s.getRegion(), s.getRef(), s.getBasicBlock(), I);
    }
  }

  void visitBeforeMemTransfer(llvm::MemTransferInst &I, CrabMemTransferOps &s) {
    if (!isRelevantCheck(I.getSource())) {
      m_trivialChecks++;
    } else {
      addCrabAssertion(s.getRegionSrc(), s.getRefSrc(), s.getBasicBlock(), I);
    }

    if (!isRelevantCheck(I.getDest())) {
      m_trivialChecks++;
    } else {
      addCrabAssertion(s.getRegionDst(), s.getRefDst(), s.getBasicBlock(), I);
    }
  }
};

EmitNullDerefChecks::EmitNullDerefChecks(const CrabBuilderParams &params,
                                         crabLitFactory &lfac,
                                         uint32_t &assertionId)
    : m_impl(new EmitNullDerefChecksImpl(params, lfac, assertionId)) {}

EmitNullDerefChecks::~EmitNullDerefChecks() {}

void EmitNullDerefChecks::visitBeforeBasicBlock(llvm::BasicBlock &BB) {
  m_impl->visitBeforeBasicBlock(BB);
}

void EmitNullDerefChecks::visitBeforeStore(llvm::StoreInst &I,
                                           CrabStoreRefOps &s) {
  m_impl->visitBeforeStore(I, s);
}

void EmitNullDerefChecks::visitBeforeLoad(llvm::LoadInst &I,
                                          CrabLoadRefOps &s) {
  m_impl->visitBeforeLoad(I, s);
}

void EmitNullDerefChecks::visitBeforeMemset(llvm::MemSetInst &I,
                                            CrabMemsetOps &s) {
  m_impl->visitBeforeMemset(I, s);
}

void EmitNullDerefChecks::visitBeforeMemTransfer(llvm::MemTransferInst &I,
                                                 CrabMemTransferOps &s) {
  m_impl->visitBeforeMemTransfer(I, s);
}

/* Begin empty implementations */
void EmitNullDerefChecks::visitAfterBasicBlock(llvm::BasicBlock &BB) {}
void EmitNullDerefChecks::visitBeforeAlloc(llvm::Instruction &I,
                                           const llvm::TargetLibraryInfo &tli,
                                           CrabMakeRefOps &s) {}
void EmitNullDerefChecks::visitAfterAlloc(llvm::Instruction &I,
                                          const llvm::TargetLibraryInfo &tli,
                                          CrabMakeRefOps &s) {}
void EmitNullDerefChecks::visitBeforeFree(llvm::Instruction &I,
                                          const llvm::TargetLibraryInfo &tli,
                                          CrabRemoveRefOps &s) {}
void EmitNullDerefChecks::visitAfterFree(llvm::Instruction &I,
                                         const llvm::TargetLibraryInfo &tli,
                                         CrabRemoveRefOps &s) {}
void EmitNullDerefChecks::visitBeforeGep(llvm::Instruction &I,
                                         CrabGepRefOps &s) {}
void EmitNullDerefChecks::visitAfterGep(llvm::Instruction &I,
                                        CrabGepRefOps &s) {}
void EmitNullDerefChecks::visitAfterStore(llvm::StoreInst &I,
                                          CrabStoreRefOps &s) {}
void EmitNullDerefChecks::visitAfterLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
}
void EmitNullDerefChecks::visitAfterMemset(llvm::MemSetInst &I,
                                           CrabMemsetOps &s) {}
void EmitNullDerefChecks::visitAfterMemTransfer(llvm::MemTransferInst &I,
                                                CrabMemTransferOps &s) {}
void EmitNullDerefChecks::visitBeforeRefSelect(llvm::SelectInst &I,
                                               CrabSelectRefOps &s) {}
void EmitNullDerefChecks::visitAfterRefSelect(llvm::SelectInst &I,
                                              CrabSelectRefOps &s) {}
void EmitNullDerefChecks::visitBeforeIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) {}
void EmitNullDerefChecks::visitAfterIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) {}

/* End empty implementations */

} // end namespace clam
