#include "clam/CrabIREmitter.hh"

#include "../CfgBuilderUtils.hh"
#include "MemoryCheckUtils.hh"
#include "UafCheck.hh"

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
       b := is_unfreed_or_null(reg, ref);
       bool_assert(b);
       store_to_ref(reg, ref, val);
*/

class EmitUafChecksImpl {
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
      if (isa<GlobalValue>(BasePtr) || isa<AllocaInst>(BasePtr)) {
        // errs() << "Skipped " << *BasePtr << " because it cannot be a dangling
        // pointer!\n";
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
    // easiest way is to extend CrabStoreRefOps with information about
    // the HeapAbstraction Region such as whether isSequence(),
    // IsCyclic(), etc.

    var_t b = m_lfac.mkBoolVar();
    std::vector<var_t> outputs{b};
    std::vector<var_or_cst_t> inputs{reg, ref};
    bb.intrinsic("is_unfreed_or_null", outputs, inputs);
    bb.bool_assert(b, getDebugLoc(&I, m_assertionId++));
    m_addedChecks++;
  }

public:
  EmitUafChecksImpl(const CrabBuilderParams &params, crabLitFactory &lfac,
                    uint32_t &assertionId)
      : m_params(params), m_lfac(lfac), m_assertionId(assertionId),
        m_addedChecks(0), m_trivialChecks(0) {}

  ~EmitUafChecksImpl() {
    llvm::errs() << "-- Inserted " << m_addedChecks;
    llvm::errs() << " use-after-free checks ";
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

EmitUafChecks::EmitUafChecks(const CrabBuilderParams &params,
                             crabLitFactory &lfac, uint32_t &assertionId)
    : m_impl(new EmitUafChecksImpl(params, lfac, assertionId)) {}

EmitUafChecks::~EmitUafChecks() {}

void EmitUafChecks::visitBeforeBasicBlock(llvm::BasicBlock &BB) {
  m_impl->visitBeforeBasicBlock(BB);
}

void EmitUafChecks::visitBeforeStore(llvm::StoreInst &I, CrabStoreRefOps &s) {
  m_impl->visitBeforeStore(I, s);
}

void EmitUafChecks::visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {
  m_impl->visitBeforeLoad(I, s);
}

void EmitUafChecks::visitBeforeMemset(llvm::MemSetInst &I, CrabMemsetOps &s) {
  m_impl->visitBeforeMemset(I, s);
}

void EmitUafChecks::visitBeforeMemTransfer(llvm::MemTransferInst &I,
                                           CrabMemTransferOps &s) {
  m_impl->visitBeforeMemTransfer(I, s);
}

/* Begin empty implementations */
void EmitUafChecks::visitAfterBasicBlock(llvm::BasicBlock &BB) {}
void EmitUafChecks::visitBeforeAlloc(llvm::Instruction &I,
                                     const llvm::TargetLibraryInfo &tli,
                                     CrabMakeRefOps &s) {}
void EmitUafChecks::visitAfterAlloc(llvm::Instruction &I,
                                    const llvm::TargetLibraryInfo &tli,
                                    CrabMakeRefOps &s) {}
void EmitUafChecks::visitBeforeFree(llvm::Instruction &I,
                                    const llvm::TargetLibraryInfo &tli,
                                    CrabRemoveRefOps &s) {}
void EmitUafChecks::visitAfterFree(llvm::Instruction &I,
                                   const llvm::TargetLibraryInfo &tli,
                                   CrabRemoveRefOps &s) {}
void EmitUafChecks::visitBeforeGep(llvm::Instruction &I, CrabGepRefOps &s) {}
void EmitUafChecks::visitAfterGep(llvm::Instruction &I, CrabGepRefOps &s) {}
void EmitUafChecks::visitAfterStore(llvm::StoreInst &I, CrabStoreRefOps &s) {}
void EmitUafChecks::visitAfterLoad(llvm::LoadInst &I, CrabLoadRefOps &s) {}
void EmitUafChecks::visitAfterMemset(llvm::MemSetInst &I, CrabMemsetOps &s) {}
void EmitUafChecks::visitAfterMemTransfer(llvm::MemTransferInst &I,
                                          CrabMemTransferOps &s) {}
void EmitUafChecks::visitBeforeRefSelect(llvm::SelectInst &I,
                                         CrabSelectRefOps &s) {}
void EmitUafChecks::visitAfterRefSelect(llvm::SelectInst &I,
                                        CrabSelectRefOps &s) {}
void EmitUafChecks::visitBeforeIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) {}
void EmitUafChecks::visitAfterIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) {}

/* End empty implementations */

} // end namespace clam
