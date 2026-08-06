/* Loop peeler borrowed from SeaHorn */

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/LoopPeel.h"
#include "llvm/Transforms/Utils/LoopUtils.h"

#include "clam/NewPmPasses.hh"

namespace clam {

using namespace llvm;
class LoopPeelerLegacyPass : public LoopPass {
public:
  static char ID;
  // -- number of iterations to peel
  unsigned m_Num;
  LoopPeelerLegacyPass(unsigned Num = 0) : LoopPass(ID) { m_Num = Num; }

  bool runOnLoop(Loop *L, LPPassManager &LPM) override;

  StringRef getPassName() const override { return "LoopPeeler"; }
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<AssumptionCacheTracker>();
    getLoopAnalysisUsage(AU);
  }
};

/** Peel Num iterations off L. Shared by the legacy and new-PM passes, which
    differ only in where they source the analyses from. */
static bool peelLoopBy(Loop *L, unsigned Num, LoopInfo &LI,
                       ScalarEvolution &SE, DominatorTree &DT,
                       AssumptionCache &AC) {
  if (Num == 0)
    return false;

  if (!L->getHeader())
    return false;

  if (!canPeel(L)) {
    return false;
  }
  // LLVM 16 requires the value map out-parameter (original instructions to the
  // last peeled-off iteration). We have no use for it.
  ValueToValueMapTy VMap;
  return peelLoop(L, Num, &LI, &SE, DT, &AC, true /* PreserveLCSSA */, VMap);
}

/**
   llvm::peeLoop() requires loops to be rotated. Here is an explanation from
   https://blog.regehr.org/archives/1603

   BEFORE Loop rotation              AFTER Loop rotation
   --------------------              --------------------

      initializer                        initializer
      goto COND                          if (condition)
    COND:                                  goto BODY
      if (condition)                     else
        goto BODY                          goto EXIT
      else                             BODY:
        goto EXIT                        body
    BODY:                                modifier
      body                               if (condition)
      modifier                             goto BODY
      goto COND                          else
    EXIT:                                  goto EXIT
                                       EXIT:

  After loop is rotated there are:
     - a pre-header in which loop condition is tested once
     - a header in which loop begins unconditionally (called BODY)
     - a unique latch with one exit branch and one backedge

  A rotated loop is easy to cut -- simply add assume(false) before the
  back-edge.

  Peeling is easy as well. Here is a comment from llvm::LoopUnrollPeel.cpp

  Peeling the first iteration transforms.

  BEFORE Peeling               AFTER Peeling
  --------------               --------------
  PreHeader:                   InsertTop:
  ...                            LoopBody
  Header:                        If (!cond) goto Exit
    LoopBody                   InsertBot:
    If (cond) goto Header      NewPreHeader:
  Exit:                        ...
                               Header:
                                LoopBody
                                If (cond) goto Header
                               Exit:
 */

bool LoopPeelerLegacyPass::runOnLoop(Loop *L, LPPassManager &LPM) {
  auto *Header = L->getHeader();
  if (!Header)
    return false;
  Function *F = Header->getParent();

  auto &SE = getAnalysis<ScalarEvolutionWrapperPass>().getSE();
  auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  auto &AC = getAnalysis<AssumptionCacheTracker>().getAssumptionCache(*F);

  return peelLoopBy(L, m_Num, LI, SE, DT, AC);
}

char LoopPeelerLegacyPass::ID = 0;
llvm::Pass *createLoopPeelerPass(unsigned Num) {
  return new LoopPeelerLegacyPass(Num);
}

PreservedAnalyses LoopPeelerPass::run(Loop &L, LoopAnalysisManager &,
                                      LoopStandardAnalysisResults &AR,
                                      LPMUpdater &) {
  if (!peelLoopBy(&L, m_Num, AR.LI, AR.SE, AR.DT, AR.AC)) {
    return PreservedAnalyses::all();
  }
  return getLoopPassPreservedAnalyses();
}

} // end namespace clam
