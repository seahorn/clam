/* Loop peeler borrowed from SeaHorn */

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/LoopPeel.h"
#include "llvm/Transforms/Utils/LoopUtils.h"

namespace clam {

using namespace llvm;
class LoopPeelerPass : public LoopPass {
public:
  static char ID;
  // -- number of iterations to peel
  unsigned m_Num;
  LoopPeelerPass(unsigned Num = 0) : LoopPass(ID) { m_Num = Num; }

  bool runOnLoop(Loop *L, LPPassManager &LPM) override;

  StringRef getPassName() const override { return "LoopPeeler"; }
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<AssumptionCacheTracker>();
    getLoopAnalysisUsage(AU);
  }
};

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

bool LoopPeelerPass::runOnLoop(Loop *L, LPPassManager &LPM) {
  if (m_Num == 0)
    return false;

  auto *Header = L->getHeader();
  if (!Header)
    return false;
  Function *F = Header->getParent();

  auto &SE = getAnalysis<ScalarEvolutionWrapperPass>().getSE();
  auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  auto &AC = getAnalysis<AssumptionCacheTracker>().getAssumptionCache(*F);

  if (!canPeel(L)) {
    return false;
  }
  auto res = peelLoop(L, m_Num, &LI, &SE, DT, &AC, true /* PreserveLCSSA */);
  return res;
}

char LoopPeelerPass::ID = 0;
llvm::Pass *createLoopPeelerPass(unsigned Num) {
  return new LoopPeelerPass(Num);
}

} // end namespace clam
