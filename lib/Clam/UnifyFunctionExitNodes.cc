#include "clam/Support/UnifyFunctionExitNodes.hh"

#include "llvm/IR/PassManager.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

using namespace llvm;

namespace clam {

char UnifyFunctionExitNodesLegacyPass::ID = 0;

bool UnifyFunctionExitNodesLegacyPass::runOnFunction(Function &F) {
  // UnifyFunctionExitNodesPass::run() takes a FunctionAnalysisManager only to
  // satisfy the new-PM signature: it queries no analysis, so an empty manager
  // is enough (LLVM 18 lib/Transforms/Utils/UnifyFunctionExitNodes.cpp).
  FunctionAnalysisManager FAM;
  return !UnifyFunctionExitNodesPass().run(F, FAM).areAllPreserved();
}

void UnifyFunctionExitNodesLegacyPass::getAnalysisUsage(
    AnalysisUsage &AU) const {
  // Same as LLVM's own wrapper before it was removed: unifying exit nodes
  // preserves the non-critical-edgeness property, and this is a cluster of
  // orthogonal transforms.
  AU.addPreservedID(BreakCriticalEdgesID);
  AU.addPreservedID(LowerSwitchID);
}

} // namespace clam

// The legacy manager resolves an addRequired<> through the pass registry, so
// the pass must be registered even though nothing selects it by name.
static llvm::RegisterPass<clam::UnifyFunctionExitNodesLegacyPass>
    X("clam-unify-function-exit-nodes",
      "Ensure each function has at most one return instruction");
