#include "llvm/IR/Function.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/Local.h"

#include "clam/config.h"

#include "seadsa/DsaAnalysis.hh"
#include "seadsa/ShadowMem.hh"

#include "clam/NewPmPasses.hh"

using namespace llvm;

namespace clam {
struct RemoveUnreachableBlocks : public FunctionPass {
  static char ID;
  RemoveUnreachableBlocks() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function &F) override {
    return removeUnreachableBlocks(F);
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    // Preserve Sea-DSA passes
    AU.addPreservedID(seadsa::DsaAnalysis::ID);
    AU.addPreservedID(seadsa::ShadowMemPass::ID);
  }

  virtual StringRef getPassName() const override {
    return "Clam: Remove unreachable blocks";
  }
};

char RemoveUnreachableBlocks::ID = 0;
Pass *createRemoveUnreachableBlocksPass() {
  return new RemoveUnreachableBlocks();
}

PreservedAnalyses RemoveUnreachableBlocksPass::run(Function &F,
                                                   FunctionAnalysisManager &) {
  if (!removeUnreachableBlocks(F)) {
    return PreservedAnalyses::all();
  }
  return PreservedAnalyses::none();
}
} // namespace clam
