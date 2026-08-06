#include "llvm/IR/Module.h"
#include "llvm/Pass.h"

#include "clam/NewPmPasses.hh"

using namespace llvm;

namespace {
/// marks all internal functions with AlwaysInline attribute
bool markInternalInline(Module &M) {
  for (Function &F : M)
    if (!F.isDeclaration() && F.hasLocalLinkage()) {
      F.setLinkage(GlobalValue::PrivateLinkage);
      F.removeFnAttr(Attribute::NoInline);
      F.removeFnAttr(Attribute::OptimizeNone);
      F.addFnAttr(Attribute::AlwaysInline);
    }
  return true;
}
} // namespace

namespace clam {

struct MarkInternalInline : public ModulePass {
  static char ID;
  MarkInternalInline() : ModulePass(ID) {}

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
  }

  virtual bool runOnModule(Module &M) override { return markInternalInline(M); }

  virtual StringRef getPassName() const override {
    return "Clam: Mark internal functions with AlwaysInline attribute";
  }
};

char MarkInternalInline::ID = 0;
Pass *createMarkInternalInlinePass() { return new MarkInternalInline(); }

PreservedAnalyses MarkInternalInlinePass::run(Module &M,
                                              ModuleAnalysisManager &) {
  markInternalInline(M);
  // Only linkage and function attributes change, no IR.
  return PreservedAnalyses::all();
}
} // namespace clam
