#include "llvm/IR/Module.h"
#include "llvm/Pass.h"

using namespace llvm;

namespace clam {

/// marks all internal functions with AlwaysInline attribute
struct MarkInternalInline : public ModulePass {
  static char ID;
  MarkInternalInline() : ModulePass(ID) {}

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
  }

  virtual bool runOnModule(Module &M) override {
    for (Function &F : M)
      if (!F.isDeclaration() && F.hasLocalLinkage()) {
        F.setLinkage(GlobalValue::PrivateLinkage);
        F.removeFnAttr(Attribute::NoInline);
        F.removeFnAttr(Attribute::OptimizeNone);
        F.addFnAttr(Attribute::AlwaysInline);
      }
    return true;
  }

  virtual StringRef getPassName() const override {
    return "Clam: Mark internal functions with AlwaysInline attribute";
  }
};

char MarkInternalInline::ID = 0;
Pass *createMarkInternalInlinePass() { return new MarkInternalInline(); }
} // namespace clam
