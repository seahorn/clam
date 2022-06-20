#pragma once

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"

namespace clam {

class NameValues : public llvm::ModulePass {
public:
  static char ID;

  NameValues() : llvm::ModulePass(ID) {}

  virtual bool runOnModule(llvm::Module &M) override;

  bool runOnFunction(llvm::Function &F);

  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

  virtual llvm::StringRef getPassName() const override { return "Clam: Name values"; }
};
} // namespace clam
