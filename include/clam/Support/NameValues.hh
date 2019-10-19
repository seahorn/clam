#pragma once

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"

namespace clam {

  class NameValues : public llvm::ModulePass {
   public:
    
    static char ID;

    NameValues () : llvm::ModulePass (ID) {}
    
    virtual bool runOnModule (llvm::Module &M);

    bool runOnFunction (llvm::Function &F);

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const;
   
    virtual llvm::StringRef getPassName() const {
      return "Clam: Name values";
    }
  };
} 
