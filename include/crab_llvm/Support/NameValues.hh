#ifndef _NAME_VALUES_HPP_
#define _NAME_VALUES_HPP_

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"

namespace crab_llvm {

  class NameValues : public llvm::ModulePass {
   public:
    
    static char ID;

    NameValues () : llvm::ModulePass (ID) {}
    
    virtual bool runOnModule (llvm::Module &M);

    bool runOnFunction (llvm::Function &F);

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const { 
      AU.setPreservesAll (); 
    }

    virtual llvm::StringRef getPassName() const {
      return "Name values";
    }
  };

} 
#endif 
