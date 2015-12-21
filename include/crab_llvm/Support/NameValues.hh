#ifndef _NAME_VALUES_HPP_
#define _NAME_VALUES_HPP_

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"

using namespace llvm;

namespace crab_llvm {

  class NameValues : public ModulePass {
   public:
    
    static char ID;

    NameValues () : ModulePass (ID) {}
    
    bool runOnModule (Module &M);

    bool runOnFunction (Function &F);

    void getAnalysisUsage (AnalysisUsage &AU) const { 
      AU.setPreservesAll (); 
    }

    virtual const char * getPassName() const {
      return "Name values";
    }
  };

} 
#endif 
