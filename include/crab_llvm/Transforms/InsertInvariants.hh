#pragma once

/* 
 * Instrument LLVM bitecode by inserting invariants computed by
 * crab. The invariants are inserted as verifier.assume instructions.
 */

#include "crab_llvm/crab_cfg.hh"
#include "llvm/Pass.h"

namespace llvm {
  class Function;
  class Module;
  class CallGraph;
}

namespace crab_llvm {

  class InsertInvariants : public llvm::ModulePass {

    llvm::Function* m_assumeFn;

  public:
    
    static char ID;        
    
    InsertInvariants (): llvm::ModulePass (ID), m_assumeFn (0) {} 

    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual llvm::StringRef getPassName () const {
      return "CrabLlvm: insert invariants as verifier.assume instructions";
    }

  };

} // end namespace crab_llvm
