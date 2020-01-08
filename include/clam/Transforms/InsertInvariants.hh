#pragma once

/* 
 * Instrument LLVM bitecode by inserting invariants computed by
 * crab. The invariants are inserted as verifier.assume instructions.
 */

#include "llvm/Pass.h"

namespace llvm {
  class Function;
  class Module;
  class CallGraph;
}

namespace clam {

  class InsertInvariants : public llvm::ModulePass {

    llvm::Function* m_assumeFn;

  public:
    
    static char ID;        
    
    InsertInvariants (): llvm::ModulePass (ID), m_assumeFn (0) {} 

    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual llvm::StringRef getPassName () const {
      return "Clam: insert invariants as verifier.assume instructions";
    }

  };

} // end namespace clam
