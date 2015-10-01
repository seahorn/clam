#ifndef SIMPLIFY_ASSUME_HPP
#define SIMPLIFY_ASSUME_HPP

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"

using namespace llvm;

namespace crab_llvm
{
  struct SimplifyAssume : public ModulePass
  {
    static char ID;
    Function* m_assumeFn;

    SimplifyAssume () : ModulePass (ID), m_assumeFn (0) {}
        
    virtual bool runOnModule (Module &M);
    virtual bool runOnFunction (Function &F);

    // It might not preserve the call graph ...
    void getAnalysisUsage (AnalysisUsage &AU) const
    {AU.setPreservesAll ();}
    
  };
    
}
#endif 
