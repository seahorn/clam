#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/Local.h"

using namespace llvm;

namespace crab_llvm
{
  struct RemoveUnreachableBlocksPass : public FunctionPass
  {
    static char ID;
    RemoveUnreachableBlocksPass () : FunctionPass (ID) {}
    
    bool runOnFunction (Function &F)
    {return removeUnreachableBlocks (F);}
    
    void getAnalysisUsage (AnalysisUsage &AU) const {
      //  AU.setPreservesAll ();
    }
    
    virtual const char * getPassName() const {
      return "Remove unreachable blocks";
    }

  };

  char RemoveUnreachableBlocksPass::ID = 0;
  Pass* createRemoveUnreachableBlocksPass () 
  { return new RemoveUnreachableBlocksPass (); }
} 
