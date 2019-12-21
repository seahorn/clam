#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/Local.h"

#include "clam/config.h"

#include "sea_dsa/ShadowMem.hh"
#include "sea_dsa/DsaAnalysis.hh"

#ifdef HAVE_DSA
#include "dsa/DataStructure.h"
#include "dsa/AllocatorIdentification.h"
#include "dsa/AddressTakenAnalysis.h"
#include "dsa/Steensgaard.hh"
#endif

using namespace llvm;

namespace clam
{
  struct RemoveUnreachableBlocksPass : public FunctionPass
  {
    static char ID;
    RemoveUnreachableBlocksPass () : FunctionPass (ID) {}
    
    bool runOnFunction (Function &F)
    {return removeUnreachableBlocks (F);}
    
    void getAnalysisUsage (AnalysisUsage &AU) const {
      #ifdef HAVE_DSA
      // Preserve DSA passes
      AU.addPreservedID(StdLibDataStructuresID);
      AU.addPreservedID(AddressTakenAnalysisID);
      AU.addPreservedID(AllocIdentifyID);
      AU.addPreservedID(LocalDataStructuresID);
      AU.addPreservedID(SteensgaardDataStructuresID);
      #endif
      // Preserve Sea-DSA passes
      AU.addPreservedID(sea_dsa::DsaAnalysis::ID);
      AU.addPreservedID(sea_dsa::ShadowMemPass::ID);
    }
    
    virtual StringRef getPassName() const {
      return "Clam: Remove unreachable blocks";
    }

  };

  char RemoveUnreachableBlocksPass::ID = 0;
  Pass* createRemoveUnreachableBlocksPass () 
  { return new RemoveUnreachableBlocksPass (); }
} 
