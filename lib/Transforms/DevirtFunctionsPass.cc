//===-------- Resolve indirect calls using signature match ----------------===//
//
// This class is almost the same than Devirt in DSA but it does not
// use alias analysis to compute the possible targets of an indirect
// call. Instead, it simply selects those functions whose signatures
// match.
//
//===----------------------------------------------------------------------===//


#include "crab_llvm/Transforms/DevirtFunctions.hh"

#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace crab_llvm {

  class DevirtualizeFunctionsPass:  public ModulePass {
   public:
    
    static char ID;
    bool m_allow_indirect_calls;
    
    DevirtualizeFunctionsPass(bool allow_indirect_calls = false)
      : ModulePass(ID)
      , m_allow_indirect_calls(allow_indirect_calls)
    {}
    
    virtual bool runOnModule(Module & M);
    
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll ();
      AU.addRequired<CallGraphWrapperPass> ();
      AU.addPreserved<CallGraphWrapperPass> ();
    }
  };



  bool DevirtualizeFunctionsPass::runOnModule (Module & M) {
    // -- Get the call graph
    CallGraph* CG = &(getAnalysis<CallGraphWrapperPass> ().getCallGraph ());
    DevirtualizeFunctions::AliasSetMap aliasSets;
    
    // -- Create alias sets
    for (auto const &F: M) {
      // -- intrinsics are never called indirectly
      if (F.isIntrinsic ()) continue;
      
      // -- local functions whose address is not taken cannot be
      // -- resolved by a function pointer
      if (F.hasLocalLinkage () && !F.hasAddressTaken ()) continue;

      // -- skip calls to declarations, these are resolved implicitly
      // -- by calling through the function pointer argument in the
      // -- default case of bounce function
      if (F.isDeclaration ()) continue;
      
      // -- skip seahorn and verifier specific intrinsics
      if (F.getName().startswith ("seahorn.")) continue;
      if (F.getName().startswith ("verifier.")) continue;
      // -- assume entry point is never called indirectly
      if (F.getName ().equals ("main")) continue;

      // -- add F to its corresponding alias set
      aliasSets[DevirtualizeFunctions::typeAliasId(F)].push_back(&F);
    }

    DevirtualizeFunctions DF(aliasSets, CG, m_allow_indirect_calls);
    
    return DF.run(M);
  }
  

  // Pass ID variable
  char DevirtualizeFunctionsPass::ID = 0;

  Pass* createDevirtualizeFunctionsPass(bool allow_indirect_calls) {
    return new DevirtualizeFunctionsPass(allow_indirect_calls);
  }
  
  // Pass registration
  RegisterPass<DevirtualizeFunctionsPass>
  XX ("devirt-functions", "Devirtualize indirect function calls using only types");

} // end namespace
