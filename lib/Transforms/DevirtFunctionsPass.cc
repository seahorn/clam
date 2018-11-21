//===-------- Resolve indirect calls using type signatures ----------------===//
//
// Resolve the targets of an indirect call by selecting all functions
// whose signatures match.
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
    bool m_allowIndirectCalls;
    
    DevirtualizeFunctionsPass(bool allowIndirectCalls = false)
      : ModulePass(ID)
      , m_allowIndirectCalls(allowIndirectCalls)
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

    /* empty because we are not using pointer analysis here*/    
    DevirtualizeFunctions::AliasTargetMap ATM; 
    DevirtualizeFunctions DF(CG, m_allowIndirectCalls);
    return DF.resolveCallSites(M, ATM);
  }
  

  // Pass ID variable
  char DevirtualizeFunctionsPass::ID = 0;

  Pass* createDevirtualizeFunctionsPass(bool allowIndirectCalls) {
    return new DevirtualizeFunctionsPass(allowIndirectCalls);
  }
  
  // Pass registration
  RegisterPass<DevirtualizeFunctionsPass>
  XX ("devirt-functions", "Devirtualize indirect function calls using only types");

} // end namespace
