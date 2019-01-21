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
    
    virtual bool runOnModule(Module & M) {
      // -- Get the call graph
      CallGraph* CG = &(getAnalysis<CallGraphWrapperPass> ().getCallGraph ());
      
      DevirtualizeFunctions DF(CG, m_allowIndirectCalls);
      NoAliasResolver NoAliasCSR;    
      CallSiteResolver* CSR = &NoAliasCSR;    
      bool res = DF.resolveCallSites(M, CSR);
      return res;
    }      
    
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<CallGraphWrapperPass> ();
      // FIXME: DevirtualizeFunctions does not fully update the call
      // graph so we don't claim it is preserved.
      // AU.setPreservesAll ();
      // AU.addPreserved<CallGraphWrapperPass> ();
    }
    
    const char* getPassName() const {
      return "DevirtualizeFunctionsPass (only types)";
    }
  };

  char DevirtualizeFunctionsPass::ID = 0;

  Pass* createDevirtualizeFunctionsPass(bool allowIndirectCalls) {
    return new DevirtualizeFunctionsPass(allowIndirectCalls);
  }
  
} // end namespace


