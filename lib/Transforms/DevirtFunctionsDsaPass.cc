//===--- Resolve indirect calls using DSA pointer analysis ---------------===//
//
// Resolve the targets of an indirect call by selecting all functions
// provided by DSA.
//
//===----------------------------------------------------------------------===//

#include "crab_llvm/config.h"

#ifndef HAVE_DSA
#include "llvm/Pass.h"
#include "llvm/IR/Module.h"

namespace crab_llvm {
class DevirtualizeFunctionsDsaPass:  public llvm::ModulePass {
public:
  static char ID;
  DevirtualizeFunctionsDsaPass(bool /*allowIndirectCalls*/ = false)
    : llvm::ModulePass(ID) {}
  
  virtual bool runOnModule(llvm::Module & M) {
    return false;
  }
  
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const {
  }
};
}

#else

/** Real code starts here **/
#include "crab_llvm/Transforms/DevirtFunctions.hh"
#include "dsa/CallTargets.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace crab_llvm {

class DevirtualizeFunctionsDsaPass:  public ModulePass {
public:

  static char ID;
  bool m_allowIndirectCalls;
  
  DevirtualizeFunctionsDsaPass(bool allowIndirectCalls = false)
    : ModulePass(ID)
    , m_allowIndirectCalls(allowIndirectCalls) {}
  
  virtual bool runOnModule(Module & M);
  
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<CallGraphWrapperPass>();
    AU.addPreserved<CallGraphWrapperPass>();
    AU.addRequired<dsa::CallTargetFinder<EQTDDataStructures>>();    
  }
};
  
  
  
bool DevirtualizeFunctionsDsaPass::runOnModule (Module & M) {
  // -- Get the call graph
  CallGraph* CG = &(getAnalysis<CallGraphWrapperPass> ().getCallGraph ());
  
  // -- Access to analysis pass which finds targets of indirect function calls
  dsa::CallTargetFinder<EQTDDataStructures> *CTF =
    &getAnalysis<dsa::CallTargetFinder<EQTDDataStructures>>();

  DevirtualizeFunctions::AliasTargetMap aliasMap;
  
  // -- Create dsa alias map
  for (auto &F: M) {
    for (auto &BB: F) {
      for (auto &I: BB) {
        Instruction *CI= nullptr;
	if (isa<CallInst>(&I)) {
	  CI = &I;
	}
	if (!CI && isa<InvokeInst>(&I)) {
	  CI = &I;
	}
	
	if (CI) {
	  CallSite CS(CI);
	  if (CTF->isComplete(CS)) {
	    aliasMap[CI].append(CTF->begin(CS), CTF->end(CS));
	  }
	}
      }
    }
  }
  
  DevirtualizeFunctions DF(CG, m_allowIndirectCalls);
  return DF.resolveCallSites(M, aliasMap);
}
  
} // end namespace
#endif 


namespace crab_llvm {
  
// Pass ID variable
char DevirtualizeFunctionsDsaPass::ID = 0;

llvm::Pass* createDevirtualizeFunctionsDsaPass(bool allowIndirectCalls) {
  return new DevirtualizeFunctionsDsaPass(allowIndirectCalls);
}
  
// Pass registration
llvm::RegisterPass<DevirtualizeFunctionsDsaPass>
X("devirt-functions-dsa",
  "Devirtualize indirect function calls using DSA pointer analysis");

} // end namespace
