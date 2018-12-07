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
    AU.setPreservesAll();
  }
  
  const char* getPassName() const {
    return "DevirtualizeFunctionsDsaPass (LLVM DSA + types)";
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

class DsaResolver: public CallSiteResolver {
  dsa::CallTargetFinder<EQTDDataStructures>* m_CTF;
  TargetMap m_TM;
  
public:
  
  DsaResolver(dsa::CallTargetFinder<EQTDDataStructures>* CTF, Module& M)
    : CallSiteResolver()
    , m_CTF(CTF) {
    
    // build the target map
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
	    if (m_CTF->isComplete(CS)) {
	      m_TM[CI].append(CTF->begin(CS), CTF->end(CS));
	    }
	  }
	}
      }
    }
  }
  
  bool useAliasing() const {
    return true;
  }
  
  bool hasTargets(const llvm::Instruction* CS) const {
    return (m_TM.find(CS) != m_TM.end());
  }
  
  AliasSet& getTargets(const llvm::Instruction* CS) {
    assert(hasTargets(CS));
    auto it = m_TM.find(CS);
    return it->second;
  }
  
  const AliasSet& getTargets(const llvm::Instruction* CS) const {
    assert(hasTargets(CS));
    auto it = m_TM.find(CS);
    return it->second;
  }
};
    
class DevirtualizeFunctionsDsaPass:  public ModulePass {
public:

  static char ID;
  bool m_allowIndirectCalls;
  
  DevirtualizeFunctionsDsaPass(bool allowIndirectCalls = false)
    : ModulePass(ID)
    , m_allowIndirectCalls(allowIndirectCalls) {}
  
  virtual bool runOnModule(Module & M) {
    // -- Get the call graph
    CallGraph* CG = &(getAnalysis<CallGraphWrapperPass> ().getCallGraph ());
    
    // -- Access to analysis pass which finds targets of indirect function calls
    dsa::CallTargetFinder<EQTDDataStructures> *CTF =
      &getAnalysis<dsa::CallTargetFinder<EQTDDataStructures>>();
    
    DevirtualizeFunctions DF(CG, m_allowIndirectCalls);
    DsaResolver DsaCSR(CTF, M);
    CallSiteResolver* CSR = &DsaCSR;
    bool res = DF.resolveCallSites(M, CSR);
    return res;
  }    
  
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<CallGraphWrapperPass>();
    AU.addRequired<dsa::CallTargetFinder<EQTDDataStructures>>();
    // FIXME: DevirtualizeFunctions does not fully update the call
    // graph so we don't claim it is preserved.
    // AU.setPreservesAll();
    // AU.addPreserved<CallGraphWrapperPass>();
  }

  const char* getPassName() const {
    return "DevirtualizeFunctionsDsaPass (LLVM DSA + types)";
  }
  
};
} // end namespace
#endif 


namespace crab_llvm {
  
char DevirtualizeFunctionsDsaPass::ID = 0;

llvm::Pass* createDevirtualizeFunctionsDsaPass(bool allowIndirectCalls) {
  return new DevirtualizeFunctionsDsaPass(allowIndirectCalls);
}
  
} // end namespace


