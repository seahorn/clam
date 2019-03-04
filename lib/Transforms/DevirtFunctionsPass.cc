/** 
 * LLVM transformation passes to resolve indirect calls 
 **/

#include "crab_llvm/config.h"
#include "crab_llvm/Transforms/DevirtFunctions.hh"
#include "llvm/Pass.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

#ifdef HAVE_DSA
#include "dsa/CallTargets.h"
#endif 

cl::opt<crab_llvm::CallSiteResolverKind>
DevirtResolver("devirt-resolver",
      cl::desc ("Method used to select potential callees"),
      cl::values 
      (clEnumValN(crab_llvm::RESOLVER_TYPES, "types", "Callees with same type"),
       clEnumValN(crab_llvm::RESOLVER_DSA  , "dsa"  , "DSA selects the potential callees")),
      cl::init(crab_llvm::RESOLVER_TYPES));
		   
static llvm::cl::opt<bool>
AllowIndirectCalls("devirt-allow-indirect-calls",
		   llvm::cl::desc("Allow creation of indirect calls "
				  "during devirtualization "
				  "(required for soundness)"),
		   llvm::cl::init(false));

// Options for Dsa
static llvm::cl::opt<bool>
ResolveIncompleteCalls("devirt-resolve-incomplete-calls",
		       llvm::cl::desc("Resolve indirect calls that might still require "
				      "reasoning about other modules"
				      "(required for soundness)"),
		       llvm::cl::init(true));

static llvm::cl::opt<unsigned>
MaxNumTargets("devirt-max-num-targets",
	      llvm::cl::desc("Do not resolve if number of targets is greater than this number."),
	      llvm::cl::init(9999));

using namespace llvm;

namespace crab_llvm {

  class DevirtualizeFunctionsPass:  public ModulePass {
   public:
    
    static char ID;
    
    DevirtualizeFunctionsPass(): ModulePass(ID) {}
      
    virtual bool runOnModule(Module & M) {
      // -- Get the call graph: unused for now
      // CallGraph* CG = &(getAnalysis<CallGraphWrapperPass> ().getCallGraph ());
      
      DevirtualizeFunctions DF(/*CG*/ nullptr, AllowIndirectCalls);
      std::unique_ptr<CallSiteResolver> CSR;
      switch(DevirtResolver) {
#ifdef HAVE_DSA
      case RESOLVER_DSA: {
	// -- Access to analysis pass which finds targets of indirect function calls
	using LlvmDsaResolver = dsa::CallTargetFinder<EQTDDataStructures>;
	LlvmDsaResolver* CTF = &getAnalysis<LlvmDsaResolver>();
	CSR.reset(new CallSiteResolverByDsa<LlvmDsaResolver>(M, *CTF,
							     ResolveIncompleteCalls,
							     MaxNumTargets));
      }
	break;
#endif 	
      case RESOLVER_TYPES:
      default:
	if (DevirtResolver == RESOLVER_DSA) {
	  errs() << "WARNING: Dsa not available, using only types to resolve indirect calls\n";
	}
	CSR.reset(new CallSiteResolverByTypes(M));
	break;
      }
      
      bool res = DF.resolveCallSites(M, &*CSR);
      return res;
    }      
    
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
#ifdef HAVE_DSA      
      if (DevirtResolver == RESOLVER_DSA) {
	AU.addRequired<dsa::CallTargetFinder<EQTDDataStructures>>();
      }
#endif       
      // AU.addRequired<CallGraphWrapperPass> ();
      // FIXME: DevirtualizeFunctions does not fully update the call
      // graph so we don't claim it is preserved.
      // AU.setPreservesAll ();
      // AU.addPreserved<CallGraphWrapperPass> ();
    }
    
    StringRef getPassName() const {
      return "DevirtualizeFunctionsPass";
    }
  };

  char DevirtualizeFunctionsPass::ID = 0;

  Pass* createDevirtualizeFunctionsPass() {
    return new DevirtualizeFunctionsPass();
  }
 
} // end namespace


