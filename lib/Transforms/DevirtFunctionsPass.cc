/**
 * LLVM transformation passes to resolve indirect calls
 **/

#include "clam/Transforms/DevirtFunctions.hh"
#include "clam/config.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

#include "seadsa/CompleteCallGraph.hh"
#include "seadsa/support/RemovePtrToInt.hh"

llvm::cl::opt<clam::CallSiteResolverKind> DevirtResolver(
    "devirt-resolver",
    llvm::cl::desc("Method used to select potential callees"),
    llvm::cl::values(clEnumValN(clam::RESOLVER_TYPES, "types",
                                "Callees with same type"),
                     clEnumValN(clam::RESOLVER_SEA_DSA, "sea-dsa",
                                "Sea-Dsa selects the potential callees")),
    llvm::cl::init(clam::RESOLVER_TYPES));

static llvm::cl::opt<bool>
    AllowIndirectCalls("devirt-allow-indirect-calls",
                       llvm::cl::desc("Allow creation of indirect calls "
                                      "during devirtualization "
                                      "(required for soundness)"),
                       llvm::cl::init(false));

// Options for Dsa's analyses
static llvm::cl::opt<bool> ResolveIncompleteCalls(
    "devirt-resolve-incomplete-calls",
    llvm::cl::desc("Resolve indirect calls that might still require "
                   "reasoning about other modules"
                   "(required for soundness)"),
    llvm::cl::init(true));

static llvm::cl::opt<unsigned> MaxNumTargets(
    "devirt-max-num-targets",
    llvm::cl::desc(
        "Do not resolve if number of targets is greater than this number."),
    llvm::cl::init(9999));

using namespace llvm;

namespace clam {

class DevirtualizeFunctionsPass : public ModulePass {
public:
  static char ID;

  DevirtualizeFunctionsPass() : ModulePass(ID) {}

  virtual bool runOnModule(Module &M) {
    // -- Get the call graph: unused for now
    // CallGraph* CG = &(getAnalysis<CallGraphWrapperPass> ().getCallGraph ());

    DevirtualizeFunctions DF(/*CG*/ nullptr, AllowIndirectCalls);
    std::unique_ptr<CallSiteResolver> CSR;
    switch (DevirtResolver) {
    case RESOLVER_SEA_DSA: {
      auto &CCG = getAnalysis<seadsa::CompleteCallGraph>();
      CSR.reset(new CallSiteResolverByDsa<seadsa::CompleteCallGraph>(
          M, CCG, ResolveIncompleteCalls, MaxNumTargets, DF.getStats()));
      break;
    }
    case RESOLVER_TYPES:
    default:
      if (DevirtResolver == RESOLVER_DSA) {
        errs() << "WARNING: Dsa not available, using only types to resolve "
                  "indirect calls\n";
      }
      CSR.reset(new CallSiteResolverByTypes(M, DF.getStats()));
      break;
    }

    bool res = DF.resolveCallSites(M, &*CSR);
    return res;
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    if (DevirtResolver == RESOLVER_SEA_DSA) {
      AU.addRequired<seadsa::RemovePtrToInt>();      
      AU.addRequired<seadsa::CompleteCallGraph>();
    }

    // AU.addRequired<CallGraphWrapperPass> ();
    // FIXME: DevirtualizeFunctions does not fully update the call
    // graph so we don't claim it is preserved.
    // AU.setPreservesAll ();
    // AU.addPreserved<CallGraphWrapperPass> ();
  }

  StringRef getPassName() const { return "Clam: Devirtualize indirect calls"; }
};

char DevirtualizeFunctionsPass::ID = 0;

Pass *createDevirtualizeFunctionsPass() {
  return new DevirtualizeFunctionsPass();
}

} // namespace clam
