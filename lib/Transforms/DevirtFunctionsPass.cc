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
#include "seadsa/SeaDsaAnalysis.hh"
#include "seadsa/TargetLibraryInfoGetter.hh"
#include "seadsa/support/RemovePtrToInt.hh"

#include "clam/NewPmPasses.hh"

#include "llvm/Analysis/CallGraph.h"

llvm::cl::opt<clam::CallSiteResolverKind> DevirtResolver(
    "devirt-resolver",
    llvm::cl::desc("Method used to devirtualize (resolve) indirect calls"),
    llvm::cl::values(
       clEnumValN(clam::RESOLVER_TYPES, "types",
		  "Choose all possible functions with same type signature"),
       clEnumValN(clam::RESOLVER_SEA_DSA, "sea-dsa",
		  "Sea-Dsa selects the potential callees "
		  "after discarding those with inconsistent types")),
    llvm::cl::init(clam::RESOLVER_TYPES));

static llvm::cl::opt<bool>AllowIndirectCalls(
    "devirt-allow-indirect-calls",
    llvm::cl::desc("Allow creation of indirect calls "
		   "during devirtualization "
		   "(required \"true\" for soundness)"),
    llvm::cl::Hidden, llvm::cl::init(false));

static llvm::cl::opt<bool> ResolveIncompleteCalls(
    "devirt-resolve-incomplete-calls",
    llvm::cl::desc("Resolve indirect calls that might still require "
                   "reasoning about other modules"
                   "(required \"false\" for soundness)"),
    llvm::cl::Hidden, llvm::cl::init(true));

static llvm::cl::opt<unsigned> MaxNumTargets(
    "devirt-max-num-targets",
    llvm::cl::desc(
    "Do not resolve if number of targets is greater than threshold"),
    llvm::cl::Hidden, llvm::cl::init(9999));
    

using namespace llvm;

namespace clam {

class DevirtualizeFunctionsLegacyPass : public ModulePass {
public:
  static char ID;

  DevirtualizeFunctionsLegacyPass() : ModulePass(ID) {}

  virtual bool runOnModule(Module &M) override {
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
      CSR.reset(new CallSiteResolverByTypes(M, DF.getStats()));
      break;
    }

    bool res = DF.resolveCallSites(M, &*CSR);
    return res;
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
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

  virtual StringRef getPassName() const override {
    return "Clam: Devirtualize indirect calls";
  }
};

char DevirtualizeFunctionsLegacyPass::ID = 0;

PreservedAnalyses DevirtualizeFunctionsPass::run(Module &M,
                                                 ModuleAnalysisManager &MAM) {
  DevirtualizeFunctions DF(/*CG*/ nullptr, AllowIndirectCalls);
  std::unique_ptr<CallSiteResolver> CSR;

  // Built here rather than pulled from the MAM: the resolver below is the only
  // consumer, and CompleteCallGraphAnalysis is not one of the analyses sea-dsa
  // exposes through SeaDsaAnalysis.hh. Its inputs are cached, though, so the
  // AllocWrapInfo/DsaLibFuncInfo work is shared with any other sea-dsa
  // consumer in the pipeline.
  std::unique_ptr<seadsa::CompleteCallGraphAnalysis> CCGA;
  switch (DevirtResolver) {
  case RESOLVER_SEA_DSA: {
    auto &AWI = MAM.getResult<seadsa::AllocWrapInfoAnalysis>(M);
    auto &DLFI = MAM.getResult<seadsa::DsaLibFuncInfoAnalysis>(M);
    auto &CG = MAM.getResult<CallGraphAnalysis>(M);
    CCGA = std::make_unique<seadsa::CompleteCallGraphAnalysis>(
        M.getDataLayout(), seadsa::mkTLIGetter(M, MAM), AWI.getAllocWrapInfo(),
        DLFI.getDsaLibFuncInfo(), CG);
    CCGA->runOnModule(M);
    CSR.reset(new CallSiteResolverByDsa<seadsa::CompleteCallGraphAnalysis>(
        M, *CCGA, ResolveIncompleteCalls, MaxNumTargets, DF.getStats()));
    break;
  }
  case RESOLVER_TYPES:
    CSR.reset(new CallSiteResolverByTypes(M, DF.getStats()));
    break;
  }

  if (!DF.resolveCallSites(M, &*CSR)) {
    return PreservedAnalyses::all();
  }
  // Indirect calls become branches over direct calls, and bounce functions are
  // added; DevirtualizeFunctions does not update the call graph.
  return PreservedAnalyses::none();
}

Pass *createDevirtualizeFunctionsPass() {
  return new DevirtualizeFunctionsLegacyPass();
}

} // namespace clam
