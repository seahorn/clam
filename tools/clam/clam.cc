///
// Clam -- Abstract Interpretation-based Analyzer for LLVM bitcode
///

#include "clam/config.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/IR/IRPrintingPasses.h" // legacy createPrintModulePass
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRPrinter/IRPrintingPasses.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"

#include "llvm/Transforms/IPO/GlobalDCE.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils/LowerInvoke.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

#include "clam/Clam.hh"
#include "clam/NewPmPasses.hh"
#include "clam/Passes.hh"

#include "seadsa/InitializePasses.hh"
#include "seadsa/support/RemovePtrToInt.hh"

#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Transforms/InstCombine/SeaInstCombine.h"
#else
#include "llvm/Transforms/InstCombine/InstCombine.h"
#endif

extern llvm::cl::OptionCategory ClamOptCat;

static llvm::cl::opt<std::string>
    InputFilename(llvm::cl::Positional,
                  llvm::cl::desc("<input LLVM bitcode file>"),
                  llvm::cl::Required, llvm::cl::value_desc("filename"),
		  llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<std::string>
    OutputFilename("o", llvm::cl::desc("Override output filename"),
                   llvm::cl::init(""), llvm::cl::value_desc("filename"),
		   llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
OutputAssembly("S", llvm::cl::desc("Write output as LLVM assembly"),
	       llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<std::string>
    AsmOutputFilename("oll", llvm::cl::desc("Output analyzed bitcode"),
                      llvm::cl::init(""), llvm::cl::value_desc("filename"),
		      llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<std::string> DefaultDataLayout(
    "default-data-layout",
    llvm::cl::desc("data layout string to use if not specified by module"),
    llvm::cl::init(""), llvm::cl::value_desc("layout-string"),
    llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool> DisableCrab(
    "no-crab",
    llvm::cl::desc("Output preprocessed bitcode but disabling Crab analysis"),
    llvm::cl::init(false), llvm::cl::Hidden,
    llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool> TurnUndefNondet(
    "clam-turn-undef-nondet",
    llvm::cl::desc("Turn undefined behaviour into non-determinism"),
    llvm::cl::init(false),
    llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    LowerUnsignedICmp("clam-lower-unsigned-icmp",
                      llvm::cl::desc("Lower ULT and ULE instructions"),
                      llvm::cl::init(false),
		      llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    LowerCstExpr("clam-lower-constant-expr",
                 llvm::cl::desc("Lower constant expressions to instructions"),
                 llvm::cl::init(true),
		 llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    LowerInvoke("clam-lower-invoke",
                llvm::cl::desc("Lower invoke instructions"),
                llvm::cl::init(true),
		llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    LowerSwitch("clam-lower-switch",
                llvm::cl::desc("Lower switch instructions"),
                llvm::cl::init(true),
		llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    LowerSelect("clam-lower-select",
                llvm::cl::desc("Lower all select instructions"),
                llvm::cl::init(false),
		llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    CrabOpt("crab-opt",
	    llvm::cl::desc("Optimize LLVM bitcode by using invariants"),
	    llvm::cl::init(false),
	    llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool> PromoteAssume(
    "crab-promote-assume",
    llvm::cl::desc("Promote verifier.assume to llvm.assume intrinsics"),
    llvm::cl::init(false),
    llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool> DotLLVMCFG(
    "clam-llvm-cfg-dot",
    llvm::cl::desc("Write a .dot file the analyzed LLVM CFG of each function"),
    llvm::cl::init(false),
    llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    NullCheck("clam-null-check-legacy",
              llvm::cl::desc("Insert checks for null dereference errors in LLVM IR"),
              llvm::cl::init(false),
	      llvm::cl::cat(ClamOptCat));

static llvm::cl::opt<bool>
    UafCheck("clam-uaf-check-legacy",
             llvm::cl::desc("Insert checks for use-after-free errors in LLVM IR"),
             llvm::cl::init(false),
	     llvm::cl::cat(ClamOptCat));


using namespace clam;

// removes extension from filename if there is one
std::string getFileName(const std::string &str) {
  std::string filename = str;
  size_t lastdot = str.find_last_of(".");
  if (lastdot != std::string::npos)
    filename = str.substr(0, lastdot);
  return filename;
}

int main(int argc, char **argv) {
  llvm::llvm_shutdown_obj shutdown; // calls llvm_shutdown() on exit

  //llvm::cl::HideUnrelatedOptions(ClamOptCat);  
  llvm::cl::ParseCommandLineOptions(
      argc, argv,
      "Clam -- Abstract Interpretation-based Analyzer of LLVM bitcode\n");

  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram PSTP(argc, argv);
  llvm::EnableDebugBuffering = true;

  std::error_code error_code;
  llvm::SMDiagnostic err;
  static llvm::LLVMContext context;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::ToolOutputFile> output;
  std::unique_ptr<llvm::ToolOutputFile> asmOutput;

  module = llvm::parseIRFile(InputFilename, err, context);
  if (!module) {
    if (llvm::errs().has_colors())
      llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: "
                 << "Bitcode was not properly read; " << err.getMessage()
                 << "\n";
    if (llvm::errs().has_colors())
      llvm::errs().resetColor();
    return 3;
  }

  if (!AsmOutputFilename.empty())
    asmOutput = std::make_unique<llvm::ToolOutputFile>(
        AsmOutputFilename.c_str(), error_code, llvm::sys::fs::OF_Text);
  if (error_code) {
    if (llvm::errs().has_colors())
      llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: Could not open " << AsmOutputFilename << ": "
                 << error_code.message() << "\n";
    if (llvm::errs().has_colors())
      llvm::errs().resetColor();
    return 3;
  }

  if (!OutputFilename.empty())
    output = std::make_unique<llvm::ToolOutputFile>(
        OutputFilename.c_str(), error_code, llvm::sys::fs::OF_None);

  if (error_code) {
    if (llvm::errs().has_colors())
      llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: Could not open " << OutputFilename << ": "
                 << error_code.message() << "\n";
    if (llvm::errs().has_colors())
      llvm::errs().resetColor();
    return 3;
  }

  ///////////////////////////////
  // initialise and run passes //
  ///////////////////////////////

  // The pipeline runs in three segments. Preprocessing and the post-Crab
  // cleanup are new-PM pipelines; ClamPass and the invariant-based Optimizer
  // are still legacy passes, so they run in a legacy PassManager in between.
  // LLVM offers no adaptor to mix the two in one pipeline, and all three
  // segments run over the same module in the original order.
  llvm::PassBuilder PB;
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  auto addFunctionPass = [](llvm::ModulePassManager &MPM, auto &&P) {
    MPM.addPass(llvm::createModuleToFunctionPassAdaptor(
        std::forward<decltype(P)>(P)));
  };

  llvm::legacy::PassManager pass_manager;
  llvm::PassRegistry &Registry = *llvm::PassRegistry::getPassRegistry();
  llvm::initializeCore(Registry);
  llvm::initializeTransformUtils(Registry);
  llvm::initializeAnalysis(Registry);

  /// call graph and other IPA passes
  // llvm::initializeIPA (Registry);
  // XXX: porting to 3.8
  llvm::initializeCallGraphWrapperPassPass(Registry);
  // XXX: commented while porting to 5.0
  // llvm::initializeCallGraphPrinterPass(Registry);
  llvm::initializeCallGraphViewerPass(Registry);
  // XXX: not sure if needed anymore
  llvm::initializeGlobalsAAWrapperPassPass(Registry);

  llvm::initializeAllocWrapInfoPass(Registry);
  llvm::initializeAllocSiteInfoPass(Registry);
  llvm::initializeRemovePtrToIntPass(Registry);
  llvm::initializeDsaAnalysisPass(Registry);
  llvm::initializeDsaInfoPassPass(Registry);
  llvm::initializeCompleteCallGraphPass(Registry);

  // add an appropriate DataLayout instance for the module
  const llvm::DataLayout *dl = &module->getDataLayout();
  if (!dl && !DefaultDataLayout.empty()) {
    module->setDataLayout(DefaultDataLayout);
    dl = &module->getDataLayout();
  }

  assert(dl && "Could not find Data Layout for the module");

  /**
   * Here only passes that are strictly necessary to avoid crashes or
   * too poor results. Passes that are only for improving precision
   * should be run in clam-pp.
   **/

  llvm::ModulePassManager preprocess;

  // kill unused internal global
  preprocess.addPass(llvm::GlobalDCEPass());
  addFunctionPass(preprocess, clam::RemoveUnreachableBlocksPass());

  // -- promote alloca's to registers
  addFunctionPass(preprocess, llvm::PromotePass());
  if (TurnUndefNondet) {
    // -- Turn undef into nondet
    preprocess.addPass(clam::NondetInitPass());
  }
  if (LowerInvoke) {
    // -- lower invoke's
    addFunctionPass(preprocess, llvm::LowerInvokePass());
    // cleanup after lowering invoke's
    addFunctionPass(preprocess, llvm::SimplifyCFGPass());
  }
  // -- ensure one single exit point per function
  addFunctionPass(preprocess, llvm::UnifyFunctionExitNodesPass());
  // -- remove unreachable blocks
  addFunctionPass(preprocess, clam::RemoveUnreachableBlocksPass());
  if (LowerSwitch) {
    // -- remove switch constructions
    addFunctionPass(preprocess, llvm::LowerSwitchPass());
    // cleanup after lowering switches
    addFunctionPass(preprocess, llvm::SimplifyCFGPass());
  }
  // -- lower constant expressions to instructions
  if (LowerCstExpr) {
    preprocess.addPass(clam::LowerCstExprPass());
    // cleanup after lowering constant expressions
    addFunctionPass(preprocess, llvm::DCEPass());
  }
  if (TurnUndefNondet) {
    addFunctionPass(preprocess, clam::DeadNondetElimPass());
  }

  // -- lower ULT and ULE instructions
  if (LowerUnsignedICmp) {
    addFunctionPass(preprocess, clam::LowerUnsignedICmpPass());
    // cleanup unnecessary and unreachable blocks
    addFunctionPass(preprocess, llvm::SimplifyCFGPass());
    addFunctionPass(preprocess, clam::RemoveUnreachableBlocksPass());
  }

  // -- remove ptrtoint and inttoptr instructions
  addFunctionPass(preprocess, seadsa::RemovePtrToIntPass());

  // -- must be the last ones before running crab.
  if (LowerSelect) {
    addFunctionPass(preprocess, clam::LowerSelectPass());
  }

  // -- ensure one single exit point per function
  //    LowerUnsignedICmpPass and LowerSelect can add multiple
  //    returns.
  addFunctionPass(preprocess, llvm::UnifyFunctionExitNodesPass());

  preprocess.run(*module.get(), MAM);

  // The analysis itself. ClamPass, the property-instrumentation checks around
  // it and the invariant-based Optimizer have not been ported to the new PM
  // yet, so this middle segment stays on the legacy manager.
  bool has_legacy_passes = false;

  if (!DisableCrab) {
    /// -- Add some properties to check
    if (NullCheck)
      pass_manager.add(clam::createNullCheckPass());
    if (UafCheck)
      pass_manager.add(clam::createUseAfterFreeCheckPass());
    /// -- run the crab analyzer
    pass_manager.add(new clam::ClamPass());
    if (DotLLVMCFG)
      pass_manager.add(createAnnotatedCFGPrinterPass());
    has_legacy_passes = true;
  }

  if (!AsmOutputFilename.empty()) {
    pass_manager.add(createPrintModulePass(asmOutput->os()));
    has_legacy_passes = true;
  }

  if (!DisableCrab && CrabOpt) {
    // post-processing of the bitcode using Crab invariants
    pass_manager.add(clam::createOptimizerPass());
    has_legacy_passes = true;
  }

  if (has_legacy_passes) {
    pass_manager.run(*module.get());
  }

  // Cleanup after the Optimizer, plus the output writers.
  llvm::ModulePassManager postprocess;
  bool has_postprocess_passes = false;

  if (!DisableCrab && CrabOpt) {
    //// Cleanup
    // -- simplify invariants added in the bitecode.
#ifdef HAVE_LLVM_SEAHORN
    const unsigned MaxIterations = 1000; /*same value used by LLVM*/
    const bool AvoidBv = true;
    const bool AvoidUnsignedICmp = true;
    const bool AvoidIntToPtr = true;
    const bool AvoidAliasing = true;
    const bool AvoidDisequalities = true;
    addFunctionPass(postprocess, llvm_seahorn::SeaInstCombinePass(
                                     MaxIterations, AvoidBv, AvoidUnsignedICmp,
                                     AvoidIntToPtr, AvoidAliasing,
                                     AvoidDisequalities));
#else
    addFunctionPass(postprocess, llvm::InstCombinePass());
#endif
    // -- remove dead edges and blocks
    addFunctionPass(postprocess, llvm::SimplifyCFGPass());
    // -- remove global strings and values
    postprocess.addPass(llvm::GlobalDCEPass());

    if (PromoteAssume) {
      // -- promote verifier.assume to llvm.assume intrinsics
      addFunctionPass(postprocess, clam::PromoteAssumePass());
    }
    has_postprocess_passes = true;
  }

  if (!OutputFilename.empty()) {
    if (OutputAssembly)
      postprocess.addPass(llvm::PrintModulePass(output->os()));
    else
      postprocess.addPass(llvm::BitcodeWriterPass(output->os()));
    has_postprocess_passes = true;
  }

  if (has_postprocess_passes) {
    postprocess.run(*module.get(), MAM);
  }


  if (!AsmOutputFilename.empty())
    asmOutput->keep();
  if (!OutputFilename.empty())
    output->keep();

  return 0;
}
