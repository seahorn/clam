///
// Clam -- Abstract Interpretation-based Analyzer for LLVM bitcode
///

#include "clam/config.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/LinkAllPasses.h"
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

#include "clam/Clam.hh"
#include "clam/Passes.hh"

#include "seadsa/InitializePasses.hh"
#include "seadsa/support/RemovePtrToInt.hh"

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

  // kill unused internal global
  pass_manager.add(llvm::createGlobalDCEPass());
  pass_manager.add(clam::createRemoveUnreachableBlocksPass());

  // -- promote alloca's to registers
  pass_manager.add(llvm::createPromoteMemoryToRegisterPass());
  if (TurnUndefNondet) {
    // -- Turn undef into nondet
    pass_manager.add(clam::createNondetInitPass());
  }
  if (LowerInvoke) {
    // -- lower invoke's
    pass_manager.add(llvm::createLowerInvokePass());
    // cleanup after lowering invoke's
    pass_manager.add(llvm::createCFGSimplificationPass());
  }
  // -- ensure one single exit point per function
  pass_manager.add(llvm::createUnifyFunctionExitNodesPass());
  // -- remove unreachable blocks
  pass_manager.add(clam::createRemoveUnreachableBlocksPass());
  if (LowerSwitch) {
    // -- remove switch constructions
    pass_manager.add(llvm::createLowerSwitchPass());
    // cleanup after lowering switches
    pass_manager.add(llvm::createCFGSimplificationPass());
  }
  // -- lower constant expressions to instructions
  if (LowerCstExpr) {
    pass_manager.add(clam::createLowerCstExprPass());
    // cleanup after lowering constant expressions
    pass_manager.add(llvm::createDeadCodeEliminationPass());
  }
  if (TurnUndefNondet) {
    pass_manager.add(clam::createDeadNondetElimPass());
  }

  // -- lower ULT and ULE instructions
  if (LowerUnsignedICmp) {
    pass_manager.add(clam::createLowerUnsignedICmpPass());
    // cleanup unnecessary and unreachable blocks
    pass_manager.add(llvm::createCFGSimplificationPass());
    pass_manager.add(clam::createRemoveUnreachableBlocksPass());
  }

  // -- remove ptrtoint and inttoptr instructions
  pass_manager.add(seadsa::createRemovePtrToIntPass());

  // -- must be the last ones before running crab.
  if (LowerSelect) {
    pass_manager.add(clam::createLowerSelectPass());
  }

  // -- ensure one single exit point per function
  //    LowerUnsignedICmpPass and LowerSelect can add multiple
  //    returns.
  pass_manager.add(llvm::createUnifyFunctionExitNodesPass());

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
  }

  if (!AsmOutputFilename.empty()) {
    pass_manager.add(createPrintModulePass(asmOutput->os()));
  }

  if (!DisableCrab && CrabOpt) {
    // post-processing of the bitcode using Crab invariants
    pass_manager.add(clam::createOptimizerPass());

    //// Cleanup
    // -- simplify invariants added in the bitecode.
    pass_manager.add(clam::createInstCombine());
    // -- remove dead edges and blocks
    pass_manager.add(llvm::createCFGSimplificationPass());
    // -- remove global strings and values
    pass_manager.add(llvm::createGlobalDCEPass());
    
    if (PromoteAssume) {
      // -- promote verifier.assume to llvm.assume intrinsics
      pass_manager.add(clam::createPromoteAssumePass());
    }
  }

  if (!OutputFilename.empty()) {
    if (OutputAssembly)
      pass_manager.add(createPrintModulePass(output->os()));
    else
      pass_manager.add(createBitcodeWriterPass(output->os()));
  }

  pass_manager.run(*module.get());

  if (!AsmOutputFilename.empty())
    asmOutput->keep();
  if (!OutputFilename.empty())
    output->keep();

  return 0;
}
