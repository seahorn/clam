///
// clam-pp -- LLVM bitcode Pre-Processor for static analysis
///

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

#include "clam/Passes.hh"
#include "clam/config.h"

#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Transforms/Scalar.h"
#endif

#include "seadsa/InitializePasses.hh"

static llvm::cl::opt<std::string>
    InputFilename(llvm::cl::Positional,
                  llvm::cl::desc("<input LLVM bitcode file>"),
                  llvm::cl::Required, llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string>
    OutputFilename("o", llvm::cl::desc("Override output filename"),
                   llvm::cl::init(""), llvm::cl::value_desc("filename"));

static llvm::cl::opt<bool>
    OutputAssembly("S", llvm::cl::desc("Write output as LLVM assembly"));

static llvm::cl::opt<std::string>
    AsmOutputFilename("oll", llvm::cl::desc("Output analyzed bitcode"),
                      llvm::cl::init(""), llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string> DefaultDataLayout(
    "default-data-layout",
    llvm::cl::desc("data layout string to use if not specified by module"),
    llvm::cl::init(""), llvm::cl::value_desc("layout-string"));

static llvm::cl::opt<bool>
    PromoteMalloc("clam-promote-malloc",
                  llvm::cl::desc("Promote malloc to alloca"),
                  llvm::cl::init(true));

static llvm::cl::opt<bool> InlineAll("clam-inline-all",
                                     llvm::cl::desc("Inline all functions"),
                                     llvm::cl::init(false));

static llvm::cl::opt<bool>
    Devirtualize("clam-devirt", llvm::cl::desc("Resolve indirect calls"),
                 llvm::cl::init(false));

static llvm::cl::opt<bool>
    Scalarize("clam-scalarize", llvm::cl::desc("Scalarize vector operations"),
              llvm::cl::init(true));

static llvm::cl::opt<bool>
    LowerInvoke("clam-lower-invoke",
                llvm::cl::desc("Lower invoke instructions"),
                llvm::cl::init(true));

static llvm::cl::opt<bool>
    LowerCstExpr("clam-lower-constant-expr",
                 llvm::cl::desc("Lower constant expressions to instructions"),
                 llvm::cl::init(true));

static llvm::cl::opt<bool>
    LowerSwitch("clam-lower-switch",
                llvm::cl::desc("Lower switch instructions"),
                llvm::cl::init(true));

static llvm::cl::opt<bool>
    LowerSelect("clam-lower-select",
                llvm::cl::desc("Lower all select instructions"),
                llvm::cl::init(false));

static llvm::cl::opt<bool> ExternalizeAddrTakenFuncs(
    "clam-externalize-addr-taken-funcs",
    llvm::cl::desc("Externalize uses of address-taken functions"),
    llvm::cl::init(false));

static llvm::cl::opt<bool>
    LowerUnsignedICmp("clam-lower-unsigned-icmp",
                      llvm::cl::desc("Lower ULT and ULE instructions"),
                      llvm::cl::init(false));

static llvm::cl::opt<bool>
    OptimizeLoops("clam-pp-loops", llvm::cl::desc("Perform loop optimizations"),
                  llvm::cl::init(false));

static llvm::cl::opt<unsigned>
    PeelLoops("clam-peel-loops", llvm::cl::desc("Number of iterations to peel"),
              llvm::cl::init(0));

static llvm::cl::opt<bool> TurnUndefNondet(
    "clam-turn-undef-nondet",
    llvm::cl::desc("Turn undefined behaviour into non-determinism"),
    llvm::cl::init(false));

// removes extension from filename if there is one
std::string getFileName(const std::string &str) {
  std::string filename = str;
  size_t lastdot = str.find_last_of(".");
  if (lastdot != std::string::npos)
    filename = str.substr(0, lastdot);
  return filename;
}

void breakAllocas(llvm::legacy::PassManager &pass_manager) {
  // -- can remove bitcast from bitcast(alloca(...))
  pass_manager.add(clam::createInstCombine());
  pass_manager.add(clam::createRemoveUnreachableBlocksPass());
  // -- break alloca's into scalars
  pass_manager.add(llvm::createSROAPass());
  if (TurnUndefNondet) {
    // -- Turn undef into nondet (undef are created by SROA when it calls
    // mem2reg)
    pass_manager.add(clam::createNondetInitPass());
  }
}

int main(int argc, char **argv) {
  llvm::llvm_shutdown_obj shutdown; // calls llvm_shutdown() on exit
  llvm::cl::ParseCommandLineOptions(
      argc, argv, "clam-pp-- LLVM bitcode Pre-Processor for static analysis\n");

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
  if (module.get() == 0) {
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

  // seadsa
  llvm::initializeCompleteCallGraphPass(Registry);
  llvm::initializeRemovePtrToIntPass(Registry);

  // add an appropriate DataLayout instance for the module
  const llvm::DataLayout *dl = &module->getDataLayout();
  if (!dl && !DefaultDataLayout.empty()) {
    module->setDataLayout(DefaultDataLayout);
    dl = &module->getDataLayout();
  }

  assert(dl && "Could not find Data Layout for the module");


  // -- Create an entry point if main doesn't exist
  pass_manager.add(clam::createInsertEntryPointPass());

  if (PromoteMalloc) {
    // -- promote top-level mallocs to alloca
    pass_manager.add(clam::createPromoteMallocPass());
  }

  // -- turn all functions internal so that we can apply some global
  // -- optimizations inline them if requested
  auto PreserveMain = [=](const llvm::GlobalValue &GV) {
    return GV.getName() == "main";
  };
  pass_manager.add(llvm::createInternalizePass(PreserveMain));

  if (Devirtualize) {
    pass_manager.add(llvm::createWholeProgramDevirtPass(nullptr, nullptr));    
    pass_manager.add(clam::createDevirtualizeFunctionsPass());
  }

  // -- externalize some user-selected functions
  pass_manager.add(clam::createExternalizeFunctionsPass());
 
  if (ExternalizeAddrTakenFuncs) {
    // -- externalize uses of address-taken functions
    pass_manager.add(clam::createExternalizeAddressTakenFunctionsPass());
  }

  // kill unused internal global
  pass_manager.add(llvm::createGlobalDCEPass());
  pass_manager.add(clam::createRemoveUnreachableBlocksPass());
  // -- global optimizations
  pass_manager.add(llvm::createGlobalOptimizerPass());

  // -- SSA
  pass_manager.add(llvm::createPromoteMemoryToRegisterPass());
  if (TurnUndefNondet) {
    // -- Turn undef into nondet
    pass_manager.add(clam::createNondetInitPass());
  }

  // -- cleanup after SSA
  pass_manager.add(clam::createInstCombine());
  pass_manager.add(llvm::createCFGSimplificationPass());
  breakAllocas(pass_manager);

  // -- global value numbering and redundant load elimination
  pass_manager.add(llvm::createGVNPass());

  // -- cleanup after break aggregates
  pass_manager.add(clam::createInstCombine());
  pass_manager.add(llvm::createCFGSimplificationPass());

  if (TurnUndefNondet) {
    // eliminate unused calls to verifier.nondet() functions
    pass_manager.add(clam::createDeadNondetElimPass());
  }

  if (LowerInvoke) {
    // -- lower invoke's
    pass_manager.add(llvm::createLowerInvokePass());
    // cleanup after lowering invoke's
    pass_manager.add(llvm::createCFGSimplificationPass());
  }

  if (InlineAll) {
    pass_manager.add(clam::createMarkInternalInlinePass());
    pass_manager.add(llvm::createAlwaysInlinerLegacyPass());
    // // after inlining we promote malloc to alloca instructions
    // pass_manager.add(clam::createPromoteMallocPass());
    // // kill unused internal global
    // pass_manager.add(llvm::createGlobalDCEPass());
    pass_manager.add(
        llvm::createGlobalDCEPass()); // kill unused internal global
    // -- promote malloc to alloca
    pass_manager.add(clam::createPromoteMallocPass());
    pass_manager.add(
        llvm::createGlobalDCEPass()); // kill unused internal global
    // XXX: for svcomp ssh programs we need to run twice to break all
    // relevant allocas
    breakAllocas(pass_manager);
    breakAllocas(pass_manager);
  }

  pass_manager.add(clam::createRemoveUnreachableBlocksPass());
  pass_manager.add(llvm::createDeadCodeEliminationPass());
  // Superseded by DCE
  //pass_manager.add(llvm::createDeadInstEliminationPass());

  if (OptimizeLoops || PeelLoops > 0) {
    // canonical form for loops
    pass_manager.add(llvm::createLoopSimplifyPass());
    // cleanup unnecessary blocks
    pass_manager.add(llvm::createCFGSimplificationPass());
    // rotate loops:
    // we don't like rotated loops unless it's strictly necessary
    if (PeelLoops > 0) {
#ifdef HAVE_LLVM_SEAHORN      
      pass_manager.add(llvm_seahorn::createLoopRotatePass(/*1023*/));
#else
      pass_manager.add(llvm::createLoopRotatePass());
#endif       
    }
    // loop-closed SSA
    pass_manager.add(llvm::createLCSSAPass());
    if (PeelLoops > 0)
      pass_manager.add(clam::createLoopPeelerPass(PeelLoops));
#ifdef HAVE_LLVM_SEAHORN
    // induction variable requires loop-closed SSA
    // Preserved by LoopPeelerPass
    // pass_manager.add(llvm::createLCSSAPass());
    // induction variable
    pass_manager.add(llvm_seahorn::createIndVarSimplifyPass());
#endif
    // trivial invariants outside loops
    pass_manager.add(llvm::createBasicAAWrapperPass());
    pass_manager.add(llvm::createLICMPass()); // LICM needs alias analysis
    pass_manager.add(llvm::createPromoteMemoryToRegisterPass());
    // dead loop elimination
    pass_manager.add(llvm::createLoopDeletionPass());
    // cleanup unnecessary blocks
    pass_manager.add(llvm::createCFGSimplificationPass());
  }

  // -- ensure one single exit point per function
  pass_manager.add(llvm::createUnifyFunctionExitNodesPass());
  pass_manager.add(llvm::createGlobalDCEPass());
  pass_manager.add(llvm::createDeadCodeEliminationPass());
  // -- remove unreachable blocks also dead cycles
  pass_manager.add(clam::createRemoveUnreachableBlocksPass());

  if (Scalarize) {
    pass_manager.add(llvm::createScalarizerPass());
    pass_manager.add(llvm::createDeadCodeEliminationPass());
  }

  if (LowerSwitch) {
    // -- remove switch constructions
    pass_manager.add(llvm::createLowerSwitchPass());
    // cleanup unnecessary blocks
    pass_manager.add(llvm::createCFGSimplificationPass());
  }

  if (LowerCstExpr) {
    // -- lower constant expressions to instructions
    pass_manager.add(clam::createLowerCstExprPass());
    pass_manager.add(llvm::createDeadCodeEliminationPass());
  }

  // -- lower ULT and ULE instructions
  if (LowerUnsignedICmp) {
    pass_manager.add(clam::createLowerUnsignedICmpPass());
    // cleanup unnecessary and unreachable blocks
    pass_manager.add(llvm::createCFGSimplificationPass());
    pass_manager.add(clam::createRemoveUnreachableBlocksPass());
  }

  // -- must be the last one to avoid llvm undoing it
  if (LowerSelect) {
    pass_manager.add(clam::createLowerSelectPass());
  }

  if (!AsmOutputFilename.empty())
    pass_manager.add(createPrintModulePass(asmOutput->os()));

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
