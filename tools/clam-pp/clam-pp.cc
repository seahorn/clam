///
// clam-pp -- LLVM bitcode Pre-Processor for static analysis
///

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRPrinter/IRPrintingPasses.h"
#include "llvm/IRReader/IRReader.h"
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

#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/GlobalDCE.h"
#include "llvm/Transforms/IPO/GlobalOpt.h"
#include "llvm/Transforms/IPO/Internalize.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/LICM.h"
#include "llvm/Transforms/Scalar/LoopDeletion.h"
#include "llvm/Transforms/Scalar/LoopPassManager.h"
#include "llvm/Transforms/Scalar/LoopRotation.h"
#include "llvm/Transforms/Scalar/SROA.h"
#include "llvm/Transforms/Scalar/Scalarizer.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils/LCSSA.h"
#include "llvm/Transforms/Utils/LoopSimplify.h"
#include "llvm/Transforms/Utils/LowerInvoke.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

#include "clam/NewPmPasses.hh"
#include "clam/config.h"

#include "seadsa/SeaDsaAnalysis.hh"
#include "seadsa/support/RemovePtrToInt.hh"

#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Loops/SeaIndVarSimplify.h"
#include "llvm_seahorn/Transforms/InstCombine/SeaInstCombine.h"
#include "llvm_seahorn/Transforms/Scalar/SeaLoopRotate.h"
#else
#include "llvm/Transforms/InstCombine/InstCombine.h"
#endif

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

static llvm::cl::opt<bool> LowerMinMaxIntrinsics(
    "clam-lower-minmax-intrinsics",
    llvm::cl::desc("Lower umax/umin/smax/smin intrinsics to icmp and select"),
    llvm::cl::init(true));

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

/// Clam's instcombine: llvm-seahorn's when available, stock LLVM's otherwise.
/// The Avoid* knobs keep instcombine from producing IR that Clam's translation
/// to CrabIR cannot represent.
static llvm::FunctionPassManager mkInstCombine() {
  llvm::FunctionPassManager FPM;
#ifdef HAVE_LLVM_SEAHORN
  const unsigned MaxIterations = 1000; /*same value used by LLVM*/
  const bool AvoidBv = true;
  const bool AvoidUnsignedICmp = true;
  const bool AvoidIntToPtr = true;
  const bool AvoidAliasing = true;
  const bool AvoidDisequalities = true;
  FPM.addPass(llvm_seahorn::SeaInstCombinePass(
      MaxIterations, AvoidBv, AvoidUnsignedICmp, AvoidIntToPtr, AvoidAliasing,
      AvoidDisequalities));
#else
  FPM.addPass(llvm::InstCombinePass());
#endif
  return FPM;
}

/// Add a function pass to a module pipeline.
template <typename PassT>
static void addFunctionPass(llvm::ModulePassManager &MPM, PassT &&P) {
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::forward<PassT>(P)));
}

/// Add a loop pass to a module pipeline. The caller is responsible for having
/// put the loops in simplified/LCSSA form first, as the new PM does not do it
/// implicitly.
template <typename PassT>
static void addLoopPass(llvm::ModulePassManager &MPM, PassT &&P,
                        bool UseMemorySSA = false) {
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(
      llvm::createFunctionToLoopPassAdaptor(std::forward<PassT>(P),
                                            UseMemorySSA)));
}

static void breakAllocas(llvm::ModulePassManager &MPM) {
  // -- can remove bitcast from bitcast(alloca(...))
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(mkInstCombine()));
  addFunctionPass(MPM, clam::RemoveUnreachableBlocksPass());
  // -- break alloca's into scalars
  addFunctionPass(MPM, llvm::SROAPass(llvm::SROAOptions::ModifyCFG));
  if (TurnUndefNondet) {
    // -- Turn undef into nondet (undef are created by SROA when it calls
    // mem2reg)
    MPM.addPass(clam::NondetInitPass());
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

  // The pipeline runs under the new pass manager. PassBuilder registers the
  // standard analyses in the four managers and cross-registers the proxies
  // between them, so any pass below can ask for the analyses it needs.
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
  // PassBuilder only knows LLVM's own analyses. sea-dsa's have to be
  // registered by the consumer, or the getResult<> calls in
  // clam::DevirtualizeFunctionsPass hit an unregistered key and crash.
  MAM.registerPass([] { return seadsa::AllocWrapInfoAnalysis(); });
  MAM.registerPass([] { return seadsa::DsaLibFuncInfoAnalysis(); });

  llvm::ModulePassManager pass_manager;

  // add an appropriate DataLayout instance for the module
  const llvm::DataLayout *dl = &module->getDataLayout();
  if (!dl && !DefaultDataLayout.empty()) {
    module->setDataLayout(DefaultDataLayout);
    dl = &module->getDataLayout();
  }

  assert(dl && "Could not find Data Layout for the module");


  // -- Create an entry point if main doesn't exist
  pass_manager.addPass(clam::InsertEntryPointPass());

  if (PromoteMalloc) {
    // -- promote top-level mallocs to alloca
    addFunctionPass(pass_manager, clam::PromoteMallocPass());
  }

  // -- turn all functions internal so that we can apply some global
  // -- optimizations inline them if requested
  auto PreserveMain = [=](const llvm::GlobalValue &GV) {
    return GV.getName() == "main";
  };
  pass_manager.addPass(llvm::InternalizePass(PreserveMain));

  if (Devirtualize) {
    // LLVM 15 removed the legacy-PM WholeProgramDevirt pass
    // (createWholeProgramDevirtPass); only the new-PM WholeProgramDevirtPass
    // remains. Clam's own devirtualization pass below performs the
    // indirect-call resolution. With --devirt-resolver=sea-dsa it wants
    // ptrtoint/inttoptr gone first, which the legacy pass used to get by
    // requiring seadsa::RemovePtrToInt; the new PM has no "required
    // transform", so schedule it explicitly.
    addFunctionPass(pass_manager, seadsa::RemovePtrToIntPass());
    pass_manager.addPass(clam::DevirtualizeFunctionsPass());
  }

  // -- externalize some user-selected functions
  pass_manager.addPass(clam::ExternalizeFunctionsPass());

  if (ExternalizeAddrTakenFuncs) {
    // -- externalize uses of address-taken functions
    pass_manager.addPass(clam::ExternalizeAddressTakenFunctionsPass());
  }

  // kill unused internal global
  pass_manager.addPass(llvm::GlobalDCEPass());
  addFunctionPass(pass_manager, clam::RemoveUnreachableBlocksPass());
  // -- global optimizations
  pass_manager.addPass(llvm::GlobalOptPass());

  // -- SSA
  addFunctionPass(pass_manager, llvm::PromotePass());
  if (TurnUndefNondet) {
    // -- Turn undef into nondet
    pass_manager.addPass(clam::NondetInitPass());
  }

  // -- cleanup after SSA
  pass_manager.addPass(llvm::createModuleToFunctionPassAdaptor(mkInstCombine()));
  addFunctionPass(pass_manager, llvm::SimplifyCFGPass());
  breakAllocas(pass_manager);

  // -- global value numbering and redundant load elimination
  addFunctionPass(pass_manager, llvm::GVNPass());

  // -- cleanup after break aggregates
  pass_manager.addPass(llvm::createModuleToFunctionPassAdaptor(mkInstCombine()));
  addFunctionPass(pass_manager, llvm::SimplifyCFGPass());

  if (TurnUndefNondet) {
    // eliminate unused calls to verifier.nondet() functions
    addFunctionPass(pass_manager, clam::DeadNondetElimPass());
  }

  if (LowerInvoke) {
    // -- lower invoke's
    addFunctionPass(pass_manager, llvm::LowerInvokePass());
    // cleanup after lowering invoke's
    addFunctionPass(pass_manager, llvm::SimplifyCFGPass());
  }

  if (InlineAll) {
    pass_manager.addPass(clam::MarkInternalInlinePass());
    pass_manager.addPass(llvm::AlwaysInlinerPass());
    // kill unused internal global
    pass_manager.addPass(llvm::GlobalDCEPass());
    // -- promote malloc to alloca
    addFunctionPass(pass_manager, clam::PromoteMallocPass());
    // kill unused internal global
    pass_manager.addPass(llvm::GlobalDCEPass());
    // XXX: for svcomp ssh programs we need to run twice to break all
    // relevant allocas
    breakAllocas(pass_manager);
    breakAllocas(pass_manager);
  }

  addFunctionPass(pass_manager, clam::RemoveUnreachableBlocksPass());
  addFunctionPass(pass_manager, llvm::DCEPass());

  if (OptimizeLoops || PeelLoops > 0) {
    // canonical form for loops
    addFunctionPass(pass_manager, llvm::LoopSimplifyPass());
    // cleanup unnecessary blocks
    addFunctionPass(pass_manager, llvm::SimplifyCFGPass());
    // rotate loops:
    // we don't like rotated loops unless it's strictly necessary
    if (PeelLoops > 0) {
#ifdef HAVE_LLVM_SEAHORN
      addLoopPass(pass_manager, llvm_seahorn::SeaLoopRotatePass(/*1023*/));
#else
      addLoopPass(pass_manager, llvm::LoopRotatePass());
#endif
    }
    // loop-closed SSA
    addFunctionPass(pass_manager, llvm::LCSSAPass());
    if (PeelLoops > 0)
      addLoopPass(pass_manager, clam::LoopPeelerPass(PeelLoops));
#ifdef HAVE_LLVM_SEAHORN
    // induction variable requires loop-closed SSA
    // Preserved by LoopPeelerPass
    // induction variable
    addLoopPass(pass_manager, llvm::SeaIndVarSimplifyPass());
#endif
    // trivial invariants outside loops
    // No BasicAA pass to schedule: under the new PM alias analysis is an
    // analysis (registered above), which LICM requests on demand.
    addLoopPass(pass_manager, llvm::LICMPass(llvm::LICMOptions()),
                /*UseMemorySSA=*/true);
    addFunctionPass(pass_manager, llvm::PromotePass());
    // dead loop elimination
    addLoopPass(pass_manager, llvm::LoopDeletionPass());
    // cleanup unnecessary blocks
    addFunctionPass(pass_manager, llvm::SimplifyCFGPass());
  }

  // -- ensure one single exit point per function
  addFunctionPass(pass_manager, llvm::UnifyFunctionExitNodesPass());
  pass_manager.addPass(llvm::GlobalDCEPass());
  addFunctionPass(pass_manager, llvm::DCEPass());
  // -- remove unreachable blocks also dead cycles
  addFunctionPass(pass_manager, clam::RemoveUnreachableBlocksPass());

  if (Scalarize) {
    addFunctionPass(pass_manager, llvm::ScalarizerPass());
    addFunctionPass(pass_manager, llvm::DCEPass());
  }

  if (LowerSwitch) {
    // -- remove switch constructions
    addFunctionPass(pass_manager, llvm::LowerSwitchPass());
    // cleanup unnecessary blocks
    addFunctionPass(pass_manager, llvm::SimplifyCFGPass());
  }

  if (LowerCstExpr) {
    // -- lower constant expressions to instructions
    pass_manager.addPass(clam::LowerCstExprPass());
    addFunctionPass(pass_manager, llvm::DCEPass());
  }

  // -- lower ULT and ULE instructions
  if (LowerUnsignedICmp) {
    addFunctionPass(pass_manager, clam::LowerUnsignedICmpPass());
    // cleanup unnecessary and unreachable blocks
    addFunctionPass(pass_manager, llvm::SimplifyCFGPass());
    addFunctionPass(pass_manager, clam::RemoveUnreachableBlocksPass());
  }

  // -- undo InstCombine's folding of min/max selects into intrinsics, which
  //    CfgBuilder cannot translate. Like the select lowering below it has to
  //    come after the last InstCombine of the pipeline, or it is folded back.
  if (LowerMinMaxIntrinsics) {
    addFunctionPass(pass_manager, clam::LowerMinMaxIntrinsicsPass());
  }

  // -- must be the last one to avoid llvm undoing it
  if (LowerSelect) {
    addFunctionPass(pass_manager, clam::LowerSelectPass());
  }

  if (!AsmOutputFilename.empty())
    pass_manager.addPass(llvm::PrintModulePass(asmOutput->os()));

  if (!OutputFilename.empty()) {
    if (OutputAssembly)
      pass_manager.addPass(llvm::PrintModulePass(output->os()));
    else
      pass_manager.addPass(llvm::BitcodeWriterPass(output->os()));
  }

  pass_manager.run(*module.get(), MAM);

  if (!AsmOutputFilename.empty())
    asmOutput->keep();
  if (!OutputFilename.empty())
    output->keep();

  return 0;
}
