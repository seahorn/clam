///
// crabllvm-pp -- LLVM bitcode Pre-Processor for static analysis
///

#include "llvm/LinkAllPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"

#include "crab_llvm/config.h"
#include "crab_llvm/Passes.hh"

#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Transforms/Scalar.h"
#endif 

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::Positional, llvm::cl::desc("<input LLVM bitcode file>"),
              llvm::cl::Required, llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("Override output filename"),
               llvm::cl::init(""), llvm::cl::value_desc("filename"));

static llvm::cl::opt<bool>
OutputAssembly("S", llvm::cl::desc("Write output as LLVM assembly"));

static llvm::cl::opt<std::string>
AsmOutputFilename("oll",
	llvm::cl::desc("Output analyzed bitcode"),
        llvm::cl::init(""), llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string>
DefaultDataLayout("default-data-layout",
        llvm::cl::desc("data layout string to use if not specified by module"),
        llvm::cl::init(""), llvm::cl::value_desc("layout-string"));

static llvm::cl::opt<bool>
InlineAll ("crab-inline-all",
	   llvm::cl::desc ("Inline all functions"),
           llvm::cl::init (false));

static llvm::cl::opt<bool>
Devirtualize ("crab-devirt", 
              llvm::cl::desc ("Resolve indirect calls"),
              llvm::cl::init (false));

static llvm::cl::opt<bool>
LowerSelect ("crab-lower-select",
	     llvm::cl::desc ("Lower all select instructions"),
             llvm::cl::init (false));

static llvm::cl::opt<bool>
LowerGv ("crab-lower-gv",
	 llvm::cl::desc ("Lower global initializers in main"),
	 llvm::cl::init (true));

static llvm::cl::opt<bool>
ExternalizeAddrTakenFuncs ("crab-externalize-addr-taken-funcs", 
         llvm::cl::desc ("Externalize uses of address-taken functions"),
         llvm::cl::init (false));

static llvm::cl::opt<bool>
LowerUnsignedICmp("crab-lower-unsigned-icmp",
	 llvm::cl::desc ("Lower ULT and ULE instructions"),
	 llvm::cl::init (false));

static llvm::cl::opt<bool>
OptimizeLoops ("crab-llvm-pp-loops", 
               llvm::cl::desc ("Perform loop optimizations"),
               llvm::cl::init (false));

static llvm::cl::opt<bool>
TurnUndefNondet ("crab-turn-undef-nondet", 
                 llvm::cl::desc ("Turn undefined behaviour into non-determinism"),
                 llvm::cl::init (false));

static llvm::cl::opt<int>
SROA_Threshold ("sroa-threshold",
                llvm::cl::desc ("Threshold for ScalarReplAggregates pass"),
                llvm::cl::init(INT_MAX));
static llvm::cl::opt<int>
SROA_StructMemThreshold ("sroa-struct",
                llvm::cl::desc ("Structure threshold for ScalarReplAggregates"),
                llvm::cl::init (INT_MAX));
static llvm::cl::opt<int>
SROA_ArrayElementThreshold ("sroa-array",
                llvm::cl::desc ("Array threshold for ScalarReplAggregates"),
                llvm::cl::init (INT_MAX));
static llvm::cl::opt<int>
SROA_ScalarLoadThreshold ("sroa-scalar-load",
                llvm::cl::desc ("Scalar load threshold for ScalarReplAggregates"),
                llvm::cl::init (-1));

// removes extension from filename if there is one
std::string getFileName(const std::string &str) {
  std::string filename = str;
  size_t lastdot = str.find_last_of(".");
  if (lastdot != std::string::npos)
    filename = str.substr(0, lastdot);
  return filename;
}

void break_allocas(llvm::legacy::PassManager &pass_manager) {
    #ifdef HAVE_LLVM_SEAHORN
    // -- can remove bitcast from bitcast(alloca(...))
    pass_manager.add (llvm_seahorn::createInstructionCombiningPass ());
    #endif     
    pass_manager.add (crab_llvm::createRemoveUnreachableBlocksPass ());
    // -- break alloca's into scalars
    pass_manager.add (llvm::createScalarReplAggregatesPass
    		      (SROA_Threshold,
    		       true,
    		       SROA_StructMemThreshold,
    		       SROA_ArrayElementThreshold,
    		       SROA_ScalarLoadThreshold));
    #ifdef HAVE_LLVM_SEAHORN
    if (TurnUndefNondet) {
      // -- Turn undef into nondet (undef are created by SROA when it calls mem2reg)
      pass_manager.add (llvm_seahorn::createNondetInitPass ());
    }
    #endif
}

int main(int argc, char **argv) {
  llvm::llvm_shutdown_obj shutdown;  // calls llvm_shutdown() on exit
  llvm::cl::ParseCommandLineOptions(argc, argv,
  "crabllvm-pp-- LLVM bitcode Pre-Processor for static analysis\n");

  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram PSTP(argc, argv);
  llvm::EnableDebugBuffering = true;

  std::error_code error_code;
  llvm::SMDiagnostic err;
  llvm::LLVMContext &context = llvm::getGlobalContext();
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::tool_output_file> output;
  std::unique_ptr<llvm::tool_output_file> asmOutput;
  
  module = llvm::parseIRFile(InputFilename, err, context);
  if (module.get() == 0)
  {
    if (llvm::errs().has_colors())
      llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: "
                 << "Bitcode was not properly read; " << err.getMessage() << "\n";
    if (llvm::errs().has_colors()) llvm::errs().resetColor();
    return 3;
  }

  if (!AsmOutputFilename.empty ())
    asmOutput = 
      llvm::make_unique<llvm::tool_output_file>
      (AsmOutputFilename.c_str(), error_code, llvm::sys::fs::F_Text);
       
  if (error_code) {
    if (llvm::errs().has_colors()) 
      llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: Could not open " << AsmOutputFilename << ": " 
                 << error_code.message () << "\n";
    if (llvm::errs().has_colors()) llvm::errs().resetColor();
    return 3;
  }

  if (!OutputFilename.empty ())
    output = llvm::make_unique<llvm::tool_output_file>
      (OutputFilename.c_str(), error_code, llvm::sys::fs::F_None);
      
  if (error_code) {
    if (llvm::errs().has_colors())
      llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: Could not open " << OutputFilename << ": " 
                 << error_code.message () << "\n";
    if (llvm::errs().has_colors()) llvm::errs().resetColor();
    return 3;
  }

  
  ///////////////////////////////
  // initialise and run passes //
  ///////////////////////////////

  llvm::legacy::PassManager pass_manager;
  llvm::PassRegistry &Registry = *llvm::PassRegistry::getPassRegistry();
  llvm::initializeAnalysis(Registry);
  
  /// call graph and other IPA passes
  //llvm::initializeIPA (Registry);
  // XXX: porting to 3.8 
  llvm::initializeCallGraphWrapperPassPass(Registry);
  llvm::initializeCallGraphPrinterPass(Registry);
  llvm::initializeCallGraphViewerPass(Registry);
  // XXX: not sure if needed anymore
  llvm::initializeGlobalsAAWrapperPassPass(Registry);  
    
  // add an appropriate DataLayout instance for the module
  const llvm::DataLayout *dl = &module->getDataLayout ();
  if (!dl && !DefaultDataLayout.empty ())
  {
    module->setDataLayout (DefaultDataLayout);
    dl = &module->getDataLayout ();
  }

  assert (dl && "Could not find Data Layout for the module");

  // -- promote top-level mallocs to alloca
  pass_manager.add (crab_llvm::createPromoteMallocPass ());  

  // -- turn all functions internal so that we can apply some global
  // -- optimizations inline them if requested
  pass_manager.add (llvm::createInternalizePass (llvm::ArrayRef<const char*>("main")));

  if (Devirtualize) {
    // -- resolve indirect calls
    pass_manager.add (crab_llvm::createDevirtualizeFunctionsPass ());
  }
  
  if (ExternalizeAddrTakenFuncs) {
    // -- externalize uses of address-taken functions
    pass_manager.add (crab_llvm::createExternalizeAddressTakenFunctionsPass ());
  }
  
  // kill unused internal global    
  pass_manager.add (llvm::createGlobalDCEPass ()); 
  pass_manager.add (crab_llvm::createRemoveUnreachableBlocksPass ());
  // -- global optimizations
  pass_manager.add (llvm::createGlobalOptimizerPass());

  if (LowerGv) {
    // -- lower initializers of global variables
    pass_manager.add (crab_llvm::createLowerGvInitializersPass ());   
  }

  // -- SSA
  pass_manager.add(llvm::createPromoteMemoryToRegisterPass());
  #ifdef HAVE_LLVM_SEAHORN
  if (TurnUndefNondet) {
    // -- Turn undef into nondet
    pass_manager.add (llvm_seahorn::createNondetInitPass ());
  }
  #endif 

  // -- cleanup after SSA
  #ifdef HAVE_LLVM_SEAHORN
  pass_manager.add (llvm_seahorn::createInstructionCombiningPass ());
  #endif 
  pass_manager.add (llvm::createCFGSimplificationPass ());
  break_allocas(pass_manager);
  // // -- break aggregates
  // pass_manager.add (llvm::createScalarReplAggregatesPass
  // 		    (SROA_Threshold,
  // 		     true,
  // 		     SROA_StructMemThreshold,
  // 		     SROA_ArrayElementThreshold,
  // 		     SROA_ScalarLoadThreshold));
  
  // #ifdef HAVE_LLVM_SEAHORN
  // if (TurnUndefNondet) {
  //    // -- Turn undef into nondet (undef are created by SROA when it calls mem2reg)
  //    pass_manager.add (llvm_seahorn::createNondetInitPass ());
  // }
  // #endif

  // -- global value numbering and redundant load elimination
  pass_manager.add (llvm::createGVNPass());
  
  // -- cleanup after break aggregates
  #ifdef HAVE_LLVM_SEAHORN
  pass_manager.add (llvm_seahorn::createInstructionCombiningPass ());
  #endif 
  pass_manager.add (llvm::createCFGSimplificationPass ());
  
  #ifdef HAVE_LLVM_SEAHORN
  if (TurnUndefNondet) {
     // eliminate unused calls to verifier.nondet() functions
     pass_manager.add (llvm_seahorn::createDeadNondetElimPass ());
  }
  #endif 

  // -- lower invoke's
  pass_manager.add(llvm::createLowerInvokePass());
  // cleanup after lowering invoke's
  pass_manager.add (llvm::createCFGSimplificationPass ());  
  
  if (InlineAll) {
    pass_manager.add (crab_llvm::createMarkInternalInlinePass ());   
    pass_manager.add (llvm::createAlwaysInlinerPass ());
    // // after inlining we promote malloc to alloca instructions
    // pass_manager.add (crab_llvm::createPromoteMallocPass ());    
    // // kill unused internal global    
    // pass_manager.add (llvm::createGlobalDCEPass ());
    pass_manager.add (llvm::createGlobalDCEPass ()); // kill unused internal global
    // -- promote malloc to alloca
    pass_manager.add (crab_llvm::createPromoteMallocPass ());
    pass_manager.add (llvm::createGlobalDCEPass ()); // kill unused internal global
    // XXX: for svcomp ssh programs we need to run twice to break all
    // relevant allocas
    break_allocas(pass_manager);
    break_allocas(pass_manager);
  }
  
  pass_manager.add (crab_llvm::createRemoveUnreachableBlocksPass ());
  pass_manager.add(llvm::createDeadInstEliminationPass());
  
  if (OptimizeLoops) {
    // canonical form for loops
    pass_manager.add (llvm::createLoopSimplifyPass());
    // cleanup unnecessary blocks     
    pass_manager.add (llvm::createCFGSimplificationPass ());  
    // loop-closed SSA 
    pass_manager.add (llvm::createLCSSAPass());
    #ifdef HAVE_LLVM_SEAHORN
    // induction variable
    pass_manager.add (llvm_seahorn::createIndVarSimplifyPass ());
    #endif 
  }

  // trivial invariants outside loops 
  pass_manager.add (llvm::createBasicAAWrapperPass());
  pass_manager.add (llvm::createLICMPass()); //LICM needs alias analysis
  pass_manager.add (llvm::createPromoteMemoryToRegisterPass());
  // dead loop elimination
  pass_manager.add (llvm::createLoopDeletionPass());
  // cleanup unnecessary blocks   
  pass_manager.add (llvm::createCFGSimplificationPass ()); 
  
  // -- ensure one single exit point per function
  pass_manager.add (llvm::createUnifyFunctionExitNodesPass ());
  pass_manager.add (llvm::createGlobalDCEPass ()); 
  pass_manager.add (llvm::createDeadCodeEliminationPass());
  // -- remove unreachable blocks also dead cycles
  pass_manager.add (crab_llvm::createRemoveUnreachableBlocksPass ());

  // -- remove switch constructions
  pass_manager.add (llvm::createLowerSwitchPass());
  // cleanup unnecessary blocks     
  pass_manager.add (llvm::createCFGSimplificationPass ());  
  
  // -- lower constant expressions to instructions
  pass_manager.add (crab_llvm::createLowerCstExprPass ());   
  pass_manager.add (llvm::createDeadCodeEliminationPass());

  // -- lower ULT and ULE instructions  
  if (LowerUnsignedICmp) {
    pass_manager.add (crab_llvm::createLowerUnsignedICmpPass ());   
    // cleanup unnecessary and unreachable blocks   
    pass_manager.add (llvm::createCFGSimplificationPass ());
    pass_manager.add (crab_llvm::createRemoveUnreachableBlocksPass ());
  }
  
  // -- must be the last one to avoid llvm undoing it
  if (LowerSelect)
    pass_manager.add (crab_llvm::createLowerSelectPass ());   

  if (!AsmOutputFilename.empty ()) 
    pass_manager.add (createPrintModulePass (asmOutput->os ()));
      
  if (!OutputFilename.empty ())  {
    if (OutputAssembly)
      pass_manager.add (createPrintModulePass (output->os ()));
    else 
      pass_manager.add (createBitcodeWriterPass (output->os ()));
  }
  
  pass_manager.run(*module.get());

  if (!AsmOutputFilename.empty ()) asmOutput->keep ();
  if (!OutputFilename.empty ()) output->keep();
  
  return 0;
}
