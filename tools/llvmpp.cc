///
// LlvmPP-- LLVM bitcode Pre-Processor for static analysis
///

#include "llvm/LinkAllPasses.h"
#include "llvm/PassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
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

#include "llvm/IR/Verifier.h"

#include <Transforms/CrabIndVarSimplify.hh>
#include <Transforms/LowerGvInitializers.hh>
#include <Transforms/NameValues.hh>
#include <Transforms/MarkInternalInline.hh>
#include <Transforms/LowerCstExpr.hh>
#include <Transforms/LowerSelect.hh>
#include <Transforms/RemoveUnreachableBlocksPass.hh>

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::Positional, llvm::cl::desc("<input LLVM bitcode file>"),
              llvm::cl::Required, llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("Override output filename"),
               llvm::cl::init(""), llvm::cl::value_desc("filename"));

static llvm::cl::opt<bool>
OutputAssembly("S", llvm::cl::desc("Write output as LLVM assembly"));

static llvm::cl::opt<std::string>
AsmOutputFilename("oll", llvm::cl::desc("Output analyzed bitcode"),
               llvm::cl::init(""), llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string>
DefaultDataLayout("default-data-layout",
                  llvm::cl::desc("data layout string to use if not specified by module"),
                  llvm::cl::init(""), llvm::cl::value_desc("layout-string"));

static llvm::cl::opt<bool>
InlineAll ("crab-inline-all", llvm::cl::desc ("Inline all functions"),
           llvm::cl::init (false));

static llvm::cl::opt<bool>
CrabLowerSelect ("crab-lower-select", llvm::cl::desc ("Lower all select instructions"),
           llvm::cl::init (false));

static llvm::cl::opt<bool>
Concurrency ("crab-concur", llvm::cl::desc ("Preprocessing for concurrent analysis"),
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

int main(int argc, char **argv) {
  llvm::llvm_shutdown_obj shutdown;  // calls llvm_shutdown() on exit
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "llvmpp-- LLVM bitcode Pre-Processor for static analysis\n");

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
    if (llvm::errs().has_colors()) llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: "
                 << "Bitcode was not properly read; " << err.getMessage() << "\n";
    if (llvm::errs().has_colors()) llvm::errs().resetColor();
    return 3;
  }

  if (!AsmOutputFilename.empty ())
    asmOutput = 
      llvm::make_unique<llvm::tool_output_file>(AsmOutputFilename.c_str(), error_code, 
                                                llvm::sys::fs::F_Text);
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
    if (llvm::errs().has_colors()) llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: Could not open " << OutputFilename << ": " 
                 << error_code.message () << "\n";
    if (llvm::errs().has_colors()) llvm::errs().resetColor();
    return 3;
  }

  
  ///////////////////////////////
  // initialise and run passes //
  ///////////////////////////////

  llvm::PassManager pass_manager;
  llvm::PassRegistry &Registry = *llvm::PassRegistry::getPassRegistry();
  llvm::initializeAnalysis(Registry);
  
  /// call graph and other IPA passes
  llvm::initializeIPA (Registry);
  
  // add an appropriate DataLayout instance for the module
  const llvm::DataLayout *dl = module->getDataLayout ();
  if (!dl && !DefaultDataLayout.empty ())
  {
    module->setDataLayout (DefaultDataLayout);
    dl = module->getDataLayout ();
  }
  if (dl) pass_manager.add (new DataLayoutPass ());
  
  // -- turn all functions internal so that we can apply some global
  // -- optimizations inline them if requested
  pass_manager.add (llvm::createInternalizePass (llvm::ArrayRef<const char*>("main")));
  pass_manager.add (llvm::createGlobalDCEPass ()); // kill unused internal global  
  pass_manager.add (new crab_llvm::RemoveUnreachableBlocksPass ());
  // -- global optimizations
  pass_manager.add (llvm::createGlobalOptimizerPass());
  
  // -- SSA
  pass_manager.add(llvm::createPromoteMemoryToRegisterPass());
  // -- cleanup after SSA
  //pass_manager.add (llvm::createInstructionCombiningPass ()); // bad for static analysis
  pass_manager.add (llvm::createCFGSimplificationPass ());
  
  
  // -- break aggregates
  pass_manager.add (llvm::createScalarReplAggregatesPass (SROA_Threshold,
                                                          true,
                                                          SROA_StructMemThreshold,
                                                          SROA_ArrayElementThreshold,
                                                          SROA_ScalarLoadThreshold));
  // -- global value numbering and redundant load elimination
  pass_manager.add (llvm::createGVNPass());
  
  // -- cleanup after break aggregates
  //pass_manager.add (llvm::createInstructionCombiningPass ());
  pass_manager.add (llvm::createCFGSimplificationPass ());
  
  // -- lower invoke's
  pass_manager.add(llvm::createLowerInvokePass());
  // cleanup after lowering invoke's
  pass_manager.add (llvm::createCFGSimplificationPass ());  
  
  if (InlineAll)
  {
    pass_manager.add (new crab_llvm::MarkInternalInline ());   
    pass_manager.add (llvm::createAlwaysInlinerPass ());
    pass_manager.add (llvm::createGlobalDCEPass ()); // kill unused internal global
  }
  
  pass_manager.add (new crab_llvm::RemoveUnreachableBlocksPass ());
  pass_manager.add(llvm::createDeadInstEliminationPass());
    
  // canonical form for loops
  pass_manager.add (llvm::createLoopSimplifyPass());
  pass_manager.add (llvm::createCFGSimplificationPass ());  // cleanup unnecessary blocks 
  // loop-closed SSA 
  pass_manager.add (llvm::createLCSSAPass());
  // induction variable
  // pass_manager.add (new crab_llvm::CrabIndVarSimplify ());
  // trivial invariants outside loops 
  pass_manager.add (llvm::createBasicAliasAnalysisPass());
  pass_manager.add (llvm::createLICMPass()); //LICM needs alias analysis
  pass_manager.add (llvm::createPromoteMemoryToRegisterPass());
  // dead loop elimination
  pass_manager.add (llvm::createLoopDeletionPass());
  pass_manager.add (llvm::createCFGSimplificationPass ()); // cleanup unnecessary blocks 
  
  // -- lower initializers of global variables
  pass_manager.add (new crab_llvm::LowerGvInitializers ());   

  // -- ensure one single exit point per function
  pass_manager.add (llvm::createUnifyFunctionExitNodesPass ());
  pass_manager.add (llvm::createGlobalDCEPass ()); 
  pass_manager.add (llvm::createDeadCodeEliminationPass());
  // -- remove unreachable blocks also dead cycles
  pass_manager.add (new crab_llvm::RemoveUnreachableBlocksPass ());

  // -- remove switch constructions
  pass_manager.add (llvm::createLowerSwitchPass());
  
  // -- lower constant expressions to instructions
  pass_manager.add (new crab_llvm::LowerCstExprPass ());   
  pass_manager.add (llvm::createDeadCodeEliminationPass());

  // -- must be the last ones:
  if (CrabLowerSelect)
    pass_manager.add (new crab_llvm::LowerSelect ());   
  pass_manager.add (new crab_llvm::NameValues ()); 

  if (!AsmOutputFilename.empty ()) 
    pass_manager.add (createPrintModulePass (asmOutput->os ()));
      
  if (!OutputFilename.empty ()) 
  {
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
