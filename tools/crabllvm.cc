///
// CrabLlvm-- Abstract Interpretation-based Analyzer for LLVM bitcode 
///

#include "llvm/LinkAllPasses.h"
#include "llvm/PassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/IR/Verifier.h"
#include "crab_llvm/config.h"

#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Transforms/Scalar.h"
#endif 

#include "crab_llvm/Passes.hh"
#include "crab_llvm/CrabLlvm.hh"
#include <crab_llvm/Transforms/InsertInvariants.hh>

#include "crab/common/debug.hpp"

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
NoCrab ("no-crab", 
        llvm::cl::desc ("Output bitecode but disable Crab analysis (debugging)"),
        llvm::cl::init (false),
        llvm::cl::Hidden);

static llvm::cl::opt<bool>
TurnUndefNondet ("crab-turn-undef-nondet", 
                 llvm::cl::desc ("Turn undefined behaviour into non-determinism"),
                 llvm::cl::init (false),
                 llvm::cl::Hidden);

static llvm::cl::opt<bool>
LowerSelect ("crab-lower-select", 
             llvm::cl::desc ("Lower all select instructions"),
             llvm::cl::init (false));


struct LogOpt {
  void operator= (const std::string &tag) const 
  { crab::CrabEnableLog (tag); } 
};

LogOpt loc;

static llvm::cl::opt<LogOpt, true, llvm::cl::parser<std::string> > 
LogClOption ("log",
             llvm::cl::desc ("Enable specified log level"),
             llvm::cl::location (loc),
             llvm::cl::value_desc ("string"),
             llvm::cl::ValueRequired, llvm::cl::ZeroOrMore);

using namespace crab_llvm;

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
                                    "CrabLlvm-- Abstract Interpretation-based Analyzer of LLVM bitcode\n");

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
  if (!module)
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

  /////
  // Here only passes that are necessary or very recommended for Crab
  /////

  // -- turn all functions internal so that we can use DSA
  pass_manager.add (llvm::createInternalizePass (llvm::ArrayRef<const char*>("main")));
  pass_manager.add (llvm::createGlobalDCEPass ()); // kill unused internal global  
  pass_manager.add (crab_llvm::createRemoveUnreachableBlocksPass ());

  // -- promote alloca's to registers
  pass_manager.add (llvm::createPromoteMemoryToRegisterPass());
  // -- ensure one single exit point per function
  pass_manager.add (llvm::createUnifyFunctionExitNodesPass ());
  // -- remove unreachable blocks 
  pass_manager.add (crab_llvm::createRemoveUnreachableBlocksPass ());
  // -- remove switch constructions
  pass_manager.add (llvm::createLowerSwitchPass());
  // -- lower constant expressions to instructions
  pass_manager.add (crab_llvm::createLowerCstExprPass ());   
  pass_manager.add (llvm::createDeadCodeEliminationPass());

  #ifdef HAVE_LLVM_SEAHORN
  if (TurnUndefNondet) 
    pass_manager.add (llvm_seahorn::createDeadNondetElimPass ());
  #endif 

  // -- must be the last ones before running crab.
  if (LowerSelect)
    pass_manager.add (crab_llvm::createLowerSelectPass ());   

  if (!NoCrab) {
    /// -- run the crab analyzer
    pass_manager.add (new crab_llvm::CrabLlvm ());
  }

  if (!AsmOutputFilename.empty ()) 
    pass_manager.add (createPrintModulePass (asmOutput->os ()));
 
  if (!NoCrab) {
    /// -- insert invariants as assume instructions
    pass_manager.add (new crab_llvm::InsertInvariants ());
    /// -- simplify invariants added in the bytecode.
    #ifdef HAVE_LLVM_SEAHORN
    pass_manager.add (llvm_seahorn::createInstructionCombiningPass ());      
    #else
    pass_manager.add (llvm::createInstructionCombiningPass ()); 
    #endif 
    pass_manager.add (crab_llvm::createSimplifyAssumePass ());
  }
      
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
