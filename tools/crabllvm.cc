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
#ifdef HAVE_DSA
#include "assistDS/Devirt.h"
#endif 
#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Transforms/Scalar.h"
#endif 

#include <Transforms/LowerGvInitializers.hh>
#include <Transforms/NameValues.hh>
#include <Transforms/MarkInternalInline.hh>
#include <Transforms/LowerCstExpr.hh>
#include <Transforms/LowerSelect.hh>
#include <Transforms/RemoveUnreachableBlocksPass.hh>

#include "crab_llvm/CrabLlvm.hh"
#include <crab_llvm/Transforms/InsertInvariants.hh>
#include "crab_llvm/ConCrabLlvm.hh"

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
Concurrency ("crab-concur", llvm::cl::desc ("Analysis of concurrent programs"),
           llvm::cl::init (false));

static llvm::cl::opt<bool>
InsertInvs ("crab-insert-invariants", 
              llvm::cl::desc ("Instrument code with invariants"),
              llvm::cl::init (false));

static llvm::cl::opt<bool>
CrabDevirtualize ("crab-devirt", llvm::cl::desc ("Resolve indirect calls"),
                    llvm::cl::init (false));

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

  // -- promote alloca's to registers
  pass_manager.add (llvm::createPromoteMemoryToRegisterPass());

#ifdef HAVE_DSA
  if (CrabDevirtualize) {
    // -- resolve indirect calls
    pass_manager.add (new llvm::Devirtualize ());
  }
#endif 

  // -- ensure one single exit point per function
  pass_manager.add (llvm::createUnifyFunctionExitNodesPass ());
  // -- remove unreachable blocks 
  pass_manager.add (new crab_llvm::RemoveUnreachableBlocksPass ());
  // -- remove switch constructions
  pass_manager.add (llvm::createLowerSwitchPass());
  // -- lower constant expressions to instructions
  pass_manager.add (new crab_llvm::LowerCstExprPass ());   
  pass_manager.add (llvm::createDeadCodeEliminationPass());

  // -- must be the last ones before running crab.
  //    It is not a must anymore since Crab can handle select
  //    instructions. However, Crab can be more precise if select are
  //    lowered. llvmpp has the option to lower select instructions.
  //pass_manager.add (new crab_llvm::LowerSelect ());   
  pass_manager.add (new crab_llvm::NameValues ()); 
  
  if (Concurrency) {
    // TOO EXPERIMENTAL
    pass_manager.add (new crab_llvm::ConCrabLlvm ());
  }
  else {
    /// -- run the crab analyzer
    pass_manager.add (new crab_llvm::CrabLlvm ());
    if (InsertInvs) {
      pass_manager.add (new crab_llvm::InsertInvariants ());
      // -- simplify invariants added in the bytecode.
#ifdef HAVE_LLVM_SEAHORN
      pass_manager.add (llvm_seahorn::createInstructionCombiningPass ());      
#else
      pass_manager.add (llvm::createInstructionCombiningPass ()); 
#endif 
    }
  }

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
