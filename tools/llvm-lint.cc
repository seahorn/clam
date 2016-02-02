///
// llvm-lint -- Check for common errors in LLVM bitecode
///

#include "llvm/PassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Analysis/Lint.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::Positional, llvm::cl::desc("<input LLVM bitcode file>"),
              llvm::cl::Required, llvm::cl::value_desc("filename"));

int main(int argc, char **argv) {

  llvm::llvm_shutdown_obj shutdown;  
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "A lint-like tool for LLVM bitecode");

  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram PSTP(argc, argv);
  llvm::EnableDebugBuffering = true;

  std::error_code error_code;
  llvm::SMDiagnostic err;
  llvm::LLVMContext &context = llvm::getGlobalContext();
  std::unique_ptr<llvm::Module> module;
  
  module = llvm::parseIRFile(InputFilename, err, context);
  if (module.get() == 0) {
    if (llvm::errs().has_colors()) llvm::errs().changeColor(llvm::raw_ostream::RED);
    llvm::errs() << "error: "
                 << "Bitcode was not properly read; " << err.getMessage() << "\n";
    if (llvm::errs().has_colors()) llvm::errs().resetColor();
    return 3;
  }
  
  llvm::PassManager pass_manager;
  pass_manager.add (llvm::createLintPass ());
  pass_manager.run(*module.get());

  return 0;
}
