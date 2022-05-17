#include "clam/Support/NameValues.hh"

#include "clam/Support/Boost.hh"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/tokenizer.hpp>

using namespace llvm;

cl::opt<bool> UseCrabNameValues(
    "use-crab-name-values",
    cl::desc("Use own crab way of naming values, otherwise LLVM instnamer"),
    cl::init(true));

namespace clam {

char NameValues::ID = 0;

bool NameValues::runOnModule(Module &M) {
  for (Module::iterator FI = M.begin(), E = M.end(); FI != E; ++FI)
    runOnFunction(*FI);
  return false;
}

bool NameValues::runOnFunction(Function &F) {
  if (UseCrabNameValues) {
    // -- print to string
    std::string funcAsm;
    raw_string_ostream out(funcAsm);
    out << F;
    out.flush();

    typedef boost::tokenizer<boost::char_separator<char>> tokenizer;
    boost::char_separator<char> nl_sep("\n");
    boost::char_separator<char> sp_sep(" :\t%@");

    tokenizer lines(funcAsm, nl_sep);
    tokenizer::iterator line_iter = lines.begin();

    // -- skip function attributes
    if (boost::starts_with(*line_iter, "; Function Attrs:"))
      ++line_iter;

    // Function parameters can be unnamed
    for (auto &Arg : F.args()) {
      if (!Arg.hasName()) {
        Arg.setName("arg");
      }
    }

    // -- skip function definition line
    ++line_iter;

    for (Function::iterator BI = F.begin(), BE = F.end();
         BI != BE && line_iter != lines.end(); ++BI) {
      BasicBlock &BB = *BI;

      if (!BB.hasName()) {
        std::string bb_line = *line_iter;
        tokenizer names(bb_line, sp_sep);
        std::string bb_name = *names.begin();
        if (bb_name == ";")
          bb_name = "un";
        BB.setName("_" + bb_name);
      }
      ++line_iter;

      for (BasicBlock::iterator II = BB.begin(), IE = BB.end();
           II != IE && line_iter != lines.end(); ++II) {
        Instruction &I = *II;
        if (!I.hasName() && !(I.getType()->isVoidTy())) {
          std::string inst_line = *line_iter;
          tokenizer names(inst_line, sp_sep);
          std::string inst_name = *names.begin();
          I.setName("_" + inst_name);
        }
        ++line_iter;
      }
    }
  } else {
    // LLVM InstructionNamer (much faster)

    for (auto &Arg : F.args()) {
      if (!Arg.hasName()) {
        Arg.setName("arg");
      }
    }

    for (BasicBlock &BB : F) {
      if (!BB.hasName()) {
        BB.setName("bb");
      }

      for (Instruction &I : BB) {
        if (!I.hasName() && !I.getType()->isVoidTy()) {
          I.setName("tmp");
        }
      }
    }
  }

  return true;
}

void NameValues::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
  AU.setPreservesAll();
}

} // namespace clam

static llvm::RegisterPass<clam::NameValues> Y("crab-name-values",
                                              "Names all unnamed values");
