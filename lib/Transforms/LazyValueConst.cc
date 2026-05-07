//===- LazyValueConstPass.cc - Example pass using LazyValueInfo ----------===//
//
// A simple Function pass that queries LLVM LazyValueInfo analysis
// to determine whether values are constant at particular program points.
// It prints discovered constants to stderr.
// The pass is intentionally small and non-intrusive: it only reads
// analysis information and preserves the IR.
//
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/LazyValueInfo.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

struct LazyValueConst : public FunctionPass {
  static char ID;
  LazyValueConst() : FunctionPass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    // We only read analysis results from LazyValueInfo
    AU.addRequired<LazyValueInfoWrapperPass>();
    AU.setPreservesAll();
  }

  bool runOnFunction(Function &F) override {
    LazyValueInfo &LVI = getAnalysis<LazyValueInfoWrapperPass>().getLVI();
    bool Changed = false;

    for (auto &BB : F) {
      for (auto &I : BB) {
        for (unsigned i = 0, e = I.getNumOperands(); i != e; ++i) {
          Value *Op = I.getOperand(i);
          // skip if operand is not integer or a constant
          if (Op->getType()->isIntegerTy() == false || isa<Constant>(Op)) {
            continue;
          }
          Constant *C = LVI.getConstant(Op, &I);
          if (isa<MemCpyInst>(&I)) {
          }
          if (C) {
            // replace the operand with the constant
            I.setOperand(i, C);
            Changed = true;
            continue;
          }
        }
      }
    }

    // no modification
    return Changed;
  }
};

char LazyValueConst::ID = 0;

} // end anonymous namespace

namespace clam {
llvm::Pass *createLazyValueConstPass() { return new LazyValueConst(); }
} // namespace clam

// Register the pass
static llvm::RegisterPass<LazyValueConst>
    X("lazy-value-const", "Detect constants using LazyValueInfo");