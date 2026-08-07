/* Replace min/max intrinsics with the icmp + select they were folded from */

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"

#include <vector>

#include "clam/NewPmPasses.hh"

namespace clam {
using namespace llvm;

// LLVM 17's InstCombine canonicalizes a select that computes a minimum or a
// maximum into one of llvm.umax/llvm.umin/llvm.smax/llvm.smin. For instance
//
//   %c = icmp eq i32 %x, 0
//   %r = select i1 %c, i32 1, i32 %x
//
// becomes
//
//   %r = call i32 @llvm.umax.i32(i32 %x, i32 1)
//
// CfgBuilder translates no intrinsic outside the arithmetic-with-overflow
// family, so the call falls through to the generic external-call path and the
// result is havoc'ed: every fact about the operands is lost at that point.
// Undoing the canonicalization restores what the analysis can read, since
// CfgBuilder handles icmp and select natively (visitSelectInst), and it lets
// LowerSelectPass go one step further and turn the select into branches and a
// phi node.
//
// This has to run after the last InstCombine of the pipeline, or the select
// it produces is folded straight back into the intrinsic.
class LowerMinMaxIntrinsicsImpl {
  // Lower one intrinsic into a comparison of its operands and a select.
  void processMinMaxInst(MinMaxIntrinsic *MM) {
    IRBuilder<> B(MM);
    Value *lhs = MM->getLHS();
    Value *rhs = MM->getRHS();
    // umax -> ugt, umin -> ult, smax -> sgt, smin -> slt. Picking lhs on the
    // true branch is what makes the select agree with the intrinsic for the
    // ties as well: both operands are equal there, so either one will do.
    Value *cond =
        B.CreateICmp(MM->getPredicate(), lhs, rhs, MM->getName() + ".cmp");
    Value *sel = B.CreateSelect(cond, lhs, rhs, MM->getName() + ".minmax");

    MM->replaceAllUsesWith(sel);
    MM->eraseFromParent();
  }

public:
  bool run(Function &F) {
    bool modified = false;

    std::vector<MinMaxIntrinsic *> worklist;
    // Initialization of the worklist with all min/max intrinsics from the
    // function
    for (inst_iterator It = inst_begin(F), E = inst_end(F); It != E; ++It) {
      Instruction *inst = &*It;
      if (MinMaxIntrinsic *MM = dyn_cast<MinMaxIntrinsic>(inst)) {
        if (MM->getType()->isIntegerTy()) {
          // we ignore vector operations
          worklist.push_back(MM);
        }
      }
    }

    while (!worklist.empty()) {
      modified = true;
      MinMaxIntrinsic *MM = worklist.back();
      worklist.pop_back();
      processMinMaxInst(MM);
    }

    return modified;
  }
};

class LowerMinMaxIntrinsics : public FunctionPass {
public:
  static char ID;

  LowerMinMaxIntrinsics() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function &F) override {
    return LowerMinMaxIntrinsicsImpl().run(F);
  }

  virtual StringRef getPassName() const override {
    return "Clam: Lower min/max intrinsics";
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
  }
};

char LowerMinMaxIntrinsics::ID = 0;
Pass *createLowerMinMaxIntrinsicsPass() { return new LowerMinMaxIntrinsics(); }

PreservedAnalyses
LowerMinMaxIntrinsicsPass::run(Function &F, FunctionAnalysisManager &) {
  if (!LowerMinMaxIntrinsicsImpl().run(F)) {
    return PreservedAnalyses::all();
  }
  // Each lowered intrinsic is replaced in place: no block is added or split.
  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}

} // namespace clam
