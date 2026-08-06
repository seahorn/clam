#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Pass.h"

#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/raw_ostream.h"

#include "clam/NewPmPasses.hh"

namespace {

using namespace llvm;

bool promoteMalloc(Function &F) {
  if (F.empty())
    return false;

  // -- only promote mallocs in top level functions
  if (!F.getName().equals("main"))
    return false;

  bool changed = false;

  SmallVector<Instruction *, 16> kill;

  for (auto &I : llvm::make_range(inst_begin(F), inst_end(F))) {
    if (CallInst *CI = dyn_cast<CallInst>(&I)) {

      CallBase &CB = *CI;
      const Function *fn = CB.getCalledFunction();
      if (!fn && CB.getCalledOperand())
        fn = dyn_cast<const Function>(
            CB.getCalledOperand()->stripPointerCasts());

      if (fn && fn->getName().equals("malloc")) {
        if (I.getType()->isPointerTy()) {
          unsigned addrSpace = 0;
          // malloc's return type is always i8* (an opaque `ptr` under LLVM 15),
          // so the buffer is n bytes: alloca i8, n -- exactly what
          // pty->getPointerElementType() yielded before opaque pointers.
          Value *nv = new AllocaInst(Type::getInt8Ty(I.getContext()), addrSpace,
                                     CB.getArgOperand(0), "malloc", &I);
          I.replaceAllUsesWith(nv);
          changed = true;
        }
      } else if (fn && fn->getName().equals("free"))
        kill.push_back(&I);
    }
  }

  // -- remove all calls to free(). This is too much, but ensures
  // -- that all promoted mallocs() are not free'ed by mistake
  for (auto *I : kill)
    I->eraseFromParent();

  return changed;
}

} // namespace

namespace clam {

using namespace llvm;

class PromoteMalloc : public FunctionPass {

public:
  static char ID;

  PromoteMalloc() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function &F) override { return promoteMalloc(F); }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    // AU.setPreservesAll ();
  }

  virtual StringRef getPassName() const override {
    return "Clam: Promote malloc to alloca instructions";
  }
};

char PromoteMalloc::ID = 0;

Pass *createPromoteMallocPass() { return new PromoteMalloc(); }

PreservedAnalyses PromoteMallocPass::run(Function &F,
                                         FunctionAnalysisManager &) {
  if (!promoteMalloc(F)) {
    return PreservedAnalyses::all();
  }
  // Instructions are added and removed, but no branch is touched.
  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
} // namespace clam

static llvm::RegisterPass<clam::PromoteMalloc>
    X("promote-malloc", "Promote top-level malloc calls to alloca");
