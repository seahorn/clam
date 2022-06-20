#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"

namespace clam {

using namespace llvm;

class LowerCstExpr : public ModulePass {

  ConstantExpr *hasCstExpr(Value *V) {
    // We only handle top-level constant expressions ignoring
    // constant subexpressions
    if (Constant *cst = dyn_cast<Constant>(V)) {
      if (ConstantExpr *ce = dyn_cast<ConstantExpr>(cst)) {
        return ce;
      }
    }
    return nullptr;
  }

  Instruction *lowerCstExpr(ConstantExpr *CstExp, Instruction *InsertionLoc) {
    Instruction *NewI = CstExp->getAsInstruction();
    // insert before
    InsertionLoc->getParent()->getInstList().insert(InsertionLoc->getIterator(),
                                                    NewI);
    return NewI;
  }

  bool runOnFunction(Function &F) {
    SmallPtrSet<Instruction *, 8> worklist;
    for (inst_iterator It = inst_begin(F), E = inst_end(F); It != E; ++It) {
      Instruction *I = &*It;
      for (unsigned int i = 0; i < I->getNumOperands(); ++i) {
        if (hasCstExpr(I->getOperand(i)))
          worklist.insert(I);
      }
    }

    bool change = !worklist.empty();
    while (!worklist.empty()) {
      auto It = worklist.begin();
      Instruction *I = *It;
      worklist.erase(*It);

      if (PHINode *PHI = dyn_cast<PHINode>(I)) {
        for (unsigned int i = 0; i < PHI->getNumIncomingValues(); ++i) {
          Instruction *InsertLoc = PHI->getIncomingBlock(i)->getTerminator();
          assert(InsertLoc);
          if (ConstantExpr *CstExp = hasCstExpr(PHI->getIncomingValue(i))) {
            // skip if CstExp is not the same as incoming PHI value
            if (CstExp != PHI->getIncomingValue(i))
              continue;
            Instruction *NewInst = lowerCstExpr(CstExp, InsertLoc);
            for (unsigned int j = PHI->getNumIncomingValues(); j > i; --j) {
              if ((PHI->getIncomingValue(j - 1) == PHI->getIncomingValue(i)) &&
                  (PHI->getIncomingBlock(j - 1) == PHI->getIncomingBlock(i))) {
                PHI->setIncomingValue(j - 1, NewInst);
              }
            }
            worklist.insert(NewInst);
          }
        }
      } else {
        for (unsigned int i = 0; i < I->getNumOperands(); ++i) {
          if (ConstantExpr *CstExp = hasCstExpr(I->getOperand(i))) {
            Instruction *NewInst = lowerCstExpr(CstExp, I);
            I->replaceUsesOfWith(CstExp, NewInst);
            worklist.insert(NewInst);
          }
        }
      }
    }
    return change;
  }

public:
  static char ID;

  LowerCstExpr() : ModulePass(ID) {}

  virtual bool runOnModule(Module &M) override {
    bool change = false;
    for (auto &F : M) {
      change |= runOnFunction(F);
    }
    return change;
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    // AU.setPreservesAll ();
  }

  virtual StringRef getPassName() const override {
    return "Clam: Lower constant expressions";
  }
};

char LowerCstExpr::ID = 0;
Pass *createLowerCstExprPass() { return new LowerCstExpr(); }

} // namespace clam
