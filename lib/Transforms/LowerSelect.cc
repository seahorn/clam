/* Remove select instructions */

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"

#include <vector>

//#define DEBUG_TYPE "lower-selects"

namespace clam {
using namespace llvm;

// STATISTIC(totalLowered, "Number of Lowered Select Instructions");

class LowerSelect : public FunctionPass {
  // Lower the select instruction into three new blocks.
  void processSelectInst(SelectInst *SI) {

    BasicBlock *curBlk = SI->getParent();
    Function *F = curBlk->getParent();
    Value *Flag = SI->getCondition();

    /// This splits a basic block into two at the specified instruction.
    /// All instructions BEFORE the specified iterator stay as part of
    /// the original basic block, an unconditional branch is added to
    /// the original BB, and the rest of the instructions in the BB are
    /// moved to the new BB, including the old terminator.
    /// IMPORTANT: this function invalidates the specified iterator.
    /// IMPORTANT: note that the select instructions goes to afterSelect
    /// Also note that this doesn't preserve any passes. To split blocks
    /// while keeping loop information consistent, use the SplitBlock
    /// utility function.
    BasicBlock *afterSelect =
        curBlk->splitBasicBlock(SI, curBlk->getName() + "LowerSelect");

    BasicBlock *trueBlock =
        BasicBlock::Create(F->getContext(), "TrueLowerSelect", F, afterSelect);
    BasicBlock *falseBlock =
        BasicBlock::Create(F->getContext(), "FalseLowerSelect", F, afterSelect);

    /// Wire trueBlock and falseblock to afterSelect via unconditional
    /// branch
    BranchInst::Create(afterSelect, trueBlock);
    BranchInst::Create(afterSelect, falseBlock);

    /// Replace the the unconditional branch added by splitBasicBlock
    /// with a conditional branch splitting on Flag **at the end** of
    /// curBlk
    curBlk->getTerminator()->eraseFromParent();
    BranchInst::Create(trueBlock, falseBlock, Flag, curBlk);

    // Insert a phi node just before the select instruction.
    PHINode *PHI =
        PHINode::Create(SI->getOperand(1)->getType(), 0, "PHILowerSelect", SI);

    PHI->addIncoming(SI->getOperand(1), trueBlock);
    PHI->addIncoming(SI->getOperand(2), falseBlock);

    // Make sure any users of the select is now an user of the phi node.
    SI->replaceAllUsesWith(PHI);

    // Finally we remove the select instruction
    SI->eraseFromParent();

    // totalLowered++;
  }

public:
  static char ID;

  LowerSelect() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function &F) override {
    bool modified = false;

    std::vector<SelectInst *> worklist;
    // Initialization of the worklist with all select instructions from
    // the function
    for (inst_iterator It = inst_begin(F), E = inst_end(F); It != E; ++It) {
      Instruction *inst = &*It;
      if (SelectInst *SI = dyn_cast<SelectInst>(inst)) {
        if (SI->getCondition()->getType()->isIntegerTy(1)) {
          // we ignore vector operations
          worklist.push_back(SI);
        }
      }
    }

    while (!worklist.empty()) {
      modified = true;
      SelectInst *SI = worklist.back();
      worklist.pop_back();
      processSelectInst(SI);
    }

    return modified;
  }

  virtual StringRef getPassName() const override  {
    return "Clam: Lower select instructions";
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override  {
    // AU.setPreservesAll ();
  }
};

char LowerSelect::ID = 0;
Pass *createLowerSelectPass() { return new LowerSelect(); }

} // namespace clam
