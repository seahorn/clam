#define DEBUG_TYPE "lower-selects"
#include "Transforms/LowerSelect.hh"
#include <vector>

namespace llvm_ikos 
{

using namespace llvm;

STATISTIC(totalLowered, "Number of Lowered Select Instructions");

// Identifier for the pass
char LowerSelect::ID = 0;

bool LowerSelect::runOnFunction(Function & F)
{
  
  std::vector<SelectInst *> worklist;
  bool modified=false;
  // Initialization of the worklist with all select instructions from
  // the function
  for (inst_iterator It = inst_begin(F), E = inst_end(F); It != E; ++It)
  {
    Instruction *inst = &*It;
    if (SelectInst * SI = dyn_cast<SelectInst>(inst)) 
    {
      if (!(SI->getCondition()->getType()->isIntegerTy(1)))
      {
        dbgs () << "We only lower a select if the flag is Boolean.\n";
        // note that the flag can be a vector of Boolean
        assert(false);
      }
      worklist.push_back(SI);
    }
  } 
    
  while (!worklist.empty()) 
  {
    modified=true;
    SelectInst * SI = worklist.back();
    worklist.pop_back();
    processSelectInst(SI);
  }
  
  return modified;
}

// Lower the select instruction into three new blocks.
void LowerSelect::processSelectInst(SelectInst *SI)
{

  BasicBlock *curBlk = SI->getParent();
  Function   *F      = curBlk->getParent();
  Value *Flag        = SI->getCondition();  
  
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
  BasicBlock * afterSelect = curBlk->splitBasicBlock (SI, 
                                                      curBlk->getName() + "LowerSelect"); 
  
  BasicBlock* trueBlock  = BasicBlock::Create (F->getContext(),
                                               "TrueLowerSelect",F,afterSelect);
  BasicBlock* falseBlock = BasicBlock::Create (F->getContext(),
                                               "FalseLowerSelect",F,afterSelect);
  
  /// Wire trueBlock and falseblock to afterSelect via unconditional
  /// branch
  BranchInst::Create (afterSelect,trueBlock);
  BranchInst::Create (afterSelect,falseBlock);
  
  /// Replace the the unconditional branch added by splitBasicBlock
  /// with a conditional branch splitting on Flag **at the end** of
  /// curBlk
  curBlk->getTerminator()->eraseFromParent();
  BranchInst::Create (trueBlock, falseBlock, Flag, curBlk);
  
  // Insert a phi node just before the select instruction.
  PHINode *PHI=PHINode::Create (SI->getOperand(1)->getType(), 
                                0,
                                "PHILowerSelect", 
                                SI);

  PHI->addIncoming (SI->getOperand(1),trueBlock);
  PHI->addIncoming (SI->getOperand(2),falseBlock);
  
  // Make sure any users of the select is now an user of the phi node.
  SI->replaceAllUsesWith(PHI);

  // Finally we remove the select instruction
  SI->eraseFromParent();
  
  totalLowered++;
}

} // end namespace

// Registration of the pass
static llvm::RegisterPass<llvm_ikos::LowerSelect> 
X ("lowerselect", "Lower select instructions",
   false, false);
