/**
 *  Replace ULT and ULE comparison instructions with SLT and SLE.
 **/

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"

#include <vector>

#define DEBUG_TYPE "lower-unsigned-icmp"

namespace clam {

using namespace llvm;

static CmpInst *mkNonNegative(Value *V, Instruction *insertPt) {
  Value *zero = ConstantInt::getSigned(V->getType(), 0);
  return CmpInst::Create(Instruction::ICmp, CmpInst::ICMP_SGE, V, zero,
			 V->hasName() ? V->getName() + "_SGE_0" : "check_SGE_0",
                         insertPt);
}

static CmpInst *mkNonNegative(Value *V, BasicBlock *insertPt) {
  Value *zero = ConstantInt::getSigned(V->getType(), 0);
  return CmpInst::Create(Instruction::ICmp, CmpInst::ICMP_SGE, V, zero,
			 V->hasName() ? V->getName() + "_SGE_0" : "check_SGE_0",	 
                         insertPt);
}

static bool isNonNegIntCst(Value *V) {
  if (ConstantInt *K = dyn_cast<ConstantInt>(V))
    return (K->getValue().isNonNegative());
  return false;
}
  
static void normalizeCmpInst(CmpInst *I) {
  switch (I->getPredicate()) {
  case ICmpInst::ICMP_UGT:
  case ICmpInst::ICMP_SGT:
    I->swapOperands();
    break;
  case ICmpInst::ICMP_UGE:
  case ICmpInst::ICMP_SGE:
    I->swapOperands();
    break;
  default:;
  }
}

STATISTIC(totalUnsignedICmpLowered,
          "Number of Lowered ULT and ULE Instructions");

class LowerUnsignedICmp : public FunctionPass {

  void processUnsignedICmp(ICmpInst *CI) {
    BasicBlock *cur = CI->getParent();
    BasicBlock *cont =
        cur->splitBasicBlock(CI, cur->getName() + "PHILowerICmp");

    Function *F = cur->getParent();
    Value *op1 = CI->getOperand(0);
    Value *op2 = CI->getOperand(1);

    // op1 is statically known >= 0
    bool isNonNegOp1 = isNonNegIntCst(op1);
    // op2 is statically known >= 0    
    bool isNonNegOp2 = isNonNegIntCst(op2);
    
    if (isNonNegOp2 && isNonNegOp1) {
      // This should not happen after InstCombine
      return;
    }
    
   if (isNonNegOp2 || isNonNegOp1) {
     // Special case: one of the two operands is a constant >=0 
     /**
      *	For the case %y is statically known to be >= 0
      *
      *  Replace %b = %y ult %z with 
      *  
      *  cur:
      *       %b1 = %z geq 0
      *       br %b1, %bb1, %bb2
      *  bb1:
      *       %b2 = %y lt %z
      *       br %cont
      *  bb2:
      *       br %cont
      *  cont:
      *       %b = PHI (%b2, %bb1) (true, %bb2)
      *
      *
      *	For the case %z is statically known to be >= 0
      *
      *  Replace %b = %y ult %z with 
      *  
      *  cur:
      *       %b1 = %y geq 0
      *       br %b1, %bb1, %bb2
      *  bb1:
      *       %b2 = %y lt %z
      *       br %cont
      *  bb2:
      *       br %cont
      *  cont:
      *       %b = PHI (%b2, %bb1) (false, %bb2)
      **/
  
     // Check whether the non-constant operand is >= 0
     BasicBlock *tt =
         BasicBlock::Create(F->getContext(), "TrueLowerICmp", F, cont);
     BasicBlock *ff =
         BasicBlock::Create(F->getContext(), "FalseLowerICmp", F, cont);
     BranchInst::Create(cont, tt);
     BranchInst::Create(cont, ff);
     CmpInst *NonNegOp1 =
         mkNonNegative(isNonNegOp2 ? op1 : op2, cur->getTerminator());
     cur->getTerminator()->eraseFromParent();
     BranchInst::Create(tt, ff, NonNegOp1, cur);

     // Create signed comparison that will replace the unsigned one
     CmpInst *newCI =
         CmpInst::Create(Instruction::ICmp, CI->getSignedPredicate(), op1, op2,
                         CI->getName(), tt->getTerminator());

     // Insert a phi node just before the unsigned instruction in
     // cont
     PHINode *PHI = PHINode::Create(CI->getType(), 0, CI->getName(), CI);
     PHI->addIncoming(newCI, tt);
     if (isNonNegOp2) {
       // %b = %op1 ult 5 and %op1 is negative       
       PHI->addIncoming(ConstantInt::getFalse(newCI->getType()), ff);
     } else {
       // %b = 5 ult %op2 and %op2 is negative
       PHI->addIncoming(ConstantInt::getTrue(newCI->getType()), ff);
     }

     // Make sure any users of the unsigned comparison is now an
     // user of the phi node.
     CI->replaceAllUsesWith(PHI);
     // Finally we remove the unsigned instruction
     CI->eraseFromParent();
   } else {    
     /**
      *  Replace %b = %y ult %z with
      *
      *     cur:
      *          %b1 = %y geq 0
      *          br %b1, %bb1, %bb2
      *     bb1:
      *          %b2 = %z gep 0
      *          br %b2, %bb3, %cont
      *     bb2:
      *          %b3 = %z gep 0
      *          br %b3, %cont, %bb4
      *     bb3:
      *          %b4 = %y lt %z
      *          br %cont
      *     bb4:
      *          %b5 = %y lt %z
      *          br %cont
      *     cont:
      *          %b = PHI (%b4, %bb3) (true, %bb1) (false, %bb2) (%b5, %bb4)
      **/
     
     // Check whether the first operand is >= 0
     BasicBlock *bb1 =
       BasicBlock::Create(F->getContext(), "TrueLowerICmp", F, cont);
     BasicBlock *bb2 =
       BasicBlock::Create(F->getContext(), "FalseLowerICmp", F, cont);
     BasicBlock *bb3 =
       BasicBlock::Create(F->getContext(), "TrueLowerICmp", F, cont);
     BasicBlock *bb4 =
       BasicBlock::Create(F->getContext(), "FalseLowerICmp", F, cont);
     
     CmpInst *b1 = mkNonNegative(op1, cur->getTerminator());
     cur->getTerminator()->eraseFromParent();
     BranchInst::Create(bb1, bb2, b1, cur);
     
     // Check whether the second operand is >= 0
     CmpInst *b2 = mkNonNegative(op2, bb1);
     BranchInst::Create(bb3, cont, b2, bb1);
     
     // Check whether the second operand is >= 0
     CmpInst *b3 = mkNonNegative(op2, bb2);
     BranchInst::Create(cont, bb4, b3, bb2);
     
     // Create signed comparison that will replace the unsigned one
     CmpInst *b4 = CmpInst::Create(Instruction::ICmp, CI->getSignedPredicate(),
				   op1, op2, CI->getName(), bb3);
     BranchInst::Create(cont, bb3);
     
     // Create signed comparison that will replace the unsigned one
     CmpInst *b5 = CmpInst::Create(Instruction::ICmp, CI->getSignedPredicate(),
				   op1, op2, CI->getName(), bb4);
     BranchInst::Create(cont, bb4);
     
     // Insert a phi node just before the unsigned instruction in
     // cont
     PHINode *PHI = PHINode::Create(CI->getType(), 0, CI->getName(), CI);
     PHI->addIncoming(ConstantInt::getTrue(CI->getType()), bb1);
     PHI->addIncoming(ConstantInt::getFalse(CI->getType()), bb2);
     PHI->addIncoming(b4, bb3);
     PHI->addIncoming(b5, bb4);
     
     // Make sure any users of the unsigned comparison is now an
     // user of the phi node.
     CI->replaceAllUsesWith(PHI);
     
     // Finally we remove the unsigned instruction
     CI->eraseFromParent();
     totalUnsignedICmpLowered++;
   }
  }

public:
  static char ID;

  LowerUnsignedICmp() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function &F) override {
    std::vector<ICmpInst *> worklist;
    for (inst_iterator It = inst_begin(F), E = inst_end(F); It != E; ++It) {
      Instruction *I = &*It;
      if (ICmpInst *CI = dyn_cast<ICmpInst>(I)) {

        if (!CI->getOperand(0)->getType()->isIntegerTy() ||
            !CI->getOperand(1)->getType()->isIntegerTy()) {
          // -- we only lower the instruction if both operands are
          //    integer
          continue;
        }

        // ensure only EQ, NEQ, SLT, ULT, SLE, ULE
        normalizeCmpInst(CI);
        if (CI->getPredicate() == CmpInst::ICMP_ULT ||
            CI->getPredicate() == CmpInst::ICMP_ULE) {
          worklist.push_back(CI);
        }
      }
    }

    bool change = !worklist.empty();
    while (!worklist.empty()) {
      ICmpInst *CI = worklist.back();
      worklist.pop_back();
      processUnsignedICmp(CI);
    }

    return change;
  }

  virtual StringRef getPassName() const override {
    return "Clam: Lower ULT and ULE instructions";
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    // AU.setPreservesAll ();
  }
};

char LowerUnsignedICmp::ID = 0;

Pass *createLowerUnsignedICmpPass() { return new LowerUnsignedICmp(); }

} // namespace clam
