/* Simplify void verifier.assume(i1) instructions */

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace llvm;

namespace crab_llvm {

struct SimplifyAssume : public ModulePass {
  static char ID;
  Function* m_assumeFn;

  SimplifyAssume() : ModulePass(ID), m_assumeFn(0) {}
        
  virtual bool runOnModule(Module &M) {
    LLVMContext& ctx = M.getContext();
    AttrBuilder B;
    AttributeList as = AttributeList::get(ctx, 
					   AttributeList::FunctionIndex,
					   B);
    m_assumeFn = dyn_cast<Function>
     (M.getOrInsertFunction("verifier.assume", 
			      as,
			      Type::getVoidTy(ctx),
			      Type::getInt1Ty(ctx)));
      
    bool change=false;
    for (auto &f : M) {
      change |= runOnFunction(f);
    }
    return change;
  }

  bool runOnFunction(llvm::Function &F) {
    if (F.isDeclaration() || F.empty())  {
      return false;
    }

    std::vector<Instruction*> RedundantInstructions;
    std::vector<BasicBlock*> UnreachableBlocks;    
    for (auto& BB: F) {
      for (auto& I: BB) {
	if (CallInst* CI = dyn_cast<CallInst>(&I)) {
	  Function *callee = CI->getCalledFunction();
	  if (callee && callee == m_assumeFn) {
	    CallSite CS(CI);          
	    Value* Cond = CS.getArgument(0);
	    if (ConstantInt* C = dyn_cast<ConstantInt>(Cond)) {
	      if (C == ConstantInt::getTrue(F.getContext())) {
		RedundantInstructions.push_back(CI);
	      } else if (C == ConstantInt::getFalse(F.getContext())) {
		UnreachableBlocks.push_back(CI->getParent());
	      }
	    }
	  }
	}	
      }
    }

    if (RedundantInstructions.empty() && UnreachableBlocks.empty()) {
      return false;
    }

    // Remove first redundant instructions
    while(!RedundantInstructions.empty()) {
      Instruction* I = RedundantInstructions.back();
      RedundantInstructions.pop_back();
      I->eraseFromParent();
    }

    // Make unreachable blocks
    IRBuilder<> Builder(F.getContext());
    while(!UnreachableBlocks.empty()) {
      BasicBlock* BB = UnreachableBlocks.back();
      UnreachableBlocks.pop_back();
      // Important to remove first all instructions in the block
      std::vector<Instruction*> BB_insts;
      for (Instruction&I: *BB) {
	BB_insts.push_back(&I);
      }
      while(!BB_insts.empty()) {
	Instruction* I = BB_insts.back();
	BB_insts.pop_back();
	I->eraseFromParent();
      }
      // Add unreachable instruction
      Builder.SetInsertPoint(BB);
      Builder.CreateUnreachable();
    }
    
    return true;
  }
    
  // It might not preserve the call graph ...
  void getAnalysisUsage(AnalysisUsage &AU) const {      
    //AU.setPreservesAll();
  }

  virtual StringRef getPassName() const {
    return "CrabLlvm: Simplify assume instructions";
  }
    
};

char crab_llvm::SimplifyAssume::ID = 0;
Pass* createSimplifyAssumePass() { return new SimplifyAssume(); }

} // end namespace 

