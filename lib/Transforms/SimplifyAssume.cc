/* Simplify void verifier.assume(i1) instructions */

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/CallSite.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

namespace crab_llvm {

struct SimplifyAssume : public ModulePass {
  static char ID;
  Function* m_assumeFn;
  unsigned int m_num_removed_blocks; // for stats
  
  SimplifyAssume()
    : ModulePass(ID)
    , m_assumeFn(0)
    , m_num_removed_blocks(0) {}
        
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

    // llvm::errs() << "*** CRABLLVM: " << m_num_removed_blocks
    // 		 << " number of unreachable blocks\n";
    return change;
  }

  bool runOnFunction(llvm::Function &F) {
    if (F.isDeclaration() || F.empty())  {
      return false;
    }

    LLVMContext& ctx = F.getContext();
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
	      if (C == ConstantInt::getTrue(ctx)) {
		// mark it as redundant instruction
		RedundantInstructions.push_back(CI);
	      } else if (C == ConstantInt::getFalse(ctx)) {
		// mark the block as unreachable
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

    // Remove all unreachable blocks 
    while(!UnreachableBlocks.empty()) {
      BasicBlock* BB = UnreachableBlocks.back();
      UnreachableBlocks.pop_back();
      m_num_removed_blocks++;
      
      TerminatorInst *BBTerm = BB->getTerminator();
      // Loop through all of our successors and make sure they know that one
      // of their predecessors is going away.
      for (BasicBlock *Succ : BBTerm->successors()) {
	Succ->removePredecessor(BB);
      }
      // Zap all the instructions in the block.
      while (!BB->empty()) {
	Instruction &I = BB->back();
	// If this instruction is used, replace uses with an arbitrary value.
	// Because control flow can't get here, we don't care what we replace the
	// value with.  Note that since this block is unreachable, and all values
	// contained within it must dominate their uses, that all uses will
	// eventually be removed (they are themselves dead).
	if (!I.use_empty()) {
	  I.replaceAllUsesWith(UndefValue::get(I.getType()));
	}
	BB->getInstList().pop_back();
      }
      // Add unreachable terminator
      BB->getInstList().push_back(new UnreachableInst(ctx));
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

