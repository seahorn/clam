#include "llvm/IR/Module.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/PatternMatch.h"

#include "boost/range.hpp"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llvm::PatternMatch;

namespace crab_llvm {

  /// Returns true if v is used by assume
  static bool hasAssumeUsers (Value &v)
  {
    for (User *U : v.users ())
      if (CallInst *ci = dyn_cast<CallInst> (U))
        if (match (ci, m_Intrinsic<Intrinsic::assume>()))
          return true;
          
    return false;
  }

  
  class PromoteAssume : public FunctionPass {
  public:
    static char ID;

    PromoteAssume() : FunctionPass(ID) {} 

    bool runOnFunction (Function &F) {
      if (F.empty ()) return false;
      
      bool Changed = false;
      
      LLVMContext &ctx = F.getContext ();
      IRBuilder<> Builder(ctx);

      std::vector<Instruction*> to_remove;
      
      for (auto &I : boost::make_iterator_range(inst_begin (F), inst_end (F))) {
        if (!isa<CallInst> (&I)) continue;
	
        Value *v = I.stripPointerCasts ();
        CallSite CS (v);
        const Function *fn = CS.getCalledFunction ();
        if (!fn && CS.getCalledValue ()) {
          fn = dyn_cast<const Function> (CS.getCalledValue ()->stripPointerCasts ());
	}

        if (fn && (fn->getName ().equals ("verifier.assume") ||
                   fn->getName ().equals ("verifier.assume.not"))) {

	  Changed = true;
          Value *arg = CS.getArgument (0);
	  
          // already used in llvm.assume. 
          if (hasAssumeUsers (*arg)) {
	    to_remove.push_back(CS.getInstruction());
	    continue;
	  }
          
          Builder.SetInsertPoint (I.getParent (), BasicBlock::iterator(I));
          if (fn->getName ().equals ("verifier.assume.not")) {
            arg = Builder.CreateNot (arg);
	  }
	  
          CallInst *c = Builder.CreateAssumption (arg);
          /* 
             mark this assumption so that we know who inserted it
             use c->getMetadata(crallvm) to test.
          */
          c->setMetadata (F.getParent ()->getMDKindID ("crabllvm"),
                          MDNode::get (ctx, None));
	  
	  /*
	    enqueue verifier.assume to be removed
	   */
	  to_remove.push_back(&I);
        }
      }
      
      while (!to_remove.empty()) {
	Instruction* I = to_remove.back();
	to_remove.pop_back();
	I->eraseFromParent();
      }

      return Changed;
    }

    void getAnalysisUsage (AnalysisUsage &AU) const {
      //AU.setPreservesAll();
    }
    
    virtual const char *getPassName() const {
      return "PromoteAssume";
    }
    
  };

  char PromoteAssume::ID = 0;

  FunctionPass* createPromoteAssumePass() {
    return new PromoteAssume ();
  }
  
}

static llvm::RegisterPass<crab_llvm::PromoteAssume>
X ("promote-assume",
   "Promote crab-llvm assume to llvm assume intrinsic");
