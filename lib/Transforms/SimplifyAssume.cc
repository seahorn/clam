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

namespace crab_llvm
{

  struct SimplifyAssume : public ModulePass
  {
    static char ID;
    Function* m_assumeFn;

    SimplifyAssume () : ModulePass (ID), m_assumeFn (0) {}
        
    virtual bool runOnModule (Module &M)
    {
      LLVMContext& ctx = M.getContext ();
      AttrBuilder B;
      AttributeSet as = AttributeSet::get (ctx, 
                                           AttributeSet::FunctionIndex,
                                           B);
      m_assumeFn = dyn_cast<Function>
          (M.getOrInsertFunction ("verifier.assume", 
                                  as,
                                  Type::getVoidTy (ctx),
                                  Type::getInt1Ty (ctx),
                                  NULL));
      
      bool change=false;
      for (auto &f : M) 
        change |= runOnFunction (f); 
      
      return change;
    }

    bool runOnFunction (llvm::Function &F)
    {
      if (F.isDeclaration () || F.empty ()) 
        return false;
      
      IRBuilder<> Builder (F.getContext());

      SmallPtrSet<Instruction*, 16> to_remove;
      for(inst_iterator I = inst_begin(&F); I != inst_end(&F); ++I) {
        if (CallInst* CI = dyn_cast<CallInst> (&*I)) {
          Function *callee = CI->getCalledFunction ();
          if (callee && callee == m_assumeFn) {
            CallSite CS (CI);          
            Value* Cond = CS.getArgument (0);
            if (ConstantInt* C = dyn_cast<ConstantInt> (Cond)) {
              if (C == ConstantInt::getTrue (F.getContext ())) {
                to_remove.insert (CI);
              }
              else if (C == ConstantInt::getFalse (F.getContext ())) {
                Builder.SetInsertPoint (CI);
                Builder.CreateUnreachable ();
              }
            }
          }
        }
      }
      for (Instruction* I: to_remove) {
        I->eraseFromParent ();
      }
      return !to_remove.empty ();
    }
    
    // It might not preserve the call graph ...
    void getAnalysisUsage (AnalysisUsage &AU) const {      
      //AU.setPreservesAll ();
    }

    virtual const char * getPassName() const {
      return "Simplify assume instructions";
    }
    
  };

  char crab_llvm::SimplifyAssume::ID = 0;
  Pass* createSimplifyAssumePass () { return new SimplifyAssume (); }

} // end namespace 

