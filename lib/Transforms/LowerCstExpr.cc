#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace crab_llvm {
  
  using namespace llvm;

  class LowerCstExpr: public ModulePass {
    
    ConstantExpr* hasCstExpr(Value *V) {
      // We only handle top-level constant expressions ignoring
      // constant subexpressions
      if (Constant * cst = dyn_cast<Constant>(V)) {
        if (ConstantExpr * ce = dyn_cast<ConstantExpr>(cst)) {
          return ce;
	}
      }
      return nullptr;
    }

    Instruction * lowerCstExpr(ConstantExpr* CstExp, 
                               Instruction* InsertionLoc) {
      Instruction* NewI = CstExp->getAsInstruction ();
      // insert before
      InsertionLoc->getParent()->getInstList().insert(InsertionLoc->getIterator(), NewI); 
      return NewI;
    }
        
    bool runOnFunction(Function & F) {
      SmallPtrSet<Instruction*, 8> worklist;
      for (inst_iterator It = inst_begin(F), E = inst_end(F); It != E; ++It) {
        Instruction *I = &*It;
        for (unsigned int i=0; i < I->getNumOperands(); ++i) {
          if (hasCstExpr (I->getOperand(i))) 
            worklist.insert (I);
        }
      }
      
      bool change = !worklist.empty ();
      while (!worklist.empty()) {
        auto It = worklist.begin ();
        Instruction*I = *It;
        worklist.erase (*It);
        
        if (PHINode * PHI = dyn_cast<PHINode>(I)) {
          for (unsigned int i = 0; i < PHI->getNumIncomingValues (); ++i) {
            Instruction* InsertLoc = PHI->getIncomingBlock (i)->getTerminator ();        
            assert(InsertLoc);
            if (ConstantExpr * CstExp = hasCstExpr (PHI->getIncomingValue(i))) {
              Instruction* NewInst = lowerCstExpr (CstExp, InsertLoc);
              for (unsigned int j=i; j < PHI->getNumIncomingValues(); j++) {
                if ( (PHI->getIncomingValue (j) == PHI->getIncomingValue (i)) &&
                     (PHI->getIncomingBlock (j) == PHI->getIncomingBlock (i))) {
                  PHI->setIncomingValue (j, NewInst);
                }
              }
              worklist.insert (NewInst);
            }
          }
        } else { 
          for (unsigned int i=0; i < I->getNumOperands (); ++i) {
            if (ConstantExpr* CstExp = hasCstExpr (I->getOperand(i))) {
              Instruction * NewInst = lowerCstExpr (CstExp, I);
              I->replaceUsesOfWith (CstExp, NewInst);
              worklist.insert (NewInst);
            }
          }
        }
      }
      return change;
    }
    
   public:
    
    static char ID; 
    
    LowerCstExpr(): ModulePass (ID) {}
    
    virtual bool runOnModule(Module &M) {
     bool change = false;
     for (auto &F: M){ change |= runOnFunction(F); }
     return change;
    }
    
    void getAnalysisUsage (AnalysisUsage &AU) const {
      //AU.setPreservesAll ();
    }

    virtual const char * getPassName() const {
      return "Lower constant expressions";
    }
    
  };

  char LowerCstExpr::ID = 0;
  Pass* createLowerCstExprPass () { return new LowerCstExpr (); }

} // end namespace 
