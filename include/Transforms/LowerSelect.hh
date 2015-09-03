#ifndef _LOWER_SELECT__HH__
#define _LOWER_SELECT__HH__

/* Remove select instructions */

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/Debug.h"

namespace crab_llvm
{
  using namespace llvm;

  class LowerSelect: public FunctionPass 
  {
    void processSelectInst(SelectInst *);
    
   public:
    
    static char ID;   
    LowerSelect(): FunctionPass(ID){ }    
    virtual bool runOnFunction(Function &);
    virtual const char* getPassName () const {return "LowerSelect";}
    virtual void getAnalysisUsage (AnalysisUsage &AU) const
    {
      AU.setPreservesAll ();
    }

  };

} 

#endif 
