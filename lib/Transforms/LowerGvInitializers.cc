/** Pass to lower scalar initializers to global variables into
    explicit initialization code */

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "boost/range.hpp"

using namespace llvm;

namespace crab_llvm {
  
  struct LowerGvInitializers : public ModulePass
  {
    static char ID;
    
    LowerGvInitializers () : ModulePass (ID) {}
    
    virtual bool runOnModule (Module &M)
    {
      const DataLayout* DL = &getAnalysis<DataLayoutPass>().getDataLayout ();

      Function *f = M.getFunction ("main");
      if (!f) return false;
      
      IRBuilder<> Builder (f->getContext ());
      
      Builder.SetInsertPoint (&f->getEntryBlock (), 
                              f->getEntryBlock ().begin ());

      bool change=false;
      for (GlobalVariable &gv : boost::make_iterator_range (M.global_begin (),
                                                            M.global_end ()))
      {
        if (!gv.hasInitializer ()) continue;
        PointerType *ty = dyn_cast<PointerType> (gv.getType ());
        if (!ty) continue;
        Type *ety = ty->getElementType ();
        // only deal with scalars for now
        if (!ety->isIntegerTy ()) continue;
        
        IntegerType* ity = cast<IntegerType> (ety);
        
        // -- create a store instruction
        Builder.CreateAlignedStore (gv.getInitializer (), &gv, 
                                    DL->getABITypeAlignment (ity));
        change=true;
      }
      
      return change;
      
    }
    
    void getAnalysisUsage (AnalysisUsage &AU) const {
      AU.setPreservesAll ();
      AU.addRequired<llvm::DataLayoutPass>();
    }

    virtual const char * getPassName() const {
      return "LowerGvInitializers";
    }
    
  };

  char LowerGvInitializers::ID = 0;
  Pass* createLowerGvInitializersPass () { return new LowerGvInitializers (); }  

} 



