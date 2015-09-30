#ifndef __INSERT_INVARIANTS_HPP_
#define __INSERT_INVARIANTS_HPP_

/* 
 * Instrument LLVM bitecode by inserting invariants computed by
 * crab. The invariants are inserted as verifier.assume instructions.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Support/raw_ostream.h"

#include "crab_llvm/CrabLlvm.hh"
#include "crab_llvm/MemAnalysis.hh"

namespace crab_llvm
{

  using namespace llvm;

  class InsertInvariants : public llvm::ModulePass
  {
    CrabLlvm* m_crab;
    Function* m_assumeFn;
    MemAnalysis m_mem;    

   private:

    bool instrument_entries (z_lin_cst_sys_t csts, 
                             llvm::BasicBlock* bb, 
                             LLVMContext &ctx,
                             CallGraph* cg);
      
    template<typename AbsDomain> bool instrument_loads (AbsDomain pre, 
                                                        basic_block_t& bb, 
                                                        LLVMContext& ctx,
                                                        CallGraph* cg);
    
   public:

    static char ID;        
    
    InsertInvariants (): 
        llvm::ModulePass (ID), m_crab(0), m_assumeFn (0) {}

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

  };

} // end namespace crab_llvm

#endif
