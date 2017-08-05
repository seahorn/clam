#ifndef __INSERT_INVARIANTS_HPP_
#define __INSERT_INVARIANTS_HPP_

/* 
 * Instrument LLVM bitecode by inserting invariants computed by
 * crab. The invariants are inserted as verifier.assume instructions.
 */

#include "llvm/Pass.h"
#include "crab_llvm/crab_cfg.hh"

namespace llvm {
  class Function;
  class Module;
  class CallGraph;
}

namespace crab_llvm {

  class InsertInvariants : public llvm::ModulePass {

    llvm::Function* m_assumeFn;

    // TODO: move this to InsertInvariants.cc so this header file does
    // not expose crab_llvm/crab_cfg.hh
    bool instrument_entries (z_lin_cst_sys_t csts, llvm::BasicBlock* bb, 
                             llvm::LLVMContext &ctx, llvm::CallGraph* cg);
      
    template<typename AbsDomain> 
    bool instrument_loads (AbsDomain pre, basic_block_t& bb,  
                           llvm::LLVMContext& ctx, llvm::CallGraph* cg);

  public:
    
    static char ID;        
    
    InsertInvariants (): llvm::ModulePass (ID), m_assumeFn (0) {} 

    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual const char* getPassName () const {return "InsertInvariants";}

  };

} // end namespace crab_llvm

#endif
