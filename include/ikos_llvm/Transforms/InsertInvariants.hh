#ifndef __INSERT_INVARIANTS_HPP_
#define __INSERT_INVARIANTS_HPP_

/* 
 * Instrument program by inserting invariants computed by Ikos. The
 * invariants are inserted as assume instructions into the LLVM
 * bitcode. The insertion happens at most once per basic block and
 * only if the block has a load instruction.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"

#include "ikos_llvm/LlvmIkos.hh"

namespace llvm_ikos
{

  using namespace llvm;

  class InsertInvariants : public llvm::ModulePass
  {
    LlvmIkos* m_ikos;
    Function* m_assumeFn;

   public:

    static char ID;        
    
    InsertInvariants (): llvm::ModulePass (ID), m_ikos(0), m_assumeFn (0) {}

    ~InsertInvariants () {} 

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

  };

} // end namespace llvm_ikos

#endif
