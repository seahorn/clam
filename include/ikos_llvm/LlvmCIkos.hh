#ifndef __LLVM_CIKOS_HPP_
#define __LLVM_CIKOS_HPP_

/* 
 * Infer invariants using Ikos for concurrent programs.
 */

#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

#include "ikos_llvm/MemAnalysis.hh"
#include "ikos_llvm/LlvmIkos.hh"

#include <ikos/cfg/ConcSys.hpp>

namespace llvm_ikos
{

  using namespace llvm;
  using namespace cfg_impl;

  class LlvmCIkos : public llvm::ModulePass
  {
    // --- used by the builder of each cfg
    MemAnalysis m_mem;    

    // --- threads in the programs
    set<Function*> m_threads;
 
    // --- map a shared variable to the threads that read/write on it
    DenseMap<const Value*, std::set<const Function*> > m_shared_vars;

   public:

    static char ID;        
    
    LlvmCIkos (): llvm::ModulePass (ID) { }

    ~LlvmCIkos () { }

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual void releaseMemory () {}
    
    virtual bool runOnModule (llvm::Module& M);

   private:

    bool processFunction (llvm::Function &F);

  };

} // end namespace llvm_ikos

#endif
