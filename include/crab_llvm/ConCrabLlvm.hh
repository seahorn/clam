#ifndef __CONC_CRAB_LLVM_HPP_
#define __CONC_CRAB_LLVM_HPP_

/* 
 * Infer invariants using Crab for concurrent programs.
 */

#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

#include "crab_llvm/MemAnalysis.hh"
#include "crab_llvm/CrabLlvm.hh"

#include <crab/cfg/ConcSys.hpp>

namespace crab_llvm
{

  using namespace llvm;
  using namespace crab::cfg_impl;

  class ConCrabLlvm : public llvm::ModulePass
  {
    // --- used by the builder of each cfg
    MemAnalysis m_mem;    

    // --- threads in the programs
    set<Function*> m_threads;
 
    // --- map a shared variable to the threads that read/write on it
    DenseMap<const Value*, std::set<const Function*> > m_shared_vars;

   public:

    static char ID;        
    
    ConCrabLlvm (): llvm::ModulePass (ID) { }

    ~ConCrabLlvm () { }

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual void releaseMemory () {}
    
    virtual bool runOnModule (llvm::Module& M);

   private:

    bool processFunction (llvm::Function &F);

  };

} // end namespace

#endif
