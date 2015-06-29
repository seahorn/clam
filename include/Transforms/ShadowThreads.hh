#ifndef __SHADOW_THREADS_HH_
#define __SHADOW_THREADS_HH_

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"

#include <set>

/*
  Identify threads and global shared variables 
 */

namespace llvm_ikos
{
  using namespace llvm;

  class ShadowThreads: public ModulePass
  {

    // map a function to a thread id
    DenseMap<const Function*, unsigned> m_thread_ids;

    // map a value to a shared variable id 
    DenseMap<const Value*, unsigned > m_shared_ids;

    // map a shared variable to the threads that read/write on it
    DenseMap<const Value*, std::set<const Function*> > m_shared_vars;

    Constant *m_threadCreateFn;

    unsigned getThreadId (const Function *n);
    unsigned getSharedVarId (const Value *v);

   public:

    static char ID;

    ShadowThreads (): ModulePass (ID) { }

    ~ShadowThreads () { }

    virtual void getAnalysisUsage (AnalysisUsage &AU) const ;

    virtual void releaseMemory () {}
    
    virtual bool runOnModule (Module& M);

    virtual bool runOnFunction (Function &F);

  };

} // end namespace llvm_ikos

#endif 
