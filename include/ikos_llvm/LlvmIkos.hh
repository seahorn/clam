#ifndef __LLVM_IKOS_HPP_
#define __LLVM_IKOS_HPP_

/* 
 * Infer invariants using Ikos.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"

#include <ikos_llvm/CfgBuilder.hh>
#include "ikos_llvm/MemAnalysis.hh"


namespace llvm_ikos
{  
   //! Base numerical domains
   enum IkosDomain { INTERVALS, 
                     CONGRUENCES, 
                     INTERVALS_CONGRUENCES, 
                     ZONES, 
                     OCTAGONS, 
                     TERMS};
}

namespace llvm_ikos
{

  using namespace llvm;
  using namespace cfg_impl;

  /*! Compute invariants using Ikos for the whole module. */
  class LlvmIkos : public llvm::ModulePass
  {
    typedef llvm::DenseMap< const llvm::BasicBlock *, 
                            z_lin_cst_sys_t > invariants_map_t;

    invariants_map_t m_pre_map;
    invariants_map_t m_post_map;
    IkosDomain       m_absdom;
    bool             m_runlive;
    MemAnalysis      m_mem;    
    VariableFactory  m_vfac;

   public:

    typedef invariants_map_t::iterator       iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    LlvmIkos (): llvm::ModulePass (ID), 
                 m_absdom (INTERVALS), m_runlive(false)  
    { }

    ~LlvmIkos () {
      m_pre_map.clear(); 
      m_post_map.clear(); 
    }

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual void releaseMemory () {
      m_pre_map.clear(); 
      m_post_map.clear(); 
    }
    
    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    // return invariants that hold at the entry of BB
    z_lin_cst_sys_t getPre (const llvm::BasicBlock *BB) const
    {
      return this->operator[] (BB);
    }

    // return invariants that hold at the exit of BB
    z_lin_cst_sys_t getPost (const llvm::BasicBlock *BB) const
    {
      const_iterator it = m_post_map.find (BB);
      assert (it != m_post_map.end ());
      return it->second;
    }

    // alias for getPre
    z_lin_cst_sys_t operator[] (const llvm::BasicBlock *BB) const
    {
      const_iterator it = m_pre_map.find (BB);
      assert (it != m_pre_map.end ());
      return it->second;
    }

    TrackedPrecision getTrackLevel () const { return m_mem.getTrackLevel (); }

    VariableFactory& getVariableFactory () { return m_vfac; }

    iterator       begin ()       { return m_pre_map.begin(); } 
    iterator       end ()         { return m_pre_map.end();   }
    const_iterator begin () const { return m_pre_map.begin(); }
    const_iterator end ()   const { return m_pre_map.end();   }

    void dump (llvm::Module &M) const;

   private:

    z_lin_cst_t mkTRUE() const 
    { return z_lin_cst_t ( z_lin_exp_t (1) == z_lin_exp_t (1)); }

    z_lin_cst_t mkFALSE() const 
    { return z_lin_cst_t ( z_lin_exp_t (1) == z_lin_exp_t (0)); }

    template<typename AbsDomain> 
    bool runOnCfg (cfg_t& cfg, llvm::Function &F);

  };

} // end namespace llvm_ikos

#endif
