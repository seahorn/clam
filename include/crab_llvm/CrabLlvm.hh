#ifndef __CRAB_LLVM_HPP_
#define __CRAB_LLVM_HPP_

/* 
 * Infer invariants using Crab.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"

#include <crab_llvm/CfgBuilder.hh>
#include "crab_llvm/MemAnalysis.hh"


namespace crab_llvm
{  
   //! Base numerical domains
   enum CrabDomain { INTERVALS, 
                     INTERVALS_CONGRUENCES, 
                     ZONES, 
                     TERMS};
}

namespace crab_llvm
{

  using namespace llvm;
  using namespace crab::cfg_impl;

  /*! Compute invariants using Crab for the whole module. */
  class CrabLlvm : public llvm::ModulePass
  {
   public:
    //! all invariants will be translated into linear constraints
    typedef z_lin_cst_sys_t inv_tbl_val_t;

   private:
    typedef llvm::DenseMap<const llvm::BasicBlock *,inv_tbl_val_t> invariants_map_t;

    invariants_map_t m_pre_map;
    invariants_map_t m_post_map;
    CrabDomain       m_absdom;
    bool             m_runlive;
    MemAnalysis      m_mem;    
    VariableFactory  m_vfac;

   public:

    typedef invariants_map_t::iterator       iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    CrabLlvm (): llvm::ModulePass (ID), 
                 m_absdom (INTERVALS), m_runlive(false)  
    { }

    ~CrabLlvm () {
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
    const inv_tbl_val_t& getPre (const llvm::BasicBlock *BB) const {
      return this->operator[] (BB);
    }

    // return invariants that hold at the exit of BB
    const inv_tbl_val_t& getPost (const llvm::BasicBlock *BB) const {
      const_iterator it = m_post_map.find (BB);
      assert (it != m_post_map.end ());
      return it->second;
    }

    // alias for getPre
    const inv_tbl_val_t& operator[] (const llvm::BasicBlock *BB) const {
      const_iterator it = m_pre_map.find (BB);
      assert (it != m_pre_map.end ());
      return it->second;
    }

    TrackedPrecision getTrackLevel () const { 
      return m_mem.getTrackLevel (); 
    }

    VariableFactory& getVariableFactory () { 
      return m_vfac; 
    }

    iterator       begin ()       { return m_pre_map.begin(); } 
    iterator       end ()         { return m_pre_map.end();   }
    const_iterator begin () const { return m_pre_map.begin(); }
    const_iterator end ()   const { return m_pre_map.end();   }

    void dump (llvm::Module &M) const;

   private:

    template<typename AbsDomain> 
    bool runOnCfg (cfg_t& cfg, llvm::Function &F);

  };

} // end namespace 

#endif
