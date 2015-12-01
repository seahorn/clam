#ifndef __CRAB_LLVM_HPP_
#define __CRAB_LLVM_HPP_

/* 
 * Infer invariants using Crab.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"

#include <crab_llvm/AbstractDomains.hh>
#include <crab_llvm/CfgBuilder.hh>
#include <crab_llvm/MemAnalysis.hh>

#include <crab/cg/Cg.hpp>
#include <crab/analysis/Liveness.hpp>

namespace crab_llvm
{  
   //! Base numerical domains
   enum CrabDomain { INTERVALS, 
                     INTERVALS_CONGRUENCES, 
                     BOXES,
                     ZONES, 
                     TERMS,
                     NUM};
}

namespace crab_llvm
{

  using namespace llvm;
  using namespace crab::cfg_impl;

  /*! Compute invariants using Crab for the whole module. */
  class CrabLlvm : public llvm::ModulePass
  {
   private:

    typedef llvm::DenseMap<const llvm::BasicBlock *, GenericAbsDomWrapperPtr> invariants_map_t;
    typedef crab::analyzer::Liveness<cfg_t> liveness_t;
    // if inter-procedural analysis
    typedef boost::unordered_map <cfg_t, const liveness_t*> liveness_map_t;

    invariants_map_t m_pre_map;
    invariants_map_t m_post_map;
    CrabDomain       m_absdom;
    MemAnalysis      m_mem;    
    VariableFactory  m_vfac;

   public:

    typedef invariants_map_t::iterator iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    CrabLlvm (): llvm::ModulePass (ID), m_absdom (INTERVALS) { }

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
    GenericAbsDomWrapperPtr getPre (const llvm::BasicBlock *BB, 
                                    bool KeepShadows=false) const;

    GenericAbsDomWrapperPtr operator[] (const llvm::BasicBlock *BB) const {
      return getPre (BB); 
    }
    
    // return invariants that hold at the exit of BB
    GenericAbsDomWrapperPtr getPost (const llvm::BasicBlock *BB, 
                                     bool KeepShadows=false) const;

    TrackedPrecision getTrackLevel () const { 
      return m_mem.getTrackLevel (); 
    }

    VariableFactory& getVariableFactory () { 
      return m_vfac; 
    }

    CrabDomain getAbsDomain () const {
      return m_absdom;
    }
   
   private:

    template<typename AbsDomain> 
    bool runOnCfg (const cfg_t& cfg, 
                   const liveness_t& live, 
                   const llvm::Function &F);

    template<typename BUAbsDomain, typename TDAbsDomain> 
    bool runOnCg (const crab::cg::CallGraph<cfg_t>& cg, 
                  const liveness_map_t& live_map,
                  const llvm::Module &M);

    void write (llvm::raw_ostream& o, const llvm::Function& F);

  };

} // end namespace 

#endif
