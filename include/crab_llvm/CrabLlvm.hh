#ifndef __CRAB_LLVM_HPP_
#define __CRAB_LLVM_HPP_

/* 
 * Infer invariants using Crab.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/ErrorHandling.h"

#include <boost/shared_ptr.hpp>

#include <crab_llvm/AbstractDomains.hh>
#include <crab_llvm/CfgBuilder.hh>
#include <crab_llvm/MemAnalysis.hh>

#include <crab/cg/Cg.hpp>
#include <crab/analysis/Liveness.hpp>

namespace crab_llvm {

  using namespace llvm;
  using namespace crab::cfg_impl;

  /*! Compute invariants using Crab for the whole module. */
  class CrabLlvm : public llvm::ModulePass {
   private:

    typedef llvm::DenseMap<const llvm::BasicBlock *, GenericAbsDomWrapperPtr> invariants_map_t;
    typedef boost::shared_ptr <CfgBuilder> CfgBuilderPtr;
    typedef boost::unordered_map <Function*, CfgBuilderPtr > builder_map_t;
    typedef crab::analyzer::Liveness<cfg_t> liveness_t;
    // if inter-procedural analysis
    typedef boost::unordered_map <cfg_t, const liveness_t*> liveness_map_t;

    invariants_map_t m_pre_map;
    invariants_map_t m_post_map;
    CrabDomain m_absdom;
    boost::shared_ptr<MemAnalysis> m_mem;    
    VariableFactory m_vfac;
    builder_map_t m_builder_map;

   public:

    typedef invariants_map_t::iterator iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    CrabLlvm (): 
        llvm::ModulePass (ID),
        m_absdom (INTERVALS), 
        m_mem (new DummyMemAnalysis ())  { }

    virtual void releaseMemory () {
      m_pre_map.clear(); 
      m_post_map.clear(); 
      
      m_builder_map.clear ();
    }
    
    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual const char* getPassName () const {return "CrabLlvm";}

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
      return m_mem->getTrackLevel (); 
    }

    VariableFactory& getVariableFactory () { 
      return m_vfac; 
    }

    MemAnalysis& getMemAnalysis () {
      return *m_mem;
    }

    CrabDomain getAbsDomain () const {
      return m_absdom;
    }

    bool hasCfg (Function* F) const {
      auto const it = m_builder_map.find (F);
      return it != m_builder_map.end ();
    }

    // pre F is in the builder map
    cfg_t& getCfg (Function* F) {
      auto it = m_builder_map.find (F);
      if (it == m_builder_map.end ())
        report_fatal_error ("Not CFG found for function");

      auto builder = it->second;
      return builder->getCfg ();
    }

   private:

    template<typename AbsDomain> 
    bool analyzeCfg (cfg_t& cfg, const Function& F, const liveness_t& live);

    template<typename BUAbsDomain, typename TDAbsDomain> 
    bool analyzeCg (const crab::cg::CallGraph<cfg_t>& cg, 
                    const liveness_map_t& live_map,
                    const llvm::Module &M);

    void writeInvariants (llvm::raw_ostream& o, const llvm::Function& F);

  };

} // end namespace 

#endif
