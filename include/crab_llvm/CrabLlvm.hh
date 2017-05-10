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

#include "crab_llvm/crab_domains.hh"
#include "crab_llvm/wrapper_domain.hh"
#include "crab_llvm/MemAnalysis.hh"

#include <boost/shared_ptr.hpp>

// forward declarations
namespace crab {
  namespace analyzer {  
     template <typename T> class liveness;  
  }
  namespace cg {  
    template <typename T> class call_graph;  
    template <typename T> class call_graph_ref;  
  }
  namespace checker {
     class checks_db;
  }
} // end crab

namespace crab_llvm {

  using namespace llvm;

  /*! Compute invariants using Crab for the whole module. */
  class CrabLlvm : public llvm::ModulePass {

    typedef crab::analyzer::liveness<cfg_ref_t> liveness_t;
    typedef crab::cg::call_graph<cfg_ref_t> call_graph_t; 
    typedef crab::cg::call_graph_ref<call_graph_t> call_graph_ref_t;
    typedef crab::checker::checks_db checks_db_t;

    typedef llvm::DenseMap<const llvm::BasicBlock *, GenericAbsDomWrapperPtr> invariants_map_t;
    typedef llvm::DenseMap<llvm::Function*, cfg_ptr_t > cfg_map_t;
    typedef boost::unordered_map <cfg_ref_t, const liveness_t*> liveness_map_t;
    
    invariants_map_t m_pre_map;
    invariants_map_t m_post_map;
    CrabDomain m_absdom;
    boost::shared_ptr<MemAnalysis> m_mem;    
    VariableFactory m_vfac;
    cfg_map_t m_cfg_map;
    boost::shared_ptr<checks_db_t> m_checks_db; 

    template<typename AbsDom> 
    bool analyzeCfg (cfg_ref_t cfg, const Function& F, const liveness_t& live);

    template<typename BUAbsDom, typename TDAbsDom> 
    bool analyzeCg (call_graph_ref_t cg, const llvm::Module &M, const liveness_map_t& live_map);

    void writeInvariants (llvm::raw_ostream& o, const llvm::Function& F);

   public:

    typedef invariants_map_t::iterator iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    CrabLlvm ()
        : llvm::ModulePass (ID), 
          m_absdom (INTERVALS), 
          m_mem (boost::make_shared<DummyMemAnalysis>()),
          m_checks_db (nullptr) { }

    virtual void releaseMemory () {
      m_pre_map.clear(); 
      m_post_map.clear(); 
      m_cfg_map.clear ();
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

    VariableFactory& getVariableFactory () { return m_vfac; }

    MemAnalysis& getMemAnalysis () { return *m_mem; }

    CrabDomain getAbsDomain () const { return m_absdom; }

    // Needed for InsertInvariants
    cfg_ptr_t getCfg (Function* F) {
      auto it = m_cfg_map.find (F);
      if (it != m_cfg_map.end ())
        return it->second;
      return nullptr;
    }

    /// To query the analysis results 

    // return total number of checks if assertion checker enabled,
    // otherwise 0
    unsigned get_total_checks() const;
    // return total number of safe checks if assertion checker
    // enabled, otherwise 0
    unsigned get_total_safe_checks () const;
    // return total number of definite error checks if assertion
    // checker enabled, otherwise 0
    unsigned get_total_error_checks () const;
    // return total number of possibly error checks if assertion
    // checker enabled, otherwise 0
    unsigned get_total_warning_checks () const;
    // Whether Crab is able to discharge all assertions
    bool is_safe () const;
    bool is_unsafe () const;
    
  };

} // end namespace 

#endif
