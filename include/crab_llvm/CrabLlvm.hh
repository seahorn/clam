#ifndef __CRAB_LLVM_HPP_
#define __CRAB_LLVM_HPP_

/* 
 * Infer invariants using Crab.
 */

#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"

#include <boost/shared_ptr.hpp>

#include "crab_llvm/crab_cfg.hh"

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
  class HeapAbstraction;
  class GenericAbsDomWrapper;
}


namespace crab_llvm {
   ////
   // Base numerical domains for user options
   ////
  enum CrabDomain
     {   INTERVALS
       , INTERVALS_CONGRUENCES
       , BOXES
       , DIS_INTERVALS
       , ZONES_SPARSE_DBM
       , ZONES_SPLIT_DBM
       , TERMS_INTERVALS
       , TERMS_DIS_INTERVALS
         //TERMS_INTERVALS x  ZONES_SPLIT_DBM
       , TERMS_ZONES 
         //(#live vars<threshold ? TERMS_INTERVALSxZONES_SPLIT_DBM, INTERVALS)
       , ADAPT_TERMS_ZONES 
       , OPT_OCT_APRON
       , PK_APRON
     };
}

namespace crab_llvm {

  /*! Compute invariants using Crab for the whole module. */
  class CrabLlvm : public llvm::ModulePass {

    typedef crab::analyzer::liveness<cfg_ref_t> liveness_t;
    typedef crab::cg::call_graph<cfg_ref_t> call_graph_t; 
    typedef crab::cg::call_graph_ref<call_graph_t> call_graph_ref_t;
    typedef crab::checker::checks_db checks_db_t;
    typedef boost::shared_ptr<GenericAbsDomWrapper> GenericAbsDomWrapperPtr;
    typedef llvm::DenseMap<const llvm::BasicBlock *, GenericAbsDomWrapperPtr>
    invariants_map_t;
    typedef llvm::DenseMap<llvm::Function*, cfg_ptr_t> cfg_map_t;
    typedef boost::unordered_map <cfg_ref_t, const liveness_t*> liveness_map_t;
    
    invariants_map_t m_pre_map;
    invariants_map_t m_post_map;
    CrabDomain m_absdom;
    boost::shared_ptr<HeapAbstraction> m_mem;    
    variable_factory_t m_vfac;
    cfg_map_t m_cfg_map;
    boost::shared_ptr<checks_db_t> m_checks_db; 

    template<typename AbsDom> 
    void analyzeCfg (cfg_ref_t cfg,
		     const llvm::Function& F,
		     const liveness_t& live);

    template<typename BUAbsDom, typename TDAbsDom> 
    void analyzeCg (call_graph_ref_t cg,
		    const llvm::Module &M,
		    const liveness_map_t& live_map);

    void writeInvariants (llvm::raw_ostream& o, const llvm::Function& F);

   public:

    typedef invariants_map_t::iterator iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    CrabLlvm ();

    virtual void releaseMemory ();
    
    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const ;

    virtual const char* getPassName () const {return "CrabLlvm";}


    variable_factory_t& getVariableFactory () { return m_vfac; }

    HeapAbstraction& getHeapAbstraction () { return *m_mem; }

    CrabDomain getAbsDomain () const { return m_absdom; }

    // Needed for InsertInvariants
    cfg_ptr_t getCfg (llvm::Function* F);
    
    /**
     * return invariants that hold at the entry of BB
     **/
    GenericAbsDomWrapperPtr
    getPre (const llvm::BasicBlock *BB, bool KeepShadows=false) const;

    GenericAbsDomWrapperPtr
    operator[] (const llvm::BasicBlock *BB) const {
      return getPre (BB); 
    }
    
    /**
     * return invariants that hold at the exit of BB
     **/
    GenericAbsDomWrapperPtr
    getPost (const llvm::BasicBlock *BB, bool KeepShadows=false) const;
    
    /**
     * To query and view the analysis results 
     **/

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
    
    void print_checks (llvm::raw_ostream &o) const;
  };

} // end namespace 

#endif
