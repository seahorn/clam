#ifndef __CRAB_LLVM_HPP_
#define __CRAB_LLVM_HPP_

/* 
 * Infer invariants using Crab.
 */

#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"
#include "crab_llvm/crab_cfg.hh"
#include <boost/shared_ptr.hpp>

// forward declarations

namespace llvm {
  class TargetLibraryInfo;
}

namespace crab {
  namespace checker {
    class checks_db;
  }
} 

namespace crab_llvm {
  class HeapAbstraction;
  struct GenericAbsDomWrapper;
  class IntraCrabLlvm_Impl;
  class InterCrabLlvm_Impl;
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
       /*, ZONES_SPARSE_DBM*/
       , ZONES_SPLIT_DBM
       , TERMS_INTERVALS
       , TERMS_DIS_INTERVALS
         //TERMS_INTERVALS x  ZONES_SPLIT_DBM
       , TERMS_ZONES 
         //(#live vars<threshold ? TERMS_INTERVALSxZONES_SPLIT_DBM, INTERVALS)
       , ADAPT_TERMS_ZONES 
       , OPT_OCT_APRON
       , PK_APRON
       , WRAPPED_INTERVALS
     };

  ////
  // Kind of checker
  ////
  enum assert_check_kind_t
    { NOCHECKS = 0,
      ASSERTION = 1,
      NULLITY = 2
    };  
}

namespace crab_llvm {

  /**
   * Class to set analysis options
   **/
  struct AnalysisParams {
    CrabDomain dom;
    CrabDomain sum_dom;
    bool run_backward;
    bool run_liveness;
    bool run_inter;
    unsigned relational_threshold;
    unsigned widening_delay;
    unsigned narrowing_iters;
    unsigned widening_jumpset;
    bool stats;
    bool print_invars;
    bool print_preconds;
    bool print_assumptions;
    bool print_summaries;
    bool keep_shadow_vars;
    assert_check_kind_t check;
    unsigned check_verbose;
    
    AnalysisParams()
      : dom(INTERVALS), sum_dom(ZONES_SPLIT_DBM),
	run_backward(false), run_liveness(false), run_inter(false),
	relational_threshold(10000),
	widening_delay(1), narrowing_iters(10), widening_jumpset(0),
	stats(false),
	print_invars(false), print_preconds(false),
	print_assumptions(false), print_summaries(false),
	keep_shadow_vars(false),
	check(NOCHECKS), check_verbose(0) {}
  };

  /**
   * A manager that keeps all the crab CFGs 
   **/
  class CfgManager {
    // The manager owns the pointers to cfg's
    llvm::DenseMap<const llvm::Function*, cfg_t*> m_cfg_map;
  public:
    CfgManager();
    ~CfgManager();
    bool has_cfg(const llvm::Function &f) const;
    cfg_ref_t operator[](const llvm::Function &f) const;
    void add(const llvm::Function &f, cfg_t *cfg);
  };
  
  /**
   * Intra-procedural analysis of a function
   * 
   * Basic usage:
   *    CfgManager cfg_man;
   *    auto tli = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
   *    IntraCrabLlvm ic(fun, tli, cfg_man);
   *    AnalysisParams params;
   *    ic.analyze(params, assumption_map_t());
   *    for (auto &b: fun) {
   *      if (auto dom_ptr = ic.get_pre(&b)) {
   *         crab::outs << *dom_ptr << "\n";
   *      }
   *    }
   **/ 
  class IntraCrabLlvm {
    
  public:
    
    typedef boost::shared_ptr<GenericAbsDomWrapper> wrapper_dom_ptr;
    typedef llvm::DenseMap<const llvm::BasicBlock*, wrapper_dom_ptr> invariant_map_t;
    typedef llvm::DenseMap<const llvm::BasicBlock*, lin_cst_sys_t> assumption_map_t;
    typedef crab::checker::checks_db checks_db_t;
    typedef boost::shared_ptr<checks_db_t> checks_db_ptr;
    typedef boost::shared_ptr<HeapAbstraction> heap_abs_ptr;
    
  private:

    std::unique_ptr<IntraCrabLlvm_Impl> m_impl;
    variable_factory_t m_vfac;    
    invariant_map_t m_pre_map;
    invariant_map_t m_post_map;

  public:

    /**
     * Constructor that builds a crab CFG
     **/
    IntraCrabLlvm(llvm::Function &fun,
		  const llvm::TargetLibraryInfo &tli,
		  CfgManager &man,
		  const crab::cfg::tracked_precision cfg_precision = crab::cfg::NUM,
		  heap_abs_ptr heap_abs = nullptr);

    ~IntraCrabLlvm();    

    /**
     * Call crab analysis on the CFG under assumptions.
     **/    
    void analyze(AnalysisParams &params, const assumption_map_t &assumptions);

    /**
     * Return invariants that hold at the entry of b
     **/
    wrapper_dom_ptr get_pre(const llvm::BasicBlock *b, bool keep_shadows=false) const;

    /**
     * Return invariants that hold at the exit of b
     **/
    wrapper_dom_ptr get_post(const llvm::BasicBlock *b, bool keep_shadows=false) const;


    /**
     * Compute strongest post-condition of an acyclic path.
     * Return false iff the path implies false.
     *
     * post contains the post-conditions at each block.
     * If it returns false then:
     *   - pre contains that necessary preconditions that imply false
     *   - core is a minimal subset of statements that implies false
     **/
    template<typename Statement>
    bool path_analyze(const AnalysisParams& params,
		      const std::vector<const llvm::BasicBlock*>& path,
		      std::vector<Statement>& core,
		      invariant_map_t& post, invariant_map_t& pre) const;
    
    template<typename Statement>
    bool path_analyze(const AnalysisParams& params,
		      const std::vector<const llvm::BasicBlock*>& path,
		      std::vector<Statement>& core) const;
  };

  /**
   * Inter-procedural analysis of a module
   **/ 
  class InterCrabLlvm {
    
  public:
    
    typedef boost::shared_ptr<GenericAbsDomWrapper> wrapper_dom_ptr;
    typedef llvm::DenseMap<const llvm::BasicBlock*, wrapper_dom_ptr> invariant_map_t;
    typedef llvm::DenseMap<const llvm::BasicBlock*, lin_cst_sys_t> assumption_map_t;
    typedef crab::checker::checks_db checks_db_t;
    typedef boost::shared_ptr<checks_db_t> checks_db_ptr;
    typedef boost::shared_ptr<HeapAbstraction> heap_abs_ptr;
    
  private:

    std::unique_ptr<InterCrabLlvm_Impl> m_impl;
    variable_factory_t m_vfac;    
    invariant_map_t m_pre_map;
    invariant_map_t m_post_map;

  public:

    /**
     * Constructor that builds a crab call graph.
     **/
    InterCrabLlvm(llvm::Module &module,
		  const llvm::TargetLibraryInfo &tli,
		  CfgManager &man,
		  const crab::cfg::tracked_precision cfg_precision = crab::cfg::NUM,
		  heap_abs_ptr heap_abs = nullptr);

    ~InterCrabLlvm();    

    /**
     * Call crab analysis on the call graph under assumptions.
     **/    
    void analyze(AnalysisParams &params, const assumption_map_t &assumptions);

    /**
     * Return invariants that hold at the entry of b
     **/
    wrapper_dom_ptr get_pre(const llvm::BasicBlock *b, bool keep_shadows=false) const;

    /**
     * Return invariants that hold at the exit of b
     **/
    wrapper_dom_ptr get_post(const llvm::BasicBlock *b, bool keep_shadows=false) const;
  };
  
  /**
   * LLVM Module pass that computes invariants using Crab.
   **/
  class CrabLlvmPass : public llvm::ModulePass {

    typedef typename IntraCrabLlvm::wrapper_dom_ptr wrapper_dom_ptr;
    typedef typename IntraCrabLlvm::invariant_map_t invariant_map_t;
    typedef typename IntraCrabLlvm::checks_db_ptr checks_db_ptr;
    typedef typename IntraCrabLlvm::heap_abs_ptr heap_abs_ptr;
    
    invariant_map_t m_pre_map;
    invariant_map_t m_post_map;
    heap_abs_ptr m_mem;    
    variable_factory_t m_vfac;
    CfgManager m_cfg_man;
    checks_db_ptr m_checks_db; 
    AnalysisParams m_params;
    const llvm::TargetLibraryInfo *m_tli;
    
   public:

    static char ID;        

    CrabLlvmPass();

    /* begin ModulePass API */    
    virtual void releaseMemory();
    
    virtual bool runOnModule(llvm::Module& M);

    virtual bool runOnFunction(llvm::Function &F);

    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const ;

    virtual const char* getPassName() const {return "CrabLlvm";}
    /* end ModulePass API */

    variable_factory_t& get_var_factory() { return m_vfac; }

    heap_abs_ptr get_heap_abstraction() { return m_mem; }

    bool has_cfg(llvm::Function &F);
    
    cfg_ref_t get_cfg(llvm::Function &F);
    
    /**
     * return invariants that hold at the entry of BB
     **/
    wrapper_dom_ptr get_pre(const llvm::BasicBlock *BB, bool KeepShadows=false) const;

    /**
     * return invariants that hold at the exit of BB
     **/
    wrapper_dom_ptr get_post(const llvm::BasicBlock *BB, bool KeepShadows=false) const;
    
    /**
     * To query and view the analysis results 
     **/

    /* return total number of checks if assertion checker enabled,
       otherwise 0 */
    unsigned get_total_checks() const;
    /* return total number of safe checks if assertion checker
       enabled, otherwise 0 */
    unsigned get_total_safe_checks() const;
    /* return total number of definite error checks if assertion
       checker enabled, otherwise 0 */
    unsigned get_total_error_checks() const;
    /* return total number of possibly error checks if assertion
       checker enabled, otherwise 0 */
    unsigned get_total_warning_checks() const;
    
    void print_checks(llvm::raw_ostream &o) const;
  };

} // end namespace 

#endif
