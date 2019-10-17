#pragma once 

/* 
 * Infer invariants using Crab.
 */

#include "clam/crab/crab_cfg.hh"
#include "crab/checkers/base_property.hpp"
#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"

#include <memory>

// forward declarations

namespace llvm {
  class TargetLibraryInfo;
}

namespace clam {
  class HeapAbstraction;
  struct GenericAbsDomWrapper;
  class IntraClam_Impl;
  class InterClam_Impl;
  class CfgBuilder;
}

namespace sea_dsa {
  class AllocWrapInfo;
}

namespace clam {
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
       , OCT
       , PK
       , WRAPPED_INTERVALS
     };

  ////
  // Heap analysis for memory disambiguation
  ////
  enum heap_analysis_t
    { // use llvm-dsa (only context-insensitive)
      LLVM_DSA = 0,
      // use context-insensitive sea-dsa
      CI_SEA_DSA = 1,
      // use context-sensitive sea-dsa
      CS_SEA_DSA = 2
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

namespace clam {

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
    bool print_unjustified_assumptions;
    bool print_summaries;
    bool store_invariants;
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
	print_unjustified_assumptions(false), print_summaries(false),
	store_invariants(true), keep_shadow_vars(false),
	check(NOCHECKS), check_verbose(0) { }

    std::string abs_dom_to_str() const;
    
    std::string sum_abs_dom_to_str() const;
  };

  /**
   * A manager that keeps all the crab CFG builders.
   * A builder contains the crab CFG plus some extra information about
   * the translation.
   **/
  class CrabBuilderManager {
  public:
    using CfgBuilderPtr = std::shared_ptr<clam::CfgBuilder>;
    
    CrabBuilderManager();
    
    ~CrabBuilderManager();
    
    CrabBuilderManager(const CrabBuilderManager& o) = delete;
    
    CrabBuilderManager& operator=(const CrabBuilderManager& o) = delete;
    
    bool has_cfg(const llvm::Function &f) const;
    
    void add(const llvm::Function &f, CfgBuilderPtr cfg_builder);
    
    cfg_t& get_cfg(const llvm::Function &f) const;
    
    const CfgBuilderPtr get_cfg_builder(const llvm::Function &f) const;    
    
  private:
    llvm::DenseMap<const llvm::Function*, CfgBuilderPtr> m_cfg_builder_map;
  };

  using edges_set = std::set<std::pair<const llvm::BasicBlock*, const llvm::BasicBlock*>>;
  
  /**
   * Intra-procedural analysis of a function
   * 
   * Basic usage:
   *    CrabBuilderManager man;
   *    auto tli = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
   *    IntraClam ic(fun, tli, man);
   *    AnalysisParams params;
   *    ic.analyze(params, assumption_map_t());
   *    for (auto &b: fun) {
   *      if (auto dom_ptr = ic.get_pre(&b)) {
   *         crab::outs << *dom_ptr << "\n";
   *      }
   *    }
   **/ 
  class IntraClam {
    
  public:
    
    typedef std::shared_ptr<GenericAbsDomWrapper> wrapper_dom_ptr;
    typedef llvm::DenseMap<const llvm::BasicBlock*, wrapper_dom_ptr> invariant_map_t;
    typedef llvm::DenseMap<const llvm::BasicBlock*, lin_cst_sys_t> assumption_map_t;
    typedef crab::checker::checks_db checks_db_t;
    typedef std::shared_ptr<HeapAbstraction> heap_abs_ptr;
    
  private:

    std::unique_ptr<IntraClam_Impl> m_impl;
    llvm::Function *m_fun;
    variable_factory_t m_vfac;    
    invariant_map_t m_pre_map;
    invariant_map_t m_post_map;
    edges_set m_infeasible_edges;    
    checks_db_t m_checks_db;
    
  public:

    /**
     * Constructor that builds a crab CFG
     **/
    IntraClam(llvm::Function &fun,
		  const llvm::TargetLibraryInfo &tli,
		  CrabBuilderManager &man,
		  const crab::cfg::tracked_precision cfg_precision = crab::cfg::NUM,
		  heap_abs_ptr heap_abs = nullptr);

    ~IntraClam();    

    /** 
     * Clear all the internal state
     **/
    void clear();
    
    /**
     * Call crab analysis on the CFG under assumptions.
     **/    
    void analyze(AnalysisParams &params, const assumption_map_t &assumptions);

    /**
     * Call crab analysis on the CFG under assumptions starting from entry
     **/    
    void analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
		 const assumption_map_t &assumptions);
    
    /**
     * Compute strongest post-condition of an acyclic path.
     * Return false iff the path implies false.
     *
     * post contains the post-conditions at each block.
     * If it returns false then:
     *   - core is a minimal subset of statements that implies false
     **/
    template<typename Statement>
    bool path_analyze(const AnalysisParams& params,
		      const std::vector<const llvm::BasicBlock*>& path,
		      /* use gradually more expensive domains until unsat is proven*/
		      bool layered_solving,
		      std::vector<Statement>& core, invariant_map_t& post) const;
    
    template<typename Statement>
    bool path_analyze(const AnalysisParams& params,
		      const std::vector<const llvm::BasicBlock*>& path,
		      /* use gradually more expensive domains until unsat is proven*/
		      bool layered_solving,
		      std::vector<Statement>& core) const;

    /**
     * Return invariants that hold at the entry of b
     **/
    wrapper_dom_ptr get_pre(const llvm::BasicBlock *b, bool keep_shadows=false) const;

    /**
     * Return invariants that hold at the exit of b
     **/
    wrapper_dom_ptr get_post(const llvm::BasicBlock *b, bool keep_shadows=false) const;

    /**
     * Return true if there might be a feasible edge between b1 and b2
     **/
    bool has_feasible_edge(const llvm::BasicBlock *b1, const llvm::BasicBlock* b2) const;
    
    /**
     * Return a database with all checks.
     **/
    const checks_db_t& get_checks_db() const;
  };

  /**
   * Inter-procedural analysis of a module
   **/ 
  class InterClam {
    
  public:
    
    typedef std::shared_ptr<GenericAbsDomWrapper> wrapper_dom_ptr;
    typedef llvm::DenseMap<const llvm::BasicBlock*, wrapper_dom_ptr> invariant_map_t;
    typedef llvm::DenseMap<const llvm::BasicBlock*, lin_cst_sys_t> assumption_map_t;
    typedef crab::checker::checks_db checks_db_t;
    typedef std::shared_ptr<HeapAbstraction> heap_abs_ptr;
    
  private:

    std::unique_ptr<InterClam_Impl> m_impl;
    variable_factory_t m_vfac;    
    invariant_map_t m_pre_map;
    invariant_map_t m_post_map;
    edges_set m_infeasible_edges;    
    checks_db_t m_checks_db;
    
  public:

    /**
     * Constructor that builds a crab call graph.
     **/
    InterClam(llvm::Module &module,
		  const llvm::TargetLibraryInfo &tli,
		  CrabBuilderManager &man,
		  const crab::cfg::tracked_precision cfg_precision = crab::cfg::NUM,
		  heap_abs_ptr heap_abs = nullptr);

    ~InterClam();    

    /** 
     * Clear all the internal state
     **/
    void clear();
    
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

    /**
     * Return true if there might be a feasible edge between b1 and b2
     **/
    bool has_feasible_edge(const llvm::BasicBlock *b1, const llvm::BasicBlock* b2) const;
    
    /**
     * Return a database with all checks.
     **/
    const checks_db_t& get_checks_db() const;
  };
  
  /**
   * LLVM Module pass that computes invariants using Crab.
   **/
  class ClamPass : public llvm::ModulePass {

    typedef typename IntraClam::wrapper_dom_ptr wrapper_dom_ptr;
    typedef typename IntraClam::invariant_map_t invariant_map_t;
    typedef typename IntraClam::checks_db_t checks_db_t;
    typedef typename IntraClam::heap_abs_ptr heap_abs_ptr;
    invariant_map_t m_pre_map;
    invariant_map_t m_post_map;
    edges_set m_infeasible_edges;
    heap_abs_ptr m_mem;    
    variable_factory_t m_vfac;
    CrabBuilderManager m_cfg_builder_man;
    checks_db_t m_checks_db; 
    AnalysisParams m_params;
    const llvm::TargetLibraryInfo *m_tli;
    
   public:

    static char ID;        

    ClamPass();

    /* begin ModulePass API */    
    virtual void releaseMemory();
    
    virtual bool runOnModule(llvm::Module& M);

    virtual bool runOnFunction(llvm::Function &F);

    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const ;

    virtual llvm::StringRef getPassName() const {return "Clam";}
    /* end ModulePass API */

    variable_factory_t& get_var_factory() { return m_vfac; }

    heap_abs_ptr get_heap_abstraction() { return m_mem; }

    const AnalysisParams& get_analysis_params() { return m_params;}

    const CrabBuilderManager& getCfgBuilderMan() const { return m_cfg_builder_man;}

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
     * Return true if there might be a feasible edge between b1 and b2
     **/
    bool has_feasible_edge(const llvm::BasicBlock *b1, const llvm::BasicBlock* b2) const;
    
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

