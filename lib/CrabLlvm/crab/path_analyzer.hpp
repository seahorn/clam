#pragma once

#include <crab_llvm/crab_cfg.hh>
#include <crab/analysis/abs_transformer.hpp>

#include <unordered_map>

namespace crab {
namespace analyzer {

  
  /**
   ** Compute the strongest post-condition over a single path given as
   ** an ordered sequence of connected basic blocks.
   **/
  template<typename CFG, typename AbsDom>
  class path_analyzer {
  private:
    typedef typename CFG::statement_t stmt_t;
    typedef typename CFG::basic_block_label_t basic_block_label_t;
    typedef AbsDom abs_dom_t;
    typedef std::unordered_map<stmt_t*, AbsDom> stmt_to_dom_map_t;
    typedef std::unordered_map<basic_block_label_t, abs_dom_t> bb_to_dom_map_t;
    typedef intra_abs_transformer<AbsDom> fwd_abs_tr_t;
    
  public:
    // precondition: cfg is well typed.      
    path_analyzer (CFG cfg, AbsDom init);
    
    path_analyzer(const path_analyzer<CFG,AbsDom>& o) = delete;
    path_analyzer<CFG,AbsDom>& operator=(const path_analyzer<CFG,AbsDom>& o) = delete;
    
    /* Return true iff the forward analysis of path is not bottom. 
     * 
     * If it returns true:
     *   get_fwd_constraints returns for each block the constraints that hold at the entry.
     * 
     * If it returns false:
     *   get_unsat_core returns the minimal subset of statements that
     *   still implies false.
     */
    bool solve(const std::vector<basic_block_label_t>& path,
	       // it first try boolean reasoning before resorting to
	       // abstract domain AbsDom.
	       bool layered_solving);
    
    abs_dom_t get_fwd_constraints(basic_block_label_t b) const {
      auto it = m_fwd_dom_map.find(b);
      if (it != m_fwd_dom_map.end()) {
	return it->second;
      } else {
	// everything after the block where bottom was inferred is
	// bottom.
	return abs_dom_t::bottom();
      }
    }
    
    void get_unsat_core(std::vector<crab::cfg::statement_wrapper>& core) const {
      core.clear();
      core.assign(m_core.begin(), m_core.end());
    }
    
  private:
    
    bool has_kid(basic_block_label_t b1, basic_block_label_t b2);
    void minimize_path(const std::vector<crab::cfg::statement_wrapper>& path,
		       unsigned bottom_stmt);
    bool remove_irrelevant_statements(std::vector<crab::cfg::statement_wrapper>& path,
				      unsigned bottom_stmt,
				      bool only_data_dependencies);
    bool solve_path(const std::vector<basic_block_label_t>& path,
		    const bool only_bool_reasoning,
		    std::vector<typename crab::cfg::statement_wrapper>& stmts,
		    unsigned& bottom_block,
		    unsigned& bottom_stmt);

    // the cfg from which all paths are originated
    CFG m_cfg;
    // tell the forward abstract transformer to start with init
    abs_dom_t m_init;
    // map from basic blocks to postconditions
    bb_to_dom_map_t m_fwd_dom_map;
    // minimal subset of statements that explains path unsatisfiability
    // (only if solver return false (i.e., bottom)
    std::vector<crab::cfg::statement_wrapper> m_core;
  }; 
  
} // end namespace
} // end namespace

