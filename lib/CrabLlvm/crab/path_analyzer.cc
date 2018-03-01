#include "crab_llvm/config.h"
#include "path_analyzer.hpp"
#include "boost/unordered_set.hpp"

// flat_killgen_domain
#include <crab/iterators/killgen_fixpoint_iterator.hpp>


namespace crab {
namespace analyzer {

template<typename CFG, typename AbsDom>
path_analyzer<CFG,AbsDom>::path_analyzer(CFG cfg, AbsDom& init, bool ignore_assertions)
  : m_cfg(cfg), m_fwd_abs_tr(&init, ignore_assertions) { }

template<typename CFG, typename AbsDom>  
bool path_analyzer<CFG,AbsDom>::solve(const std::vector<basic_block_label_t>& path,
				      bool compute_preconditions) {

  // Reset state
  m_fwd_dom_map.clear();
  m_bwd_dom_map.clear();
  m_core.clear();
  
  if (path.empty()) {
    CRAB_WARN("Empty path: do nothing\n");
    return true;
  }
  
  // Sanity checks
  basic_block_label_t first = path.front();
  basic_block_label_t last = path.back();
  if (m_cfg.entry() != first)
    CRAB_ERROR("First block of the path must be the entry block of the cfg");
  if (m_cfg.has_exit() && (m_cfg.exit() != last))
      CRAB_ERROR("Last block of the path must be the exit block of the cfg");
  if (path.size() > 1) {
    boost::unordered_set<basic_block_label_t> visited;
    for (unsigned i=0;i < path.size(); ++i) {
      if (!visited.insert(path[i]).second) {
	CRAB_ERROR("The path is not acyclic");
      }
      if (i < path.size() - 1)  {
	if (!has_kid(path[i], path[i+1])) {
	  CRAB_WARN("There is no an edge from ",
		    cfg_impl::get_label_str(path[i]), " to ",
		    cfg_impl::get_label_str(path[i+1]));
	  return true;
	}
      }
    }
  }

  // contain all statements along the path until the end of the path
  // or bottom is found.
  std::vector<typename crab::cfg::statement_wrapper> path_statements;
  bool bottom_found = false;
  unsigned bottom_pos = path.size();
  // Compute strongest post-condition over the path
  abs_dom_t pre = m_fwd_abs_tr.inv();
  stmt_to_dom_map_t stmt_dom_map;
  for(unsigned i=0; i < path.size(); ++i) {
    if (pre.is_bottom()) {
      if (!bottom_found) {
	bottom_pos = i; // update only the first time bottom is found
      }
      bottom_found = true;
      break;
    }

    // store constraints that hold at the entry of the block
    basic_block_label_t node = path[i];
    auto it = m_fwd_dom_map.find (node);
    if (it == m_fwd_dom_map.end()) {
      m_fwd_dom_map.insert(std::make_pair (node, pre));
    }

    // compute strongest post-condition for one block
    m_fwd_abs_tr.set (pre);
    auto &b = m_cfg.get_node (node);      
    for (auto &s : b) {
      if (!s.is_assert() && !s.is_ptr_assert() && !s.is_bool_assert()) {
	path_statements.push_back(crab::cfg::statement_wrapper(&s, node));
      }
      // XXX: we can store forward constraints that might help the
      // backward analysis. This step is optional. We don't use it
      // for now.
      // stmt_dom_map.insert(std::make_pair(&s, m_fwd_abs_tr.inv()));
      s.accept (&m_fwd_abs_tr);
    }
  }

  if (bottom_found) {
    if (compute_preconditions) {
      // -- Compute pre-conditions starting from the block for which we
      //    inferred bottom.
      assert (bottom_pos < path.size());
      abs_dom_t abs_val = abs_dom_t::top(); 
      bwd_abs_tr_t bwd_abs_tr(&abs_val, stmt_dom_map, true);
      for(int i=bottom_pos; i >= 0; --i) {
	basic_block_label_t node = path[i];
	auto &b = m_cfg.get_node (node);
	for(auto &s: boost::make_iterator_range(b.rbegin(),b.rend())) {
	  s.accept (&bwd_abs_tr);
	}
	auto it = m_bwd_dom_map.find (node);
	if (it == m_bwd_dom_map.end()) {
	  m_bwd_dom_map.insert(std::make_pair (node, abs_val));
	}
	if (abs_val.is_bottom())
	  break;
      }
    }
    
    // -- Compute minimal subset of statements that still implies
    //    bottom.
    minimize_path(path_statements);
  }
  return !bottom_found;
}
  

template<typename CFG, typename AbsDom>  
bool path_analyzer<CFG,AbsDom>::has_kid(basic_block_label_t b1, basic_block_label_t b2) {
  for (basic_block_label_t child: m_cfg.next_nodes (b1)) {
    if (child == b2)
      return true;
  }
  return false;
}

// Compute a minimal subset of statements based on syntactic
// dependencies. 
template<typename CFG, typename AbsDom>    
void path_analyzer<CFG,AbsDom>::
remove_irrelevant_statements(std::vector<crab::cfg::statement_wrapper>& core) {
  typedef typename CFG::basic_block_t::assume_t assume_t;
  typedef typename CFG::basic_block_t::bool_assume_t bool_assume_t;
  
  unsigned size = core.size();
  if (size <= 1) {
    return;
  }
  
  stmt_t& last = *(core[size-1].m_s);
  
  if (!(last.is_assume()) && !(last.is_bool_assume())) {
    // we expect last statement to be an assume, otherwise we bail out.
    return;
  }

  typedef domains::flat_killgen_domain<typename CFG::variable_t> var_dom_t;
  var_dom_t d;
  if (last.is_assume()) {
    auto assume = static_cast<const assume_t*>(&last);
    if (assume->constraint().is_contradiction()) {
      return; // we do nothing
    }
    for (typename CFG::variable_t v: assume->constraint().variables()) {
      d += v;
    }
  } else {
    auto bool_assume = static_cast<const bool_assume_t*>(&last);
    d += bool_assume->cond();
  }

  // Traverse the whole path backwards and remove irrelevant
  // statements based on data dependencies.
  std::vector<bool> enabled(size, false);      
  enabled[size-1] = true;  // added the last assume
  for(int i= size-2; i >= 0 ; --i) { // start from penultimate statement
    stmt_t& s = *(core[i].m_s);
    var_dom_t uses, defs;
    const typename stmt_t::live_t& ls = s.get_live();
    for (auto v: boost::make_iterator_range(ls.uses_begin(), ls.uses_end()))
    { uses += v; }
    for (auto v: boost::make_iterator_range(ls.defs_begin(), ls.defs_end()))
    { defs += v; }
    if (defs.is_bottom() && !uses.is_bottom() && !(uses & d).is_bottom ()) {
      d += uses;
      enabled[i] = true;
    } else if (!(d & defs).is_bottom()) {
      d -= defs;
      d += uses;
      enabled[i] = true;      
    } else {
      // irrelevant statement
    }
  }
  std::vector<crab::cfg::statement_wrapper> res;
  res.reserve(size);
  for(unsigned i=0; i < size; ++i) {
    if (enabled[i]) res.push_back(core[i]);
  }
  core.assign(res.begin(), res.end());
}
  
  
/** 
 ** Compute a minimal subset of statements needed to prove that the
 ** path is infeasible.
 ** 
 ** Pre: the strongest post-condition of path is false.
 ** Pre: path is well-formed.
 **/
template<typename CFG, typename AbsDom>    
void path_analyzer<CFG,AbsDom>::minimize_path(const std::vector<crab::cfg::statement_wrapper>& path) {

  std::vector<crab::cfg::statement_wrapper> core(path.begin(), path.end());
  // Remove first syntactically irrelevant statements wrt to the last
  // statement which should be an assume statement where bottom was
  // first inferred.
  remove_irrelevant_statements(core);
  
  // We use the enabled vector because we need to preserve the order
  // since abstract interpretation cares about that.
  std::vector<bool> enabled(core.size(), true);
  for (unsigned i = 0; i < core.size (); ++i) {
    enabled[i] = false;
    abs_dom_t pre = AbsDom::top(); // m_fwd_abs_tr.inv();
    bool res = true;
    for(unsigned j=0; j < core.size(); ++j) {
      if (enabled[j]) {
	m_fwd_abs_tr.set (pre);
	core[j].m_s->accept (&m_fwd_abs_tr);
	res = !pre.is_bottom();
	if (!res) break;
      }
    }
    if (res) {
      enabled[i] = true;
    }
  }

  m_core.reserve(core.size());
  for (unsigned i=0; i < core.size();++i) {
    if (enabled[i]) {
      m_core.push_back(core[i]);
    }
  }
  assert(!m_core.empty());
}
}
}

#include <crab_llvm/crab_domains.hh>
#include <crab_llvm/crab_cfg.hh>

namespace crab {
namespace analyzer {
// explicit instantiations
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::num_domain_t>;
#ifdef HAVE_ALL_DOMAINS  
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::term_int_domain_t>;
#endif   
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::interval_domain_t>;
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::wrapped_interval_domain_t>;      
} 
} 

