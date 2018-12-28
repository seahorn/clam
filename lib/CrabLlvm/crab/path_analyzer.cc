#include "crab_llvm/config.h"
#include "path_analyzer.hpp"
// flat_killgen_domain
#include <crab/iterators/killgen_fixpoint_iterator.hpp>

#include <boost/unordered_set.hpp>
#include <boost/range/iterator_range.hpp>

namespace crab {
namespace analyzer {

template<typename CFG, typename AbsDom>
path_analyzer<CFG,AbsDom>::path_analyzer(CFG cfg, AbsDom init, bool ignore_assertions)
  : m_cfg(cfg), m_init(init), m_ignore_assertions(ignore_assertions) { }

template<typename CFG, typename AbsDom>  
bool path_analyzer<CFG,AbsDom>::
solve_path(const std::vector<basic_block_label_t>& path,
	   const bool only_bool_reasoning,
	   std::vector<typename crab::cfg::statement_wrapper>& path_statements,
	   unsigned& bottom_pos) {
  
  bool bottom_found = false;
  bottom_pos = path.size();
  
  AbsDom pre(m_init);
  fwd_abs_tr_t abs_tr(&pre);
  for(unsigned i=0, e=path.size(); i < e; ++i) {
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
    auto &b = m_cfg.get_node (node);
    for (auto &s : b) {
      if (only_bool_reasoning) {
	if (!(s.is_bool_bin_op() || s.is_bool_assign_cst() || s.is_bool_assign_var() ||
	      s.is_bool_assume() || s.is_bool_assert()     || s.is_bool_select())) {
	  continue;
	}
      }
      if (!s.is_assert() && !s.is_ptr_assert() && !s.is_bool_assert()) {
	path_statements.push_back(crab::cfg::statement_wrapper(&s, node));
      }
      // XXX: we can store forward constraints that might help the
      // backward analysis. This step is optional. We don't use it
      // for now.
      // stmt_dom_map.insert(std::make_pair(&s, m_fwd_abs_tr.inv()));
      s.accept (&abs_tr);
    }
  }
  return bottom_found;
}
  
template<typename CFG, typename AbsDom>  
bool path_analyzer<CFG,AbsDom>::solve(const std::vector<basic_block_label_t>& path,
				      bool layered_solving, bool compute_preconditions) {
  
  // Reset state
  m_fwd_dom_map.clear();
  m_bwd_dom_map.clear();
  m_core.clear();
  
  if (path.empty()) {
    CRAB_WARN("Empty path: do nothing\n");
    return true;
  }
    
  #if 0
  // Sanity checks
  basic_block_label_t first = path.front();
  basic_block_label_t last = path.back();  
  if (m_cfg.entry() != first)
    CRAB_ERROR("First block of the path must be the entry block of the cfg");
  if (m_cfg.has_exit() && (m_cfg.exit() != last))
    CRAB_ERROR("Last block of the path must be the exit block of the cfg");
  #endif
  
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
  // Compute strongest post-condition over the path
  unsigned bottom_pos;
  stmt_to_dom_map_t stmt_dom_map;
  bool bottom_found;
  if (!layered_solving) {
    bottom_found = solve_path(path, false /*only_bool_reasoning*/,
			      path_statements, bottom_pos);
  } else {
    // -- Layered reasoning: we try first to solve the path by using
    //    only boolean reasoning.  If it fails then we use the
    //    corresponding abstract domain.
    bottom_found = solve_path(path, true /*only_bool_reasoning*/,
			      path_statements, bottom_pos);
  
    if (!bottom_found) {
      // clear up m_fwd_dom_map before we solve again the path.
      for(unsigned i=0, e=path.size(); i<e; ++i) {
	basic_block_label_t node = path[i];
	m_fwd_dom_map.erase(node);
      }
      bottom_found = solve_path(path, false /*only_bool_reasoning*/,
				path_statements, bottom_pos);
    } // else {
    //   crab::outs() << "Crab proved infeasibility of the path using only boolean reasoning\n";
    // }
  }
  
  if (bottom_found) {
    if (compute_preconditions) {
      // -- Compute pre-conditions starting from the block for which we
      //    inferred bottom.
      assert (bottom_pos < path.size());
      AbsDom abs_val; 
      bwd_abs_tr_t abs_tr(&abs_val, &stmt_dom_map, true);
      for(int i=bottom_pos; i >= 0; --i) {
	basic_block_label_t node = path[i];
	auto &b = m_cfg.get_node (node);
	for(auto &s: boost::make_iterator_range(b.rbegin(),b.rend())) {
	  s.accept (&abs_tr);
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
// Return true if the core is just the statement "assume(false)"
template<typename CFG, typename AbsDom>    
bool path_analyzer<CFG,AbsDom>::
remove_irrelevant_statements(std::vector<crab::cfg::statement_wrapper>& core) {
  typedef typename CFG::basic_block_t::assume_t assume_t;
  typedef typename CFG::basic_block_t::bool_assume_t bool_assume_t;
  
  unsigned size = core.size();
  basic_block_label_t parent_label = core[size-1].m_parent;
  auto& parent_bb = m_cfg.get_node(parent_label);
  
  // if the core contains at most one constraint then we are done
  if (size <= 1) {
    if (size == 1) {
      if (auto assume = static_cast<const assume_t*>(core[0].m_s)) {
	if (assume->constraint().is_contradiction()) {
	  return true;
	}
      }
    }
    return false;
  }

  // if there is an "assume(false)" then we are done
  for(auto& s: boost::make_iterator_range(parent_bb.rbegin(), parent_bb.rend())) {
    if (s.is_assume()) {
      auto assume = static_cast<assume_t*>(&s);
      if (assume->constraint().is_contradiction()) {
	core.clear();
	core.push_back(typename crab::cfg::statement_wrapper(&s, parent_label));
	return true; 
      }
    }
  }

  // starting from the last statement we search for the first assume
  // statement going backwards. We only search in the last basic
  // block.
  stmt_t* last_assume = nullptr;
  int index = size - 1;
  for(auto& s: boost::make_iterator_range(parent_bb.rbegin(), parent_bb.rend())) {
    if (s.is_assume() || s.is_bool_assume()) {
      last_assume = &s;
      break;
    }
    --index;
  }
    
  if (!last_assume) {
    // we expect last statement to be an assume, otherwise we bail out.
    crab::outs () << "WARNING path analyzer: "
		  << "we couldn't find an assume in the last basic block.\n";
    return false;
  }

  typedef domains::flat_killgen_domain<typename CFG::variable_t> var_dom_t;
  var_dom_t d;
  if (last_assume->is_assume()) {
    auto assume = static_cast<const assume_t*>(last_assume);
    assert(!assume->constraint().is_contradiction());
    for (typename CFG::variable_t v: assume->constraint().variables()) {
      d += v;
    }
  } else {
    auto bool_assume = static_cast<const bool_assume_t*>(last_assume);
    d += bool_assume->cond();
  }

  // Traverse the whole path backwards and remove irrelevant
  // statements based on data dependencies.
  std::vector<bool> enabled(size, false);      
  enabled[index] = true;  // added the last assume
  for(int i= index-1; i >= 0 ; --i) { 
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
    if (enabled[i]) {
      //crab::outs () << "\t" << *(core[i].m_s) << "\n";
      res.push_back(core[i]);
    }
  }
  core.assign(res.begin(), res.end());
  return false;
}
  
  
/** 
 ** Compute a minimal subset of statements needed to prove that the
 ** path is infeasible.
 ** 
 ** Pre: the strongest post-condition of path is false.
 ** Pre: path is well-formed.
 **/
template<typename CFG, typename AbsDom>    
void path_analyzer<CFG,AbsDom>::
minimize_path(const std::vector<crab::cfg::statement_wrapper>& path) {

  std::vector<crab::cfg::statement_wrapper> core(path.begin(), path.end());
  // Remove first syntactically irrelevant statements wrt to the last
  // statement which should be an assume statement where bottom was
  // first inferred.
  
  if (remove_irrelevant_statements(core)) {
    // the path contains "assume(false)"
    m_core.clear();
    m_core.insert(m_core.end(), core.begin(), core.end());
    return;
  }

  
  std::vector<bool> enabled(core.size(), true);
  for (unsigned i=0; i < core.size (); ++i) {
    AbsDom inv;
    fwd_abs_tr_t abs_tr(&inv);    
    for(unsigned j=0; j < core.size(); ++j) {
      if (i != j && enabled[j]) {
	core[j].m_s->accept (&abs_tr);
	if (inv.is_bottom()) {
	  break;
	}
      }
    }
    if (inv.is_bottom()) {
      enabled[i] = false;
    }
  }

  m_core.reserve(core.size());
  for (unsigned i=0; i < core.size();++i) {
    if (enabled[i]) {
      m_core.push_back(core[i]);
    }
  }

  // sanity checks  
  assert(!m_core.empty());
  if (false) { // disable by default
    AbsDom inv;
    fwd_abs_tr_t abs_tr(&inv);        
    bool is_bottom = false;
    for (unsigned i=0, e=m_core.size(); i<e; ++i) {
      m_core[i].m_s->accept (&abs_tr);
      if (inv.is_bottom()) {
	is_bottom=true;
	break;
      }
    }
    if (!is_bottom) {
      CRAB_ERROR("Abstract core is not unsat!\n");
    }
  }
}
}
}

#include <crab_llvm/crab_domains.hh>
#include <crab_llvm/crab_cfg.hh>

namespace crab {
namespace analyzer {
// explicit instantiations
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::num_domain_t>;
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::split_dbm_domain_t>;
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::boxes_domain_t>;
#ifdef HAVE_ALL_DOMAINS  
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::term_int_domain_t>;
#endif   
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::interval_domain_t>;
template class path_analyzer<crab_llvm::cfg_ref_t, crab_llvm::wrapped_interval_domain_t>;      
} 
} 

