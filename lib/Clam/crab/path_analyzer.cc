#include "path_analyzer.hpp"
#include <clam/crab/crab_lang.hh>
#include <crab/domains/discrete_domains.hpp>
#include <crab/support/debug.hpp>

#include <unordered_set>

namespace crab {
namespace analyzer {

static bool do_sanity_check = true;
static bool only_syntactic_core = false;
// remove irrelevant constraints in two phases: first, by considering
// only data dependencies. This usually creates small set of
// statements but they might not imply bottom. If not, then we ignore
// them and try again but this time considering both data and control
// dependencies.
static bool speculative_only_data = true;

template <typename CFG, typename AbsDom>
path_analyzer<CFG, AbsDom>::path_analyzer(CFG cfg, AbsDom init)
    : m_cfg(cfg), m_init(init) {}

template <typename CFG, typename AbsDom>
bool path_analyzer<CFG, AbsDom>::solve_path(
    const std::vector<basic_block_label_t> &path,
    const bool only_bool_reasoning, std::vector<statement_t *> &path_statements,
    // block where bottom was detected
    unsigned &bottom_block,
    // first statement  in bottom_block-nth block where bottom was detected
    unsigned &bottom_stmt) {

  bool bottom_found = false;
  bottom_block = path.size();

  AbsDom pre(m_init);
  fwd_abs_tr_t abs_tr(std::move(pre));
  for (unsigned i = 0, e = path.size(); i < e; ++i) {
    const AbsDom &new_pre = abs_tr.get_abs_value();
    if (new_pre.is_bottom()) {
      break;
    }
    // store constraints that hold at the entry of the block
    basic_block_label_t node = path[i];
    auto it = m_fwd_dom_map.find(node);
    if (it == m_fwd_dom_map.end()) {
      m_fwd_dom_map.insert(std::make_pair(node, new_pre));
    }

    // compute strongest post-condition for one block
    auto &b = m_cfg.get_node(node);
    bottom_stmt = 0;
    for (auto &s : b) {
      if (only_bool_reasoning) {
        if (!(s.is_bool_bin_op() || s.is_bool_assign_cst() ||
              s.is_bool_assign_var() || s.is_bool_assume() ||
              s.is_bool_assert() || s.is_bool_select())) {
          bottom_stmt++;
          continue;
        }
      }
      
      //if (!s.is_assert() && !s.is_ref_assert() && !s.is_bool_assert()) {
      path_statements.push_back(&s);
      //}
      
      CRAB_LOG("path-analyzer",
	       crab::outs() << "CRAB interpreting " << s << "\n";);
      
      s.accept(&abs_tr);

      if (abs_tr.get_abs_value().is_bottom()) {
	if (!bottom_found) {
	  bottom_block = i; // update only the first time bottom is found
	}
	bottom_found = true;
        break;
      } else {
        bottom_stmt++;
      }
    }
  }
  return bottom_found;
}

template <typename CFG, typename AbsDom>
bool path_analyzer<CFG, AbsDom>::solve(
    const std::vector<basic_block_label_t> &path, bool layered_solving) {

  // Reset state
  m_fwd_dom_map.clear();
  m_core.clear();

  if (path.empty()) {
    CRAB_WARN("Empty path: do nothing\n");
    return true;
  }

#if 0
  if (do_sanity_check) {
    // Sanity checks
    basic_block_label_t first = path.front();
    basic_block_label_t last = path.back();  
    if (m_cfg.entry() != first)
      CRAB_ERROR("First block of the path must be the entry block of the cfg");
    if (m_cfg.has_exit() && (m_cfg.exit() != last))
      CRAB_ERROR("Last block of the path must be the exit block of the cfg");
  }
#endif

  if (path.size() > 1) {
    std::unordered_set<basic_block_label_t> visited;
    for (unsigned i = 0; i < path.size(); ++i) {
      if (!visited.insert(path[i]).second) {
        CRAB_ERROR("The path is not acyclic");
      }
      if (i < path.size() - 1) {
        if (!has_kid(path[i], path[i + 1])) {
	  // Enable temporarily warning messages
	  bool flag = crab::CrabWarningFlag;
	  crab::CrabEnableWarningMsg(true);
          CRAB_WARN("There is no an edge from ",
              crab::basic_block_traits<clam::basic_block_t>::to_string(path[i]),
              " to ",
		    crab::basic_block_traits<clam::basic_block_t>::to_string(path[i + 1]));
	  crab::CrabEnableWarningMsg(flag);
          return true;
        }
      }
    }
  }

  // contain all statements along the path until the end of the path
  // or bottom is found.
  std::vector<statement_t *> path_statements;
  // Compute strongest post-condition over the path
  unsigned bottom_block; // block where bottom was detected
  unsigned
      bottom_stmt; // first statement in bottom_block where bottom was detected
  bool bottom_found;
  if (!layered_solving) {
    bottom_found = solve_path(path, false /*only_bool_reasoning*/,
                              path_statements, bottom_block, bottom_stmt);
  } else {
    // -- Layered reasoning: we try first to solve the path by using
    //    only boolean reasoning.  If it fails then we use the
    //    corresponding abstract domain.
    bottom_found = solve_path(path, true /*only_bool_reasoning*/,
                              path_statements, bottom_block, bottom_stmt);

    if (!bottom_found) {
      // clear up m_fwd_dom_map before we solve again the path.
      for (unsigned i = 0, e = path.size(); i < e; ++i) {
        basic_block_label_t node = path[i];
        m_fwd_dom_map.erase(node);
      }
      bottom_found = solve_path(path, false /*only_bool_reasoning*/,
                                path_statements, bottom_block, bottom_stmt);
    } 
  }

  if (bottom_found) {
    // -- Compute minimal subset of statements that still implies
    //    bottom.
    if (!minimize_path(path_statements, bottom_stmt)) {
      // If we cannot explain why the path is bottom the we return
      // true.
      return true;
    }
  }
  return !bottom_found;
}

template <typename CFG, typename AbsDom>
bool path_analyzer<CFG, AbsDom>::has_kid(basic_block_label_t b1,
                                         basic_block_label_t b2) {
  for (basic_block_label_t child : m_cfg.next_nodes(b1)) {
    if (child == b2)
      return true;
  }
  return false;
}

// Compute a minimal subset of statements based on syntactic
// dependencies.
// Return true if the core is just the statement "assume(false)"
template <typename CFG, typename AbsDom>
typename path_analyzer<CFG, AbsDom>::FalsePathInfo
path_analyzer<CFG, AbsDom>::remove_irrelevant_statements(
    std::vector<statement_t *> &core, unsigned bottom_stmt,
    bool only_data_dependencies) {
  
  using assume_t = typename CFG::basic_block_t::assume_t;
  using bool_assume_t = typename CFG::basic_block_t::bool_assume_t;
  using ref_assume_t = typename CFG::basic_block_t::assume_ref_t;
  using assert_t = typename CFG::basic_block_t::assert_t;
  using bool_assert_t = typename CFG::basic_block_t::bool_assert_t;
  using ref_assert_t = typename CFG::basic_block_t::assert_ref_t;  
  
  using var_dom_t = ikos::discrete_domain<typename CFG::variable_t>;

  auto isAssumeFalse = [](const statement_t &s) {
			 if (s.is_assume()) {
			   auto assume = static_cast<const assume_t *>(&s);
			   return (assume->constraint().is_contradiction());
			 } else if (s.is_ref_assume()) {
			   auto ref_assume = static_cast<const ref_assume_t *>(&s);
			   return ref_assume->constraint().is_contradiction();
			 } else {
			   // Note that bool_assume(false) is not part
			   // of the CrabIR.
			   return false;
			 }
  };

  auto getSliceCriteriaVariables = [](const statement_t &s) -> var_dom_t {
     // we consider both assume and assert statements
     var_dom_t res;
     
     if (s.is_assume()) {
       auto assume = static_cast<const assume_t *>(&s);
       for (typename CFG::variable_t v : assume->constraint().variables()) {
	 res += v;
       }
     } else if (s.is_bool_assume()) {
       auto bool_assume = static_cast<const bool_assume_t *>(&s);
       res += bool_assume->cond();
     } else if (s.is_ref_assume()) {
       auto ref_assume = static_cast<const ref_assume_t *>(&s);
       for (typename CFG::variable_t v : ref_assume->constraint().variables()) {
	 res += v;
       }
     } else if (s.is_assert()) {
       auto assrt = static_cast<const assert_t *>(&s);
       for (typename CFG::variable_t v : assrt->constraint().variables()) {
	 res += v;
       }
     } else if (s.is_bool_assert()) {
       auto bool_assrt = static_cast<const bool_assert_t *>(&s);
       res += bool_assrt->cond();
     } else if (s.is_ref_assert()) {
       auto ref_assrt = static_cast<const ref_assert_t *>(&s);
       for (typename CFG::variable_t v : ref_assrt->constraint().variables()) {
	 res += v;
       }
     }
     
     return res;
  };
  
  unsigned size = core.size();
  basic_block_t *parent_bb = core[size - 1]->get_parent();
  assert(parent_bb);

  // if the core contains at most one constraint then we are done
  if (size <= 1) {
    if (size == 1) {
      if (isAssumeFalse(*(core[0]))) {
	return FalsePathInfo::TRIVIAL_EXPLANATION;
      }
    }
    return FalsePathInfo::EXPLANATION;
  }

  // if there is an "assume(false)" then we are done
  for (auto it = parent_bb->rbegin(), et = parent_bb->rend(); it != et; ++it) {
    auto &s = *it;
    if (isAssumeFalse(s)) {
      core.clear();
      core.push_back(&s);
      return FalsePathInfo::TRIVIAL_EXPLANATION;
    }
  }

  // Find the statement where bottom was first detected. We just need
  // to look at the last block.
  statement_t *last_stmt = nullptr;
  unsigned i = 0;
  for (auto it = parent_bb->begin(), et = parent_bb->end(); it != et; ++it) {
    auto &s = *it;
    if (i == bottom_stmt) {
      last_stmt = &s;
    }
    i++;
  }

  if (!last_stmt) {
    // This probably shouldn't happen
    CRAB_LOG("path-analyzer",    
	     crab::outs() << "Bail out: cannot find statement where path become false\n";);
    return FalsePathInfo::NOT_EXPLANATION;
  }

  // start from the last statement and go reverse skipping statements
  // until last_stmt is found.
  int j = size - 1;
  for (int i = size - 1; i >= 0; --i) {
    statement_t &s = *(core[i]);
    if (last_stmt == &s) {
      break;
    } else {
      j--;
    }
  }

  var_dom_t d = getSliceCriteriaVariables(*last_stmt);
  if (d.is_bottom()) {
    // One common reason is that the last statement is assert(false)
    CRAB_LOG("path-analyzer",
	     crab::outs() << "Bail out: cannot use " << *last_stmt << " to produce a slicing target\n");
    return FalsePathInfo::NOT_EXPLANATION;
  }

  CRAB_LOG("path-analyzer",
	   crab::outs() << "Slicing criteria: " << *last_stmt << " with variables " << d << "\n";);

  // Traverse the whole path backwards and remove irrelevant
  // statements based on data dependencies.
  std::vector<bool> enabled(size, false);
  enabled[j] = true; // marked last_stmt
  for (int i = j - 1; i >= 0; --i) {
    statement_t &s = *(core[i]);
    var_dom_t uses, defs;
    const typename statement_t::live_t &ls = s.get_live();
    for (auto it = ls.uses_begin(), et = ls.uses_end(); it != et; ++it) {
      uses += *it;
    }
    for (auto it = ls.defs_begin(), et = ls.defs_end(); it != et; ++it) {
      defs += *it;
    }

    if (defs.is_bottom() && !uses.is_bottom()) {
      // control and data dependencies
      if (!only_data_dependencies || !(uses & d).is_bottom()) {
        d += uses;
        enabled[i] = true;
      } else {
        // irrelevant statement
      }
    } else if (!(d & defs).is_bottom()) {
      // data dependencies
      d -= defs;
      d += uses;
      enabled[i] = true;
    } else {
      // irrelevant statement
    }
  }

  CRAB_LOG("path-analyzer",
	   crab::outs() << "SYNTACTIC UNSAT CORE PATH ";
	   if (only_data_dependencies) {
	     crab::outs() << " (only data dependencies)";
	   } else {
	     crab::outs() << " (data+control dependencies)";	     
	   }
	   crab::outs() << ":\n";);    
	   
  std::vector<statement_t *> res;
  res.reserve(size);
  for (unsigned i = 0; i < size; ++i) {
    if (enabled[i]) {
      CRAB_LOG("path-analyzer",      
	       crab::outs() << "\t" << *(core[i]) << "\n";);
      res.push_back(core[i]);
    }
  }
  core.assign(res.begin(), res.end());
  return FalsePathInfo::EXPLANATION;
}

/**
 ** Return true if it can compute a minimal subset of statements
 ** needed to prove that the path is false (i.e., bottom).
 **
 ** Pre: the strongest post-condition of path is false.
 ** Pre: path is well-formed.
 **/
template <typename CFG, typename AbsDom>
bool path_analyzer<CFG, AbsDom>::minimize_path(
    const std::vector<statement_t *> &path, unsigned bottom_stmt) {

  std::vector<statement_t *> core(path.begin(), path.end());

  if (only_syntactic_core) {
    /* only syntactic */
    FalsePathInfo res = remove_irrelevant_statements(core, bottom_stmt,
						  false /*data+control dependencies*/);
    m_core.clear();
    if (res == FalsePathInfo::NOT_EXPLANATION) {
      // we give up here because the path might be false because some
      // assert(false) was found but the assertion is not actually
      // reachable under a more precise abstraction.
      return false;
    }
    m_core.insert(m_core.end(), core.begin(), core.end());
  } else {
    /* syntactic + semantic */

    // Remove first syntactically irrelevant statements wrt to the last
    // statement which should be an assume statement where bottom was
    // first inferred.
    bool only_data = speculative_only_data;
    for (unsigned i = 0; i < 2; ++i) {
      if (i == 1) {
        core.assign(path.begin(), path.end());
      }

      FalsePathInfo res = remove_irrelevant_statements(core, bottom_stmt, only_data);
      
      if (res == FalsePathInfo::TRIVIAL_EXPLANATION) {
        // the path contains "assume(false)"
        m_core.clear();
        m_core.insert(m_core.end(), core.begin(), core.end());
        return true;
      } else if (res == FalsePathInfo::NOT_EXPLANATION) {
	m_core.clear();
	return false;
      }
      
      if (only_data) {
        // check that what we get still implies bottom: it might not
        // imply bottom because we only considered data dependencies.
        AbsDom inv = m_init.make_top();
        fwd_abs_tr_t abs_tr(std::move(inv));
        bool is_bottom = false;
        for (unsigned i = 0, e = core.size(); i < e; ++i) {
          core[i]->accept(&abs_tr);
          if (abs_tr.get_abs_value().is_bottom()) {
            is_bottom = true;
            break;
          }
        }
        if (is_bottom) {
          break;
        } else {
          // try next iteration considering data+control dependencies.
          only_data = false;
        }
      } else {
        break;
      }
    } // end for

    std::vector<bool> enabled(core.size(), true);
    for (unsigned i = 0; i < core.size(); ++i) {
      AbsDom inv = m_init.make_top();
      fwd_abs_tr_t abs_tr(std::move(inv));
      for (unsigned j = 0; j < core.size(); ++j) {
        if (i != j && enabled[j]) {
          core[j]->accept(&abs_tr);
          if (abs_tr.get_abs_value().is_bottom()) {
            break;
          }
        }
      }
      if (abs_tr.get_abs_value().is_bottom()) {
        enabled[i] = false;
      }
    }

    CRAB_LOG("path-analyzer",
	     crab::outs() << "SEMANTIC UNSAT CORE PATH:\n";);

    m_core.reserve(core.size());
    for (unsigned i = 0; i < core.size(); ++i) {
      if (enabled[i]) {
        m_core.push_back(core[i]);
	CRAB_LOG("path-analyzer",	  
		 crab::outs() << "\t" << *(core[i]) << "\n";);
      }
    }

    if (do_sanity_check) {
      AbsDom inv = m_init.make_top();
      fwd_abs_tr_t abs_tr(std::move(inv));
      bool is_bottom = false;
      for (unsigned i = 0, e = m_core.size(); i < e; ++i) {
        m_core[i]->accept(&abs_tr);
        if (abs_tr.get_abs_value().is_bottom()) {
          is_bottom = true;
          break;
        }
      }
      if (!is_bottom) {
        CRAB_ERROR("Abstract core is not unsat!");
      }
    }
  }
  return true;
}
} // namespace analyzer
} // namespace crab

#include <clam/Clam.hh>

namespace crab {
namespace analyzer {
// explicit instantiations
template class path_analyzer<clam::cfg_ref_t, clam::clam_abstract_domain>;
} // end namespace analyzer
} // end namespace crab
