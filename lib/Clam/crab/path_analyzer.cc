#include "path_analyzer.hpp"
#include "clam/crab/crab_lang.hh"

#include <crab/domains/killgen_domain.hpp>

#include <unordered_set>

namespace crab {
namespace analyzer {

static bool do_sanity_check = true;
static bool do_debugging = false;
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
    const bool only_bool_reasoning,
    std::vector<statement_t*> &path_statements,
    // block where bottom was detected
    unsigned &bottom_block,
    // first statement  in bottom_block-nth block where bottom was detected
    unsigned &bottom_stmt) {

  bool bottom_found = false;
  bottom_block = path.size();

  AbsDom pre(m_init);
  fwd_abs_tr_t abs_tr(std::move(pre));
  for (unsigned i = 0, e = path.size(); i < e; ++i) {
    AbsDom new_pre(std::move(abs_tr.get_abs_value()));
    if (new_pre.is_bottom()) {
      if (!bottom_found) {
        bottom_block = i; // update only the first time bottom is found
      }
      bottom_found = true;
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
      if (!s.is_assert() && !s.is_ref_assert() && !s.is_bool_assert()) {
        path_statements.push_back(&s);
      }
      s.accept(&abs_tr);
      AbsDom next_pre(std::move(abs_tr.get_abs_value()));
      if (next_pre.is_bottom()) {
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
          CRAB_WARN("There is no an edge from ",
                    crab::basic_block_traits<clam::basic_block_t>::to_string(path[i]), " to ",
                    crab::basic_block_traits<clam::basic_block_t>::to_string(path[i + 1]));
          return true;
        }
      }
    }
  }

  // contain all statements along the path until the end of the path
  // or bottom is found.
  std::vector<statement_t*> path_statements;
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
    } // else {
    //   crab::outs() << "Crab proved infeasibility of the path using only
    //   boolean reasoning\n";
    // }
  }

  if (bottom_found) {
    // -- Compute minimal subset of statements that still implies
    //    bottom.
    minimize_path(path_statements, bottom_stmt);
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
bool path_analyzer<CFG, AbsDom>::remove_irrelevant_statements(
    std::vector<statement_t*> &core, unsigned bottom_stmt,
    bool only_data_dependencies) {
  typedef typename CFG::basic_block_t::assume_t assume_t;
  typedef typename CFG::basic_block_t::bool_assume_t bool_assume_t;

  unsigned size = core.size();
  basic_block_t *parent_bb = core[size - 1]->get_parent();
  assert(parent_bb);
  
  if (do_debugging) {
    crab::outs() << "CRAB PATH:\n";
    for (auto s : core) {
      crab::outs() << "\t" << *s << "\n";
    }
  }

  // if the core contains at most one constraint then we are done
  if (size <= 1) {
    if (size == 1) {
      if (auto assume = static_cast<const assume_t *>(core[0])) {
        if (assume->constraint().is_contradiction()) {
          return true;
        }
      }
    }
    return false;
  }

  // if there is an "assume(false)" then we are done
  for (auto it = parent_bb->rbegin(), et = parent_bb->rend(); it != et; ++it) {
    auto &s = *it;
    if (s.is_assume()) {
      auto assume = static_cast<assume_t *>(&s);
      if (assume->constraint().is_contradiction()) {
        core.clear();
        core.push_back(&s);
        return true;
      }
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

  if (!last_stmt || (!last_stmt->is_assume() && !last_stmt->is_bool_assume())) {
    crab::outs() << "WARNING path analyzer: "
                 << "we couldn't find an assume in the last basic block.\n";
    return false;
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

  typedef domains::flat_killgen_domain<typename CFG::variable_t> var_dom_t;
  var_dom_t d;
  if (last_stmt->is_assume()) {
    auto assume = static_cast<const assume_t *>(last_stmt);
    assert(!assume->constraint().is_contradiction());
    for (typename CFG::variable_t v : assume->constraint().variables()) {
      d += v;
    }
  } else {
    auto bool_assume = static_cast<const bool_assume_t *>(last_stmt);
    d += bool_assume->cond();
  }

  if (do_debugging) {
    crab::outs() << "Slicing criteria: " << *last_stmt << "\n";
  }

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

  if (do_debugging) {
    crab::outs() << "SYNTACTIC UNSAT CORE PATH:\n";
  }
  std::vector<statement_t*> res;
  res.reserve(size);
  for (unsigned i = 0; i < size; ++i) {
    if (enabled[i]) {
      if (do_debugging) {
        crab::outs() << "\t" << *(core[i]) << "\n";
      }
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
template <typename CFG, typename AbsDom>
void path_analyzer<CFG, AbsDom>::minimize_path(
    const std::vector<statement_t*> &path,
    unsigned bottom_stmt) {

  std::vector<statement_t*> core(path.begin(), path.end());

  if (only_syntactic_core) {
    /* only syntactic */
    remove_irrelevant_statements(core, bottom_stmt,
                                 false /*data+control dependencies*/);
    m_core.clear();
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

      if (remove_irrelevant_statements(core, bottom_stmt, only_data)) {
        // the path contains "assume(false)"
        m_core.clear();
        m_core.insert(m_core.end(), core.begin(), core.end());
        return;
      }

      if (only_data) {
        // check that what we get still implies bottom: it might not
        // imply bottom because we only considered data dependencies.
        AbsDom inv = m_init.make_top();
        fwd_abs_tr_t abs_tr(std::move(inv));
        bool is_bottom = false;
        for (unsigned i = 0, e = core.size(); i < e; ++i) {
          core[i]->accept(&abs_tr);
          if (inv.is_bottom()) {
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
    }

    std::vector<bool> enabled(core.size(), true);
    for (unsigned i = 0; i < core.size(); ++i) {
      AbsDom inv = m_init.make_top();
      fwd_abs_tr_t abs_tr(std::move(inv));
      for (unsigned j = 0; j < core.size(); ++j) {
        if (i != j && enabled[j]) {
          core[j]->accept(&abs_tr);
          AbsDom next_inv = std::move(abs_tr.get_abs_value());
          if (next_inv.is_bottom()) {
            break;
          }
        }
      }
      AbsDom next_inv = std::move(abs_tr.get_abs_value());
      if (next_inv.is_bottom()) {
        enabled[i] = false;
      }
    }

    if (do_debugging) {
      crab::outs() << "SEMANTIC UNSAT CORE PATH:\n";
    }

    m_core.reserve(core.size());
    for (unsigned i = 0; i < core.size(); ++i) {
      if (enabled[i]) {
        m_core.push_back(core[i]);
        if (do_debugging) {
          crab::outs() << "\t" << core[i] << "\n";
        }
      }
    }

    if (do_sanity_check) {
      AbsDom inv = m_init.make_top();
      fwd_abs_tr_t abs_tr(std::move(inv));
      bool is_bottom = false;
      for (unsigned i = 0, e = m_core.size(); i < e; ++i) {
        m_core[i]->accept(&abs_tr);
        AbsDom next_inv = std::move(abs_tr.get_abs_value());
        if (next_inv.is_bottom()) {
          is_bottom = true;
          break;
        }
      }
      if (!is_bottom) {
        CRAB_ERROR("Abstract core is not unsat!");
      }
    }
  }
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

