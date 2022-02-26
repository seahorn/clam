#pragma once

/*
 * Definition of Crab Control Flow Graph (CFG) and Call Graph using
 * const llvm::Value* as variable names and const llvm::BasicBlock* as
 * basic block labels.
 */

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

#include "crab/analysis/dataflow/liveness.hpp"
#include "crab/cfg/cfg.hpp"
#include "crab/cg/cg.hpp"
#include "crab/types/indexable.hpp"
#include "crab/types/varname_factory.hpp"
#include <crab/cfg/basic_block_traits.hpp>

#include <functional>
#include <memory>

namespace clam {

// This wrapper is needed because we can have crab blocks which do not
// correspond to llvm blocks.
class llvm_basic_block_wrapper : public crab::indexable {
public:
  // the new block represents that the control is at b
  llvm_basic_block_wrapper(const llvm::BasicBlock *b, std::size_t id)
      : m_bb(b), m_edge(nullptr, nullptr), m_name(b->getName()), m_id(id) {
    assert(b->hasName());
  }

  // the new block represents that the control goes from src to dst
  llvm_basic_block_wrapper(const llvm::BasicBlock *src,
                           const llvm::BasicBlock *dst, std::string name,
                           std::size_t id)
      : m_bb(nullptr), m_edge(src, dst), m_name(name), m_id(id) {}

  llvm_basic_block_wrapper()
      : m_bb(nullptr), m_edge(nullptr, nullptr), m_name(""), m_id(0) {}

  // for boost bgl
  llvm_basic_block_wrapper(std::nullptr_t)
      : m_bb(nullptr), m_edge(nullptr, nullptr), m_name(""), m_id(0) {}

  std::string get_name() const { return m_name; }

  bool is_edge() const { return !m_bb && (m_edge.first && m_edge.second); }

  const llvm::BasicBlock *get_basic_block() const { return m_bb; }

  const std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *> &
  get_edge() const {
    return m_edge;
  }

  bool operator==(const llvm_basic_block_wrapper &other) const {
    return m_name == other.m_name;
  }

  bool operator!=(const llvm_basic_block_wrapper &other) const {
    return !(this->operator==(other));
  }

  bool operator<(const llvm_basic_block_wrapper &other) const {
    return m_name < other.m_name;
  }

  std::size_t hash() const { return std::hash<std::string>{}(m_name); }

  // used by some crab datastructures
  virtual ikos::index_t index() const override { return m_id; }

  virtual void write(crab::crab_os &o) const override { o << get_name(); }

private:
  //
  // class invariant: a block wrapper corresponds to either a basic
  // block or edge, but not both.
  //

  // the block wrapper corresponds to a llvm basic block
  const llvm::BasicBlock *m_bb;
  // the block wrapper corresponds to a llvm edge
  std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *> m_edge;
  // block wrapper name
  std::string m_name;
  // block wrapper unique identifier
  std::size_t m_id;
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &o,
                                     const llvm_basic_block_wrapper &b) {
  o << b.get_name();
  return o;
}

inline crab::crab_os &operator<<(crab::crab_os &o,
                                 const llvm_basic_block_wrapper &b) {
  b.write(o);
  return o;
}
} // namespace clam

namespace std {
template <> struct hash<clam::llvm_basic_block_wrapper> {
  size_t operator()(const clam::llvm_basic_block_wrapper &bb) const {
    return bb.hash();
  }
};
} // namespace std

namespace clam {
/** Define a Crab CFG and call graph over integers **/
using variable_factory_t = crab::var_factory_impl::variable_factory<const llvm::Value*>; 
using varname_t = typename variable_factory_t::varname_t;
using number_t = ikos::z_number;
using var_t = crab::variable<number_t, varname_t>;
using var_or_cst_t = crab::variable_or_constant<number_t, varname_t>;
using basic_block_label_t = llvm_basic_block_wrapper;
using cfg_t = crab::cfg::cfg<basic_block_label_t, varname_t, number_t>;
using cfg_ref_t = crab::cfg::cfg_ref<cfg_t>;
using basic_block_t = cfg_t::basic_block_t;
using statement_t = typename cfg_t::basic_block_t::statement_t;
using lin_exp_t = typename cfg_t::basic_block_t::lin_exp_t;
using lin_cst_t = typename cfg_t::basic_block_t::lin_cst_t;
using ref_cst_t = crab::reference_constraint<number_t, varname_t>;
using lin_cst_sys_t = ikos::linear_constraint_system<number_t, varname_t>;
using disj_lin_cst_sys_t =
    ikos::disjunctive_linear_constraint_system<number_t, varname_t>;
using cg_t = crab::cg::call_graph<cfg_ref_t>;

using lin_exp_unordered_set =
    ikos::linear_expression_unordered_set<number_t, varname_t>;
using lin_cst_unordered_set =
    ikos::linear_constraint_unordered_set<number_t, varname_t>;
template <typename Value>
using lin_exp_unordered_map =
    ikos::linear_expression_unordered_map<number_t, varname_t, Value>;
template <typename Value>
using lin_cst_unordered_map =
    ikos::linear_constraint_unordered_map<number_t, varname_t, Value>;

using liveness_t = crab::analyzer::live_and_dead_analysis<cfg_ref_t>;
using varset_t = typename liveness_t::varset_domain_t;
} // end namespace clam

namespace crab {
template <> class variable_name_traits<const llvm::Value *> {
public:
  static std::string to_string(const llvm::Value *v) {
    return v->getName().str();
  }
};

template <> class variable_name_traits<std::string> {
public:
  static std::string to_string(std::string v) { return v; }
};

template <> class basic_block_traits<clam::basic_block_t> {
public:
  using basic_block_label_t = clam::basic_block_t::basic_block_label_t;
  static std::string to_string(const basic_block_label_t &bbl) {
    return bbl.get_name();
  }
};
} // end namespace crab

namespace {
inline llvm::raw_ostream &operator<<(llvm::raw_ostream &o,
                                     const clam::cfg_t &cfg) {
  crab::crab_string_os s;
  s << cfg;
  o << s.str();
  return o;
}

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &o,
                                     clam::cfg_ref_t cfg) {
  crab::crab_string_os s;
  s << cfg;
  o << s.str();
  return o;
}
} // namespace
