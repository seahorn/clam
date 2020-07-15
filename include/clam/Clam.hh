#pragma once

/*
 * Infer invariants using Crab.
 */

#include "clam/ClamAnalysisParams.hh"
#include "clam/crab/crab_lang.hh"
#include "crab/checkers/base_property.hpp"
#include "crab/domains/generic_abstract_domain.hpp"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Pass.h"

#include <memory>

// forward declarations
namespace clam {
class IntraClamImpl;
class InterClamImpl;
class CrabBuilderManager;
} // namespace clam

namespace clam {

// A wrapper for an arbitrary abstract domain, cheap to copy
using clam_abstract_domain = crab::domains::abstract_domain_ref<var_t>;

/**
 * Intra-procedural analysis of a function
 *
 * Basic usage:
 *    // Create a crab cfg builder manager
 *    CrabBuilderParams params;
 *    auto tli = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
 *    std::unique_ptr<HeapAbstraction> mem(new DummyHeapAbstraction());
 *    CrabBuilderManager man(params, tli, std::move(mem));
 *    // Create an intra-procedural analysis
 *    IntraClam ic(fun, man);
 *
 *    AnalysisParams params;
 *    ic.analyze(params);
 *    for (auto &b: fun) {
 *      if (clam_abstract_domain dom = ic.getPre(&b)) {
 *         crab::outs << dom << "\n";
 *      }
 *    }
 **/
class IntraClam {
public:
  using abs_dom_map_t =
      llvm::DenseMap<const llvm::BasicBlock *, clam_abstract_domain>;
  using lin_csts_map_t =
      llvm::DenseMap<const llvm::BasicBlock *, lin_cst_sys_t>;
  using checks_db_t = crab::checker::checks_db;

private:
  std::unique_ptr<IntraClamImpl> m_impl;
  CrabBuilderManager &m_builder_man;
  const llvm::Function &m_fun;
  abs_dom_map_t m_pre_map;
  abs_dom_map_t m_post_map;
  checks_db_t m_checks_db;

public:
  /**
   * Constructor that builds a crab CFG
   **/
  IntraClam(const llvm::Function &fun, CrabBuilderManager &man);

  ~IntraClam();

  /**
   * Clear all the internal state
   **/
  void clear();

  /* return the manager used to build all CFGs */
  CrabBuilderManager &getCfgBuilderMan();

  /**
   * Call crab analysis on the CFG under assumptions.
   **/
  void analyze(AnalysisParams &params,
               const abs_dom_map_t &assumptions = abs_dom_map_t());

  /**
   * Call crab analysis on the CFG under assumptions starting from entry
   **/
  void analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
               const abs_dom_map_t &assumptions);

  /**
   * Call crab analysis on the CFG under assumptions.
   **/
  void analyze(AnalysisParams &params, const lin_csts_map_t &assumptions);

  /**
   * Call crab analysis on the CFG under assumptions starting from entry
   **/
  void analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
               const lin_csts_map_t &assumptions);

  /**
   * Compute strongest post-condition of an acyclic path.
   * Return false iff the path implies false.
   *
   * post contains the post-conditions at each block.
   * If it returns false then:
   *   - core is a minimal subset of statements that implies false
   **/
  bool
  pathAnalyze(const AnalysisParams &params,
              const std::vector<const llvm::BasicBlock *> &path,
              /* use gradually more expensive domains until unsat is proven*/
              bool layered_solving, std::vector<statement_t *> &core,
              abs_dom_map_t &post) const;

  bool
  pathAnalyze(const AnalysisParams &params,
              const std::vector<const llvm::BasicBlock *> &path,
              /* use gradually more expensive domains until unsat is proven*/
              bool layered_solving, std::vector<statement_t *> &core) const;

  /**
   * Return invariants that hold at the entry of b
   **/
  llvm::Optional<clam_abstract_domain> getPre(const llvm::BasicBlock *b,
                                              bool keep_shadows = false) const;

  /**
   * Return invariants that hold at the exit of b
   **/
  llvm::Optional<clam_abstract_domain> getPost(const llvm::BasicBlock *b,
                                               bool keep_shadows = false) const;

  /**
   * Return a database with all checks.
   **/
  const checks_db_t &getChecksDB() const;
};

/**
 * Inter-procedural analysis of a module
 **/
class InterClam {

public:
  using abs_dom_map_t = typename IntraClam::abs_dom_map_t;
  using lin_csts_map_t = typename IntraClam::lin_csts_map_t;
  using checks_db_t = typename IntraClam::checks_db_t;

private:
  std::unique_ptr<InterClamImpl> m_impl;
  CrabBuilderManager &m_builder_man;
  abs_dom_map_t m_pre_map;
  abs_dom_map_t m_post_map;
  checks_db_t m_checks_db;

public:
  /**
   * Constructor that builds a crab call graph.
   **/
  InterClam(const llvm::Module &module, CrabBuilderManager &man);

  ~InterClam();

  /* return the manager used to build all CFGs */
  CrabBuilderManager &getCfgBuilderMan();

  /**
   * Clear all the internal state
   **/
  void clear();

  /**
   * Call crab analysis on the call graph under assumptions.
   **/
  void analyze(AnalysisParams &params, const abs_dom_map_t &assumptions);

  /**
   * Call crab analysis on the call graph under assumptions.
   **/
  void analyze(AnalysisParams &params, const lin_csts_map_t &assumptions);

  /**
   * Return invariants that hold at the entry of b
   **/
  llvm::Optional<clam_abstract_domain> getPre(const llvm::BasicBlock *b,
                                              bool keep_shadows = false) const;

  /**
   * Return invariants that hold at the exit of b
   **/
  llvm::Optional<clam_abstract_domain> getPost(const llvm::BasicBlock *b,
                                               bool keep_shadows = false) const;

  /**
   * Return a database with all checks.
   **/
  const checks_db_t &getChecksDB() const;
};

/**
 * LLVM Module pass that computes invariants using Crab.
 **/
class ClamPass : public llvm::ModulePass {

  using abs_dom_map_t = typename IntraClam::abs_dom_map_t;
  using checks_db_t = typename IntraClam::checks_db_t;

  abs_dom_map_t m_pre_map;
  abs_dom_map_t m_post_map;
  std::unique_ptr<CrabBuilderManager> m_cfg_builder_man;
  checks_db_t m_checks_db;
  AnalysisParams m_params;

public:
  static char ID;

  ClamPass();

  /* begin ModulePass API */
  virtual void releaseMemory();

  virtual bool runOnModule(llvm::Module &M);

  virtual bool runOnFunction(llvm::Function &F);

  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const;

  virtual llvm::StringRef getPassName() const {
    return "Clam: Crab for Llvm Abstraction Manager";
  }
  /* end ModulePass API */

  /* return the manager used to build all CFGs */
  CrabBuilderManager &getCfgBuilderMan();

  /* return the analysis options */
  const AnalysisParams &getAnalysisParams() const { return m_params; }

  /* return true if there is Crab CFG for F */
  bool hasCfg(llvm::Function &F);

  /* return the Crab CFG associated to F */
  cfg_ref_t getCfg(llvm::Function &F);

  /**
   * return invariants that hold at the entry of BB
   **/
  llvm::Optional<clam_abstract_domain> getPre(const llvm::BasicBlock *BB,
                                              bool KeepShadows = false) const;

  /**
   * return invariants that hold at the exit of BB
   **/
  llvm::Optional<clam_abstract_domain> getPost(const llvm::BasicBlock *BB,
                                               bool KeepShadows = false) const;

  /**
   * To query and view the analysis results
   **/

  /* return total number of checks if assertion checker enabled,
     otherwise 0 */
  unsigned getTotalChecks() const;
  /* return total number of safe checks if assertion checker
     enabled, otherwise 0 */
  unsigned getTotalSafeChecks() const;
  /* return total number of definite error checks if assertion
     checker enabled, otherwise 0 */
  unsigned getTotalErrorChecks() const;
  /* return total number of possibly error checks if assertion
     checker enabled, otherwise 0 */
  unsigned getTotalWarningChecks() const;

  void printChecks(llvm::raw_ostream &o) const;
};

} // namespace clam
