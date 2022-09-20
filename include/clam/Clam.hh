#pragma once

/**
 * Infer invariants using Crab.
 *
 * The analyzers of llvm Module or Function assume that the following
 * LLVM passes have been executed before:
 *
 * 1. llvm::createLowerInvokePass
 * 2. llvm::createLowerSwitchPass
 * 3. clam::createLowerCstExprPass
 * 4. llvm::createUnifyFunctionExitNodesPass
 *
 * Without passes 1-2, invoke and switch instructions will be ignored.
 * Without passes 3-4, the analyzer might produce an error.
 **/

#include "clam/ClamAnalysisParams.hh"
#include "clam/ClamQueryAPI.hh"
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
class IntraGlobalClamImpl;
class InterGlobalClamImpl;
class CrabBuilderManager;
} // namespace clam

namespace clam {

// A wrapper for an arbitrary abstract domain, cheap to copy
using clam_abstract_domain = crab::domains::abstract_domain_ref<var_t>;
  
// Generic class for a Clam global analysis (intra or inter)  
class ClamGlobalAnalysis: public ClamQueryAPI {
public:
  using abs_dom_map_t =
      llvm::DenseMap<const llvm::BasicBlock *, clam_abstract_domain>;
  using lin_csts_map_t =
      llvm::DenseMap<const llvm::BasicBlock *, lin_cst_sys_t>;  
  using checks_db_t = crab::checker::checks_db;

  virtual ~ClamGlobalAnalysis() = default;

  /**
   * Clear all the internal state
   **/
  virtual void clear() = 0;

  
  /* Return the manager used to build all CFGs */
  virtual CrabBuilderManager &getCfgBuilderMan() = 0;
  virtual const CrabBuilderManager &getCfgBuilderMan() const = 0;

  /**
   * Call crab analysis on the call graph under assumptions.
   **/
  virtual void analyze(AnalysisParams &params,
		       const abs_dom_map_t &assumptions) = 0;

  /**
   * Return invariants that hold at the entry of b
   **/
  virtual llvm::Optional<clam_abstract_domain> getPre(const llvm::BasicBlock *b,
						      bool keep_shadows) const = 0;

  /**
   * Return invariants that hold at the exit of b
   **/
  virtual llvm::Optional<clam_abstract_domain> getPost(const llvm::BasicBlock *b,
						       bool keep_shadows) const = 0;
  
  /**
   * Return a database with all checks.
   **/
  virtual const checks_db_t &getChecksDB() const = 0;

  /**
   * Return true if there might be a feasible edge between b1 and b2
   **/
  virtual bool hasFeasibleEdge(const llvm::BasicBlock *b1,
			       const llvm::BasicBlock *b2) const = 0;
};
  
/**
 * Global analysis of a module
 *
 * Basic usage:
 *    // Create a crab cfg builder manager
 *    CrabBuilderParams cparams;
 *    auto tli = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
 *    std::unique_ptr<HeapAbstraction> mem(new DummyHeapAbstraction());
 *    CrabBuilderManager man(cparams, tli, std::move(mem));
 *
 *    // Create a global analysis
 *    IntraGlobalClam ga(m, man); // or InterGlobalClam ga(m, man);
 *
 *    AnalysisParams aparams;
 *    ClamGlobalAnalysis::abs_dom_map_t assumptions;
 *    ga.analyze(aparams, assumptions);
 *    for (auto &f: m) {
 *       for (auto &b: f) {
 *         llvm::Optional<clam_abstract_domain> dom = ga.getPre(&b);
 *         if (dom.hasValue()) {
 *            crab::outs << dom.getValue() << "\n";
 *         }
 *      }
 *    }
 **/

class IntraGlobalClam: public ClamGlobalAnalysis {
public:
  using typename ClamGlobalAnalysis::abs_dom_map_t;
  using typename ClamGlobalAnalysis::lin_csts_map_t;
  using typename ClamGlobalAnalysis::checks_db_t;

private:
  std::unique_ptr<IntraGlobalClamImpl> m_impl;  

public:
  
  IntraGlobalClam(const llvm::Module &module, CrabBuilderManager &man);

  ~IntraGlobalClam();

  /* return the manager used to build all CFGs */
  CrabBuilderManager &getCfgBuilderMan() override;
  const CrabBuilderManager &getCfgBuilderMan() const override;  

  /**
   * Clear all the internal state
   **/
  void clear() override;

  /**
   * Call crab analysis on the whole module under assumptions.
   **/
  void analyze(AnalysisParams &params, const abs_dom_map_t &assumptions) override;

  /**
   * Return invariants that hold at the entry of b
   **/
  llvm::Optional<clam_abstract_domain> getPre(const llvm::BasicBlock *b,
                                              bool keep_shadows = false) const override;

  /**
   * Return invariants that hold at the exit of b
   **/
  llvm::Optional<clam_abstract_domain> getPost(const llvm::BasicBlock *b,
                                               bool keep_shadows = false) const override;

  /**
   * Return a database with all checks.
   **/
  const checks_db_t &getChecksDB() const override;

  /**
   * Return true if there might be a feasible edge between b1 and b2
   **/
  bool hasFeasibleEdge(const llvm::BasicBlock *b1,
                       const llvm::BasicBlock *b2) const override;

  /* ClamQueryAPI */
  llvm::AliasResult alias(const llvm::MemoryLocation &,
			  const llvm::MemoryLocation &,
			  llvm::AAQueryInfo &) override;
  
  llvm::ConstantRange range(const llvm::Instruction &I) override;
  
  llvm::ConstantRange range(const llvm::BasicBlock &B,
			    const llvm::Value &V) override;

  llvm::Optional<ClamQueryAPI::TagVector> tags(const llvm::Instruction &I) override;
  
  llvm::Optional<ClamQueryAPI::TagVector> tags(const llvm::BasicBlock &B,
					       const llvm::Value &V) override;
  
};

class InterGlobalClam: public ClamGlobalAnalysis {
public:
  using typename ClamGlobalAnalysis::abs_dom_map_t;
  using typename ClamGlobalAnalysis::lin_csts_map_t;
  using typename ClamGlobalAnalysis::checks_db_t;

private:
  std::unique_ptr<InterGlobalClamImpl> m_impl;

public:
  /**
   * Constructor that builds a crab call graph.
   **/
  InterGlobalClam(const llvm::Module &module, CrabBuilderManager &man);

  ~InterGlobalClam();

  /* return the manager used to build all CFGs */
  CrabBuilderManager &getCfgBuilderMan() override;
  const CrabBuilderManager &getCfgBuilderMan() const override;  

  /**
   * Clear all the internal state
   **/
  void clear() override;

  /**
   * Call crab analysis on the call graph under assumptions.
   **/
  void analyze(AnalysisParams &params, const abs_dom_map_t &assumptions) override;

  /**
   * Call crab analysis on the call graph under assumptions.
   **/
  void analyze(AnalysisParams &params, const lin_csts_map_t &assumptions);

  /**
   * Return invariants that hold at the entry of b
   **/
  llvm::Optional<clam_abstract_domain> getPre(const llvm::BasicBlock *b,
                                              bool keep_shadows = false) const override;

  /**
   * Return invariants that hold at the exit of b
   **/
  llvm::Optional<clam_abstract_domain> getPost(const llvm::BasicBlock *b,
                                               bool keep_shadows = false) const override;

  /**
   * Return a database with all checks.
   **/
  const checks_db_t &getChecksDB() const override;

  /**
   * Return true if there might be a feasible edge between b1 and b2
   **/
  bool hasFeasibleEdge(const llvm::BasicBlock *b1,
                       const llvm::BasicBlock *b2) const override;

  /* ClamQueryAPI */
  llvm::AliasResult alias(const llvm::MemoryLocation &,
			  const llvm::MemoryLocation &,
			  llvm::AAQueryInfo &) override;
  
  llvm::ConstantRange range(const llvm::Instruction &I) override;
  
  llvm::ConstantRange range(const llvm::BasicBlock &B,
			    const llvm::Value &V) override;

  llvm::Optional<ClamQueryAPI::TagVector> tags(const llvm::Instruction &I) override;
  
  llvm::Optional<ClamQueryAPI::TagVector> tags(const llvm::BasicBlock &B,
					       const llvm::Value &V) override;
};

/**
 * LLVM Module pass that computes invariants using Crab.
 **/
class ClamPass : public llvm::ModulePass {

  using abs_dom_map_t = typename ClamGlobalAnalysis::abs_dom_map_t;
  using checks_db_t = typename ClamGlobalAnalysis::checks_db_t;

  std::unique_ptr<CrabBuilderManager> m_cfg_builder_man;
  AnalysisParams m_params;
  std::unique_ptr<ClamGlobalAnalysis> m_ga;

public:
  static char ID;

  ClamPass();

  /* begin ModulePass API */
  virtual void releaseMemory() override;

  virtual bool runOnModule(llvm::Module &M) override;

  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

  virtual llvm::StringRef getPassName() const override {
    return "Clam: Crab for Llvm Abstraction Manager";
  }
  /* end ModulePass API */

  ClamGlobalAnalysis& getClamGlobalAnalysis();
  const ClamGlobalAnalysis& getClamGlobalAnalysis() const;
  
  /* return the manager used to build all CFGs */
  CrabBuilderManager &getCfgBuilderMan();
  const CrabBuilderManager &getCfgBuilderMan() const;  

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
   * Return true if there might be a feasible edge between b1 and b2
   **/
  bool hasFeasibleEdge(const llvm::BasicBlock *b1,
                       const llvm::BasicBlock *b2) const;

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


/**
 * Low-level API: intra-procedural analysis of a function.
 * 
 * This class differs from IntraGlobalClam in two ways: (1) it takes a
 * function rather than the whole module, and (2) it offers the method
 * pathAnalyze that abstractly executes "paths" as sequences of basic
 * blocks.
 *
 * Basic usage:
 *    // Create a crab cfg builder manager
 *    CrabBuilderParams cparams;
 *    auto tli = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
 *    std::unique_ptr<HeapAbstraction> mem(new DummyHeapAbstraction());
 *    CrabBuilderManager man(cparams, tli, std::move(mem));
 *
 *    // Create an intra-procedural analysis
 *    IntraClam ic(fun, man);
 *
 *    AnalysisParams aparams;
 *    ic.analyze(aparams);
 *    for (auto &b: fun) {
 *      llvm::Optional<clam_abstract_domain> dom = ic.getPre(&b);
 *      if (dom.hasValue()) {
 *         crab::outs << dom.getValue() << "\n";
 *      }
 *    }
 **/
class IntraClam {
public:
  using abs_dom_map_t = typename ClamGlobalAnalysis::abs_dom_map_t;
  using lin_csts_map_t = typename ClamGlobalAnalysis::lin_csts_map_t;
  using checks_db_t = typename ClamGlobalAnalysis::checks_db_t;

private:
  std::unique_ptr<IntraClamImpl> m_impl;

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
  const CrabBuilderManager &getCfgBuilderMan() const;

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
   * 
   * Return false if the path implies false and an explanation (core)
   * is found. An explanation is a subset of statements that implies
   * false.
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

  /**
   * Return true if there might be a feasible edge between b1 and b2
   **/
  bool hasFeasibleEdge(const llvm::BasicBlock *b1,
                       const llvm::BasicBlock *b2) const;
};

  
} // namespace clam
