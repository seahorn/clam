#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

#include "./crab/path_analyzer.hpp"
#include "./crab/printer.hpp"
#include "clam/CfgBuilder.hh"
#include "clam/Clam.hh"
#include "clam/ClamAnalysisParams.hh"
#include "clam/CrabDomainParser.hh"
#include "clam/DummyHeapAbstraction.hh"
#include "clam/RegisterAnalysis.hh"
#include "clam/SeaDsaHeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "clam/Support/NameValues.hh"
#include "clam/crab/crab_domains.hh"

#include "seadsa/AllocWrapInfo.hh"
#include "seadsa/CompleteCallGraph.hh"
#include "seadsa/DsaLibFuncInfo.hh"
#include "seadsa/InitializePasses.hh"
#include "seadsa/support/Debug.h"

#include "crab/analysis/bwd_analyzer.hpp"
#include "crab/analysis/dataflow/assumptions.hpp"
#include "crab/analysis/fwd_analyzer.hpp"
#include "crab/analysis/inter/top_down_inter_analyzer.hpp"
#include "crab/cfg/cfg_to_dot.hpp"
#include "crab/cg/cg.hpp"
#include "crab/cg/cg_bgl.hpp"
#include "crab/checkers/assertion.hpp"
#include "crab/checkers/checker.hpp"
#include "crab/config.h"
#include "crab/support/debug.hpp"
#include "crab/support/stats.hpp"

#include <functional>
#include <memory>
#include <unordered_map>

using namespace llvm;
using namespace clam;

#include "ClamOptions.def"

namespace clam {

/** =========== Begin typedefs ==========**/
using checks_db_t = typename IntraClam::checks_db_t;
using abs_dom_map_t = typename IntraClam::abs_dom_map_t;
using lin_csts_map_t = typename IntraClam::lin_csts_map_t;
// -- live symbols
using liveness_t = crab::analyzer::live_and_dead_analysis<cfg_ref_t>;
using liveness_map_t = std::unordered_map<cfg_ref_t, const liveness_t *>;
// -- forward/backward analyzer for intra-procedural analysis
using intra_analyzer_t =
    crab::analyzer::intra_forward_backward_analyzer<cfg_ref_t,
                                                    clam_abstract_domain>;
// -- intra property checker
using intra_checker_t = crab::checker::intra_checker<intra_analyzer_t>;
using assertion_property_checker_t =
    crab::checker::assert_property_checker<intra_analyzer_t>;
// -- inter-procedural analysis
using inter_params_t =
    crab::analyzer::top_down_inter_analyzer_parameters<cg_ref_t>;
using inter_analyzer_t =
    crab::analyzer::top_down_inter_analyzer<cg_ref_t, clam_abstract_domain>;
// -- path analyzer
using path_analyzer_t =
    crab::analyzer::path_analyzer<cfg_ref_t, clam_abstract_domain>;
// -- for pretty-printing results
using block_annotation_t = crab_pretty_printer::block_annotation;
using inv_annotation_t = crab_pretty_printer::invariant_annotation;
using unproven_assume_annotation_t =
    crab_pretty_printer::unproven_assumption_annotation;
#if 0
  using assumption_analysis_t = crab::analyzer::assumption_naive_analysis<cfg_ref_t>;
#else
using assumption_analysis_t =
    crab::analyzer::assumption_dataflow_analysis<cfg_ref_t>;
#endif
/** =========== End typedefs =============**/

class AnalysisResults {
  using edges_set =
      std::set<std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>>;

public:
  // invariants that hold at the entry of a block
  abs_dom_map_t &premap;
  // invariants that hold at the exit of a block
  abs_dom_map_t &postmap;
  // infeasible edges (for debugging or pretty-printing)
  edges_set infeasible_edges;
  // database with all the checks
  checks_db_t &checksdb;

  AnalysisResults(abs_dom_map_t &pre, abs_dom_map_t &post,
                  edges_set &false_edges, checks_db_t &db)
      : premap(pre), postmap(post), infeasible_edges(false_edges),
        checksdb(db) {}
};

static bool isTrackable(const Function &fun) {
  return !fun.isDeclaration() && !fun.empty() && !fun.isVarArg();
}

/** return invariant for block in table but filtering out shadow_varnames **/
static llvm::Optional<clam_abstract_domain>
lookup(const abs_dom_map_t &table, const llvm::BasicBlock &block,
       // remove shadow variables
       const std::vector<varname_t> &shadow_varnames) {
  auto it = table.find(&block);
  if (it == table.end()) {
    return llvm::None;
  }

  if (shadow_varnames.empty()) {
    return it->second;
  } else {
    std::vector<var_t> shadow_vars;
    shadow_vars.reserve(shadow_varnames.size());
    for (unsigned i = 0; i < shadow_vars.size(); ++i) {
      // we need to create a typed variable
      shadow_vars.push_back(var_t(shadow_varnames[i], crab::UNK_TYPE, 0));
    }
    clam_abstract_domain copy_invariants(it->second);
    copy_invariants.forget(shadow_vars);
    return copy_invariants;
  }
}

/** update table with pre or post invariants **/
static bool update(abs_dom_map_t &table, const llvm::BasicBlock &block,
                   clam_abstract_domain absval) {
  bool already = false;
  auto it = table.find(&block);
  if (it == table.end()) {
    table.insert(std::make_pair(&block, absval));
  } else {
    it->second = absval;
    already = true;
  }
  return already;
}

/**
 * Internal implementation of the intra-procedural analysis
 **/
class IntraClamImpl {
public:
  IntraClamImpl(const Function &fun, CrabBuilderManager &man)
      : m_cfg_builder(nullptr), m_fun(fun), m_vfac(man.getVarFactory()) {

    if (isTrackable(m_fun)) {
      if (!man.hasCfg(m_fun)) {
        CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                               << "Started Crab CFG construction for "
                               << fun.getName() << "\n");
        m_cfg_builder = man.mkCfgBuilder(m_fun);
        CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                               << "Finished Crab CFG construction for "
                               << fun.getName() << "\n");
      } else {
        m_cfg_builder = man.getCfgBuilder(m_fun);
      }
    } else {
      // CRAB_VERBOSE_IF(1, llvm::outs() << "Cannot build CFG for "
      //                                << fun.getName() << "\n");
    }
  }

  void analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
               // assumptions can be provided in abs_dom format or
               // as linear constraints.
               const abs_dom_map_t &abs_dom_assumptions,
               const lin_csts_map_t &lin_csts_assumptions,
               AnalysisResults &results) {

    if (!m_cfg_builder) {
      CRAB_VERBOSE_IF(1, llvm::outs() << "Skipped analysis for "
                                      << m_fun.getName() << "\n");
      return;
    }

    const liveness_t *live = nullptr;
    if (params.run_liveness || params.dom.isRelational()) {
      // -- run liveness
      m_cfg_builder->computeLiveSymbols();
      if (params.dom.isRelational()) {
        live = m_cfg_builder->getLiveSymbols();
        assert(live);
        unsigned total_live, avg_live_per_blk, max_live_per_blk;
        live->get_stats(total_live, max_live_per_blk, avg_live_per_blk);
        CRAB_VERBOSE_IF(
            1, crab::outs()
                   << "Max live per block: " << max_live_per_blk << "\n"
                   << "Threshold: " << params.relational_threshold << "\n");
        if (max_live_per_blk > params.relational_threshold) {
          // default domain
          params.dom = CrabDomain::INTERVALS;
        }
      }
    }

    if (CrabBuildOnlyCFG) {
      return;
    }

    if (DomainRegistry::count(params.dom)) {
      crabAnalyze(params, entry, DomainRegistry::at(params.dom),
                  abs_dom_assumptions, lin_csts_assumptions,
                  (params.run_liveness) ? live : nullptr, results);
    } else {
      CLAM_ERROR("Intra-procedural analysis for " << params.dom.name()
                                                  << " not found.");
    }
  }

  bool pathAnalyze(const AnalysisParams &params,
                   const std::vector<const llvm::BasicBlock *> &blocks,
                   bool layered_solving, std::vector<statement_t *> &core,
                   bool populate_inv_map, abs_dom_map_t &post) const {

    assert(m_cfg_builder);
    // build the full path (included internal basic blocks added
    // during the translation to Crab)
    std::vector<basic_block_label_t> path;
    path.reserve(blocks.size());
    for (unsigned i = 0; i < blocks.size(); ++i) {
      path.push_back(m_cfg_builder->getCrabBasicBlock(blocks[i]));
      if (i < blocks.size() - 1) {
        if (const basic_block_label_t *edge_bb =
                m_cfg_builder->getCrabBasicBlock(blocks[i], blocks[i + 1])) {
          path.push_back(*edge_bb);
        }
      }
    }

    bool res = true;
    if (DomainRegistry::count(params.dom)) {
      crabPathAnalyze(path, DomainRegistry::at(params.dom), core,
                      layered_solving, populate_inv_map, post, res);
    } else {
      CLAM_ERROR("Path analysis for  " << params.dom.name() << " not found.");
    }
    return res;
  }

private:
  CfgBuilderPtr m_cfg_builder;
  const Function &m_fun;
  variable_factory_t &m_vfac;

  void crabAnalyze(const AnalysisParams &params, const BasicBlock *entry,
                   clam_abstract_domain entry_abs,
                   const abs_dom_map_t &abs_dom_assumptions,
                   const lin_csts_map_t &lin_csts_assumptions,
                   const liveness_t *live, AnalysisResults &results) {

    CRAB_VERBOSE_IF(1, auto fdecl = m_cfg_builder->getCfg().get_func_decl();
                    crab::get_msg_stream()
                    << "Running intra-procedural analysis with "
                    << "\"" << entry_abs.domain_name() << "\""
                    << " for " << fdecl.get_func_name() << "  ... \n";);

    // -- run intra-procedural analysis
    intra_analyzer_t analyzer(m_cfg_builder->getCfg(), entry_abs);
    typename intra_analyzer_t::assumption_map_t crab_assumptions;

    // Reconstruct a crab assumption map from an abs_dom_map_t
    for (auto &kv : abs_dom_assumptions) {
      crab_assumptions.insert(
          {m_cfg_builder->getCrabBasicBlock(kv.first), kv.second});
    }

    // Reconstruct a crab assumption map from a lin_csts_map_t
    for (auto &kv : lin_csts_assumptions) {
      clam_abstract_domain absval = entry_abs.make_top();
      absval += kv.second;
      crab_assumptions.insert(
          {m_cfg_builder->getCrabBasicBlock(kv.first), absval});
    }

    analyzer.run(m_cfg_builder->getCrabBasicBlock(entry), entry_abs,
                 !params.run_backward, crab_assumptions, live,
                 params.widening_delay, params.narrowing_iters,
                 params.widening_jumpset);
    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Finished intra-procedural analysis.\n");

    // --- checking assertions
    if (params.check == CheckerKind::ASSERTION) {
      CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                             << "Checking assertions ... \n");
      intra_checker_t checker(analyzer,
                              {std::make_shared<assertion_property_checker_t>(
                                  params.check_verbose)});
      checker.run();
      CRAB_VERBOSE_IF(1, llvm::outs() << "Function " << m_fun.getName() << "\n";
                      checker.show(crab::outs()));
      results.checksdb += checker.get_all_checks();
      CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                             << "Finished assert checking.\n");
    }

    // -- store invariants
    if (params.store_invariants || params.print_invars) {
      CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Storing invariants.\n");
      for (basic_block_label_t bl :
           llvm::make_range(m_cfg_builder->getCfg().label_begin(),
                            m_cfg_builder->getCfg().label_end())) {
        if (bl.is_edge()) {
          // Note that we use get_post instead of get_pre:
          //   the crab block (bl) has an assume statement corresponding
          //   to the branch condition in the predecessor of the
          //   LLVM edge. We want the invariant *after* the
          //   evaluation of the assume.
          if (analyzer.get_post(bl).is_bottom()) {
            results.infeasible_edges.insert(
                {bl.get_edge().first, bl.get_edge().second});
          }
        } else if (const BasicBlock *B = bl.get_basic_block()) {
          // --- invariants that hold at the entry of the blocks
          update(results.premap, *B, analyzer.get_pre(bl));
          // --- invariants that hold at the exit of the blocks
          update(results.postmap, *B, analyzer.get_post(bl));
        } else {
          // this should be unreachable
          assert(
              false &&
              "A Crab block should correspond to either an LLVM edge or block");
        }
      }
      CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "All invariants stored.\n");
    }

    // -- print all cfg annotations (if any)
    if (params.print_invars || params.print_unjustified_assumptions) {
      std::vector<std::unique_ptr<block_annotation_t>> pool_annotations;
      if (m_cfg_builder->getCfg().has_func_decl()) {
        auto fdecl = m_cfg_builder->getCfg().get_func_decl();
        crab::outs() << "\n" << fdecl << "\n";
      } else {
        llvm::outs() << "\n"
                     << "function " << m_fun.getName() << "\n";
      }
      if (params.print_invars) {
        std::vector<varname_t> shadow_varnames;
        if (!params.keep_shadow_vars) {
          shadow_varnames = std::vector<varname_t>(
              m_vfac.get_shadow_vars().begin(), m_vfac.get_shadow_vars().end());
        }
        pool_annotations.emplace_back(std::make_unique<inv_annotation_t>(
            results.premap, results.postmap, shadow_varnames, &lookup));
      }

      // XXX: it must be alive when print_annotations is called.
      assumption_analysis_t unproven_assumption_analyzer(
          m_cfg_builder->getCfg());
      if (params.print_unjustified_assumptions) {
        // -- run first the analysis
        unproven_assumption_analyzer.exec();
        pool_annotations.emplace_back(
            std::make_unique<unproven_assume_annotation_t>(
                m_cfg_builder->getCfg(), &unproven_assumption_analyzer));
      }
      crab_pretty_printer::print_annotations(
          m_cfg_builder->getCfg(), results.checksdb, pool_annotations);
    }

    return;
  }

  // res is false iff the analysis of the path implies bottom
  void crabPathAnalyze(const std::vector<basic_block_label_t> &path,
                       clam_abstract_domain init,
                       std::vector<statement_t *> &core, bool layered_solving,
                       bool populate_inv_map, abs_dom_map_t &post,
                       bool &res) const {
    path_analyzer_t path_analyzer(m_cfg_builder->getCfg(), init);
    res = path_analyzer.solve(path, layered_solving);
    if (populate_inv_map) {
      for (auto n : path) {
        if (const llvm::BasicBlock *bb = n.get_basic_block()) {
          clam_abstract_domain abs_val = path_analyzer.get_fwd_constraints(n);
          post.insert({bb, abs_val});
          if (abs_val.is_bottom()) {
            // the rest of blocks must be also bottom so we don't
            // bother storing them.
            break;
          }
        }
      }
    }

    if (!res) {
      path_analyzer.get_unsat_core(core);
    }
  }
}; // end class

/**
 *   Begin IntraClam methods
 **/
IntraClam::IntraClam(const Function &fun, CrabBuilderManager &man)
    : m_impl(nullptr), m_fun(fun), m_builder_man(man) {
  m_impl = std::make_unique<IntraClamImpl>(m_fun, m_builder_man);
}

IntraClam::~IntraClam() = default;

void IntraClam::clear() {
  m_pre_map.clear();
  m_post_map.clear();
  m_checks_db.clear();
}

CrabBuilderManager &IntraClam::getCfgBuilderMan() { return m_builder_man; }

void IntraClam::analyze(AnalysisParams &params,
                        const abs_dom_map_t &assumptions) {
  AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                             m_checks_db};
  lin_csts_map_t lin_csts_assumptions;
  m_impl->analyze(params, &(m_fun.getEntryBlock()), assumptions,
                  lin_csts_assumptions, results);
}

void IntraClam::analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
                        const abs_dom_map_t &assumptions) {
  AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                             m_checks_db};
  lin_csts_map_t lin_csts_assumptions;
  m_impl->analyze(params, entry, assumptions, lin_csts_assumptions, results);
}

void IntraClam::analyze(AnalysisParams &params,
                        const lin_csts_map_t &assumptions) {
  AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                             m_checks_db};
  abs_dom_map_t abs_dom_assumptions;
  m_impl->analyze(params, &(m_fun.getEntryBlock()), abs_dom_assumptions,
                  assumptions, results);
}

void IntraClam::analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
                        const lin_csts_map_t &assumptions) {
  AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                             m_checks_db};
  abs_dom_map_t abs_dom_assumptions;
  m_impl->analyze(params, entry, abs_dom_assumptions, assumptions, results);
}

bool IntraClam::pathAnalyze(const AnalysisParams &params,
                            const std::vector<const llvm::BasicBlock *> &path,
                            bool layered_solving,
                            std::vector<statement_t *> &core) const {
  abs_dom_map_t post_conditions;
  return m_impl->pathAnalyze(params, path, layered_solving, core, false,
                             post_conditions);
}

bool IntraClam::pathAnalyze(const AnalysisParams &params,
                            const std::vector<const llvm::BasicBlock *> &path,
                            bool layered_solving,
                            std::vector<statement_t *> &core,
                            abs_dom_map_t &post_conditions) const {
  return m_impl->pathAnalyze(params, path, layered_solving, core, true,
                             post_conditions);
}

llvm::Optional<clam_abstract_domain>
IntraClam::getPre(const llvm::BasicBlock *block, bool keep_shadows) const {
  std::vector<varname_t> shadows;
  auto &vfac = m_builder_man.getVarFactory();
  if (!keep_shadows)
    shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
                                     vfac.get_shadow_vars().end());
  return lookup(m_pre_map, *block, shadows);
}

llvm::Optional<clam_abstract_domain>
IntraClam::getPost(const llvm::BasicBlock *block, bool keep_shadows) const {
  std::vector<varname_t> shadows;
  auto &vfac = m_builder_man.getVarFactory();
  if (!keep_shadows)
    shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
                                     vfac.get_shadow_vars().end());
  return lookup(m_post_map, *block, shadows);
}

bool IntraClam::hasFeasibleEdge(const llvm::BasicBlock *b1,
                                const llvm::BasicBlock *b2) const {
  return !(m_infeasible_edges.count({b1, b2}) > 0);
}

const checks_db_t &IntraClam::getChecksDB() const { return m_checks_db; }
/**
 *   End IntraClam methods
 **/

/**
 *  Internal implementation of the inter-procedural analysis
 **/
class InterClamImpl {
public:
  InterClamImpl(const Module &M, CrabBuilderManager &man)
      : m_cg(nullptr), m_crab_builder_man(man), m_M(M) {

    std::vector<cfg_ref_t> cfg_ref_vector;
    for (auto const &F : m_M) {
      if (isTrackable(F)) {
        // -- build cfg's
        if (!man.hasCfg(F)) {
          m_crab_builder_man.mkCfgBuilder(F);
        }
        cfg_t *cfg = &(m_crab_builder_man.getCfg(F));
        cfg_ref_vector.push_back(*cfg);
        // CRAB_VERBOSE_IF(1, llvm::outs()
        //<< "Built Crab CFG for " << F.getName() << "\n");
      } else {
        CRAB_VERBOSE_IF(1, llvm::outs() << "Cannot build CFG for "
                                        << F.getName() << "\n");
      }
    }
    // build call graph
    m_cg = std::make_unique<cg_t>(cfg_ref_vector.begin(), cfg_ref_vector.end());
  }

  void analyze(AnalysisParams &params,
               // assumptions can be provided in abs_dom format or
               // as linear constraints.
               const abs_dom_map_t &abs_dom_assumptions /*unused*/,
               const lin_csts_map_t &lin_csts_assumptions /*unused*/,
               AnalysisResults &results) {

    // If the number of live variables per block is too high we
    // switch to a cheap domain regardless what the user wants.
    CrabDomain::Type absdom = params.dom;

    /* Compute liveness information and choose statically the
       abstract domain */

    if (params.run_liveness || absdom.isRelational()) {
      unsigned max_live_per_blk = 0;
      for (auto cg_node : llvm::make_range(vertices(*m_cg))) {
        const liveness_t *live = nullptr;

        // Get the cfg builder to run liveness
        if (const Function *fun = m_M.getFunction(cg_node.name())) {
          auto cfg_builder = m_crab_builder_man.getCfgBuilder(*fun);
          assert(cfg_builder);
          // run liveness
          cfg_builder->computeLiveSymbols();
          live = cfg_builder->getLiveSymbols();
          // update max number of live variables for whole cg
          unsigned total_live, max_live_per_blk_, avg_live_per_blk;
          live->get_stats(total_live, max_live_per_blk_, avg_live_per_blk);
          max_live_per_blk = std::max(max_live_per_blk, max_live_per_blk_);
        }

        if (absdom.isRelational()) {
          // FIXME: the selection of the final domain is fixed for the
          //        whole program. That is, if there is one function that
          //        exceeds the threshold then the cheaper domain will be
          //        used for all functions. We should be able to change
          //        from one function to another.
          CRAB_VERBOSE_IF(
              1, crab::outs()
                     << "Max live per block: " << max_live_per_blk << "\n"
                     << "Threshold: " << params.relational_threshold << "\n");
          if (max_live_per_blk > params.relational_threshold) {
            // default domain
            absdom = CrabDomain::INTERVALS;
          }
        }

        if (params.run_liveness) {
          m_live_map.insert({cg_node.get_cfg(), live});
        }
      } // end for
    }

    // -- run the interprocedural analysis
    if (!CrabBuildOnlyCFG) {
      ////
      // TODO: pass assumptions to the inter-procedural analysis
      /////
      if (DomainRegistry::count(params.dom)) {
        analyze(params, DomainRegistry::at(params.dom), results);
      } else {
        CLAM_ERROR("Inter-procedural analysis for  " << params.dom.name()
                                                     << " not found");
      }
    }
  }

private:
  // crab call graph
  std::unique_ptr<cg_t> m_cg;
  // crab cfg builder manager
  CrabBuilderManager &m_crab_builder_man;
  // the LLVM module
  const Module &m_M;
  // live symbols
  liveness_map_t m_live_map;

  basic_block_label_t getCrabBasicBlock(const BasicBlock *bb) const {
    const Function *f = bb->getParent();
    auto builder = m_crab_builder_man.getCfgBuilder(*f);
    return builder->getCrabBasicBlock(bb);
  }

  /** Run inter-procedural analysis on the whole call graph **/
  void analyze(const AnalysisParams &params, clam_abstract_domain init,
               AnalysisResults &results) {

    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Running top-down inter-procedural analysis "
                           << "with domain:"
                           << "\"" << init.domain_name() << "\""
                           << "  ...\n";);

    inter_params_t inter_params;
    inter_params.run_checker = (params.check != CheckerKind::NOCHECKS);
    inter_params.checker_verbosity = params.check_verbose;
    inter_params.keep_cc_invariants = false;
    inter_params.keep_invariants =
        params.store_invariants || params.print_invars;
    inter_params.max_call_contexts = params.max_calling_contexts;
    inter_params.exact_summary_reuse = params.exact_summary_reuse;
    inter_params.analyze_recursive_functions =
        params.analyze_recursive_functions;
    inter_params.live_map = (params.run_liveness ? &m_live_map : nullptr);
    inter_params.widening_delay = params.widening_delay;
    inter_params.descending_iters = params.narrowing_iters;
    inter_params.thresholds_size = params.widening_jumpset;
    inter_analyzer_t analyzer(*m_cg, init, inter_params);
    analyzer.run(init);
    if (inter_params.run_checker) {
      results.checksdb += analyzer.get_all_checks();
    }

    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Finished inter-procedural analysis.\n");

    // -- store invariants
    if (params.store_invariants || params.print_invars) {
      CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Storing invariants.\n");
    }

    for (auto &n : llvm::make_range(vertices(*m_cg))) {
      cfg_ref_t cfg = n.get_cfg();
      if (const Function *F = m_M.getFunction(n.name())) {
        if (params.store_invariants || params.print_invars) {
          for (basic_block_label_t bl :
               llvm::make_range(cfg.label_begin(), cfg.label_end())) {
            if (bl.is_edge()) {
              // Note that we use get_post instead of getPre:
              //   the crab block (bl) has an assume statement corresponding
              //   to the branch condition in the predecessor of the
              //   LLVM edge. We want the invariant *after* the
              //   evaluation of the assume.
              if (analyzer.get_post(cfg, bl).is_bottom()) {
                results.infeasible_edges.insert(
                    {bl.get_edge().first, bl.get_edge().second});
              }
            } else if (const BasicBlock *B = bl.get_basic_block()) {
              // --- invariants that hold at the entry of the blocks
              auto pre = analyzer.get_pre(cfg, getCrabBasicBlock(B));
              update(results.premap, *B, pre);
              // --- invariants that hold at the exit of the blocks
              auto post = analyzer.get_post(cfg, getCrabBasicBlock(B));
              update(results.postmap, *B, post);
            } else {
              // this should be unreachable
              assert(false && "A Crab block should correspond to either an "
                              "LLVM edge or block");
            }
          }

          // --- print invariants and summaries
          if (params.print_invars && isTrackable(*F)) {
            if (cfg.has_func_decl()) {
              auto fdecl = cfg.get_func_decl();
              crab::outs() << "\n" << fdecl << "\n";
            } else {
              llvm::outs() << "\n"
                           << "function " << F->getName() << "\n";
            }

            std::vector<varname_t> shadow_varnames;
            if (!params.keep_shadow_vars) {
              shadow_varnames = std::vector<varname_t>(
                  m_crab_builder_man.getVarFactory().get_shadow_vars().begin(),
                  m_crab_builder_man.getVarFactory().get_shadow_vars().end());
            }
            std::vector<std::unique_ptr<block_annotation_t>> annotations;
            annotations.emplace_back(std::make_unique<inv_annotation_t>(
                results.premap, results.postmap, shadow_varnames, &lookup));
            crab_pretty_printer::print_annotations(cfg, results.checksdb,
                                                   annotations);
          }
        }
      }
    }

    if (params.store_invariants || params.print_invars) {
      CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "All invariants stored.\n");
    }

    return;
  }
};

/**
 *   Begin InterClam methods
 **/
InterClam::InterClam(const Module &module, CrabBuilderManager &man)
    : m_impl(nullptr), m_builder_man(man) {
  m_impl = std::make_unique<InterClamImpl>(module, m_builder_man);
}

InterClam::~InterClam() {}

void InterClam::clear() {
  m_pre_map.clear();
  m_post_map.clear();
  m_checks_db.clear();
}

CrabBuilderManager &InterClam::getCfgBuilderMan() { return m_builder_man; }

void InterClam::analyze(AnalysisParams &params,
                        const abs_dom_map_t &assumptions) {
  AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                             m_checks_db};
  lin_csts_map_t lin_csts_assumptions;
  m_impl->analyze(params, assumptions, lin_csts_assumptions, results);
}

void InterClam::analyze(AnalysisParams &params,
                        const lin_csts_map_t &assumptions) {
  AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                             m_checks_db};
  abs_dom_map_t abs_dom_assumptions;
  m_impl->analyze(params, abs_dom_assumptions, assumptions, results);
}

llvm::Optional<clam_abstract_domain>
InterClam::getPre(const llvm::BasicBlock *block, bool keep_shadows) const {
  std::vector<varname_t> shadows;
  if (!keep_shadows)
    shadows = std::vector<varname_t>(
        m_builder_man.getVarFactory().get_shadow_vars().begin(),
        m_builder_man.getVarFactory().get_shadow_vars().end());
  return lookup(m_pre_map, *block, shadows);
}

llvm::Optional<clam_abstract_domain>
InterClam::getPost(const llvm::BasicBlock *block, bool keep_shadows) const {
  std::vector<varname_t> shadows;
  if (!keep_shadows)
    shadows = std::vector<varname_t>(
        m_builder_man.getVarFactory().get_shadow_vars().begin(),
        m_builder_man.getVarFactory().get_shadow_vars().end());
  return lookup(m_post_map, *block, shadows);
}

const checks_db_t &InterClam::getChecksDB() const { return m_checks_db; }

bool InterClam::hasFeasibleEdge(const llvm::BasicBlock *b1,
                                const llvm::BasicBlock *b2) const {
  return !(m_infeasible_edges.count({b1, b2}) > 0);
}

/**
 * End InterClam methods
 **/

/**
 * Begin ClamPass methods
 **/
ClamPass::ClamPass() : llvm::ModulePass(ID), m_cfg_builder_man(nullptr) {
  // initialize sea-dsa dependencies
  llvm::initializeAllocWrapInfoPass(*llvm::PassRegistry::getPassRegistry());
  llvm::initializeCompleteCallGraphPass(*llvm::PassRegistry::getPassRegistry());
}

void ClamPass::releaseMemory() {
  m_pre_map.clear();
  m_post_map.clear();
  m_checks_db.clear();
}

bool ClamPass::runOnModule(Module &M) {
  /// Translate the module to Crab CFGs
  CrabBuilderParams builder_params(CrabTrackLev, CrabCFGSimplify, true,
                                   CrabEnableUniqueScalars, CrabIncludeHavoc,
                                   CrabEnableBignums, CrabAddNonNullity,
                                   CrabPrintCFG, CrabDotCFG);

  auto &tli = getAnalysis<TargetLibraryInfoWrapperPass>();

  /// Create the CFG builder manager
  std::unique_ptr<HeapAbstraction> mem(new DummyHeapAbstraction());
  switch (CrabHeapAnalysis) {
  case heap_analysis_t::CI_SEA_DSA:
  case heap_analysis_t::CS_SEA_DSA: {
    CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Started sea-dsa analysis\n";);
    // CallGraph &cg = getAnalysis<CallGraphWrapperPass>().getCallGraph();
    CallGraph &cg =
        getAnalysis<seadsa::CompleteCallGraph>().getCompleteCallGraph();
    seadsa::AllocWrapInfo &allocWrapInfo = getAnalysis<seadsa::AllocWrapInfo>();
    // FIXME: if we pass "this" then allocWrapInfo can be more
    // precise because it can use LoopInfo. However, I get some
    // crash that I need to debug.
    allocWrapInfo.initialize(M, nullptr /*this*/);
    seadsa::DsaLibFuncInfo &dsaLibFuncInfo =
        getAnalysis<seadsa::DsaLibFuncInfo>();

    mem.reset(new SeaDsaHeapAbstraction(
        M, cg, tli, allocWrapInfo, dsaLibFuncInfo,
        (CrabHeapAnalysis == heap_analysis_t::CS_SEA_DSA),
        CrabDsaDisambiguateUnknown, CrabDsaDisambiguatePtrCast,
        CrabDsaDisambiguateExternal));
    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Finished sea-dsa analysis\n";);
    break;
  }
  case heap_analysis_t::NONE:
  default:
    CLAM_WARNING("running clam without heap analysis");
  }
  m_cfg_builder_man.reset(
      new CrabBuilderManager(builder_params, tli, std::move(mem)));

  unsigned num_analyzed_funcs = 0;
  CRAB_VERBOSE_IF(
      1,
      for (auto &F
           : M) {
        if (isTrackable(F)) {
          num_analyzed_funcs++;
        }
      } crab::get_msg_stream()
          << "Started clam\n";
      crab::get_msg_stream()
      << "Total number of analyzed functions:" << num_analyzed_funcs << "\n";);

  /// Run the analysis

  m_params.dom = ClamDomain;
  m_params.run_backward = CrabBackward;
  m_params.run_inter = CrabInter;
  m_params.max_calling_contexts = CrabInterMaxSummaries;
  m_params.analyze_recursive_functions = CrabInterRecursiveFunctions;
  m_params.exact_summary_reuse = CrabInterExactSummaryReuse;
  m_params.run_liveness = CrabLive;
  m_params.relational_threshold = CrabRelationalThreshold;
  m_params.widening_delay = CrabWideningDelay;
  m_params.narrowing_iters = CrabNarrowingIters;
  m_params.widening_jumpset = CrabWideningJumpSet;
  m_params.stats = CrabStats.getValue();
  m_params.print_invars = CrabPrintAns;
  m_params.print_unjustified_assumptions = CrabPrintUnjustifiedAssumptions;
  m_params.print_summaries = CrabPrintSumm;
  m_params.store_invariants = CrabStoreInvariants;
  m_params.keep_shadow_vars = CrabKeepShadows;
  m_params.check = CrabCheck;
  m_params.check_verbose = CrabCheckVerbose;

  if (m_params.run_inter) {
    InterClamImpl inter_crab(M, *m_cfg_builder_man);
    AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                               m_checks_db};
    /* -- empty assumptions */
    abs_dom_map_t abs_dom_assumptions;
    lin_csts_map_t lin_csts_assumptions;
    inter_crab.analyze(m_params, abs_dom_assumptions, lin_csts_assumptions,
                       results);
  } else {
    unsigned fun_counter = 1;
    for (auto &F : M) {
      if (!m_params.run_inter && isTrackable(F)) {
        CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                               << "###Function " << fun_counter << "/"
                               << num_analyzed_funcs << "###\n";);
        ++fun_counter;
        runOnFunction(F);
      }
    }
  }

  if (builder_params.dot_cfg) {
    for (auto &F : M) {
      if (m_cfg_builder_man->hasCfg(F)) {
        cfg_t &cfg = m_cfg_builder_man->getCfg(F);
#if 1
        auto pre_fn = [this](const basic_block_label_t &node)
            -> boost::optional<clam_abstract_domain> {
          if (const BasicBlock *BB = node.get_basic_block()) {
            llvm::Optional<clam_abstract_domain> res = getPre(BB);
            if (res.hasValue()) {
              return res.getValue();
            }
          }
          return boost::optional<clam_abstract_domain>();
        };
        auto post_fn = [this](const basic_block_label_t &node)
            -> boost::optional<clam_abstract_domain> {
          if (const BasicBlock *BB = node.get_basic_block()) {
            llvm::Optional<clam_abstract_domain> res = getPost(BB);
            if (res.hasValue()) {
              return res.getValue();
            }
          }
          return boost::optional<clam_abstract_domain>();
        };

        cfg_to_dot<cfg_t, clam_abstract_domain>(cfg, pre_fn, post_fn,
                                                m_checks_db);
#else
        cfg_to_dot(cfg);
#endif
      }
    }
  }

  if (m_params.stats) {
    crab::CrabStats::PrintBrunch(crab::outs());
  }

  if (m_params.check != CheckerKind::NOCHECKS) {
    llvm::outs() << "\n************** ANALYSIS RESULTS ****************\n";
    printChecks(llvm::outs());
    llvm::outs() << "************** ANALYSIS RESULTS END*************\n";

    if (m_params.stats) {
      llvm::outs() << "\n************** BRUNCH STATS ********************\n";
      if (getTotalChecks() == 0) {
        llvm::outs() << "BRUNCH_STAT Result NOCHECKS\n";
      } else if (getTotalErrorChecks() > 0) {
        llvm::outs() << "BRUNCH_STAT Result FALSE\n";
      } else if (getTotalWarningChecks() == 0) {
        llvm::outs() << "BRUNCH_STAT Result TRUE\n";
      } else {
        llvm::outs() << "BRUNCH_STAT Result INCONCLUSIVE\n";
      }
      llvm::outs() << "************** BRUNCH STATS END *****************\n\n";
    }
  }

  return false;
}

bool ClamPass::runOnFunction(Function &F) {
  IntraClamImpl intra_crab(F, *m_cfg_builder_man);
  AnalysisResults results = {m_pre_map, m_post_map, m_infeasible_edges,
                             m_checks_db};
  /* -- empty assumptions */
  abs_dom_map_t abs_dom_assumptions;
  lin_csts_map_t lin_csts_assumptions;
  intra_crab.analyze(m_params, &F.getEntryBlock(), abs_dom_assumptions,
                     lin_csts_assumptions, results);
  return false;
}

void ClamPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesAll();
  AU.addRequired<TargetLibraryInfoWrapperPass>();

  bool runSeaDsa = (CrabHeapAnalysis == heap_analysis_t::CI_SEA_DSA ||
                    CrabHeapAnalysis == heap_analysis_t::CS_SEA_DSA);

  if (runSeaDsa) {
    // dependency for immutable AllocWrapInfo
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<seadsa::AllocWrapInfo>();
    AU.addRequired<seadsa::DsaLibFuncInfo>();
  }

  AU.addRequired<UnifyFunctionExitNodes>();
  AU.addRequired<clam::NameValues>();

  // More precise than LLVM callgraph
  AU.addRequired<seadsa::CompleteCallGraph>();
  // AU.addRequired<CallGraphWrapperPass>();
}

/**
 * For clam clients
 **/

bool ClamPass::hasCfg(llvm::Function &F) {
  return m_cfg_builder_man->hasCfg(F);
}

cfg_ref_t ClamPass::getCfg(llvm::Function &F) {
  assert(m_cfg_builder_man->hasCfg(F));
  return m_cfg_builder_man->getCfg(F);
}

CrabBuilderManager &ClamPass::getCfgBuilderMan() { return *m_cfg_builder_man; }

// return invariants that hold at the entry of block
llvm::Optional<clam_abstract_domain>
ClamPass::getPre(const llvm::BasicBlock *block, bool keep_shadows) const {
  std::vector<varname_t> shadows;
  auto &vfac = m_cfg_builder_man->getVarFactory();
  if (!keep_shadows)
    shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
                                     vfac.get_shadow_vars().end());
  return lookup(m_pre_map, *block, shadows);
}

// return invariants that hold at the exit of block
llvm::Optional<clam_abstract_domain>
ClamPass::getPost(const llvm::BasicBlock *block, bool keep_shadows) const {
  std::vector<varname_t> shadows;
  auto &vfac = m_cfg_builder_man->getVarFactory();
  if (!keep_shadows)
    shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
                                     vfac.get_shadow_vars().end());
  return lookup(m_post_map, *block, shadows);
}

bool ClamPass::hasFeasibleEdge(const llvm::BasicBlock *b1,
                               const llvm::BasicBlock *b2) const {
  return !(m_infeasible_edges.count({b1, b2}) > 0);
}

/**
 * For assertion checking
 **/

unsigned ClamPass::getTotalChecks() const {
  return getTotalSafeChecks() + getTotalErrorChecks() + getTotalWarningChecks();
}

unsigned ClamPass::getTotalSafeChecks() const {
  return m_checks_db.get_total_safe();
}

unsigned ClamPass::getTotalErrorChecks() const {
  return m_checks_db.get_total_error();
}

unsigned ClamPass::getTotalWarningChecks() const {
  return m_checks_db.get_total_warning();
}

void ClamPass::printChecks(raw_ostream &o) const {
  unsigned safe = getTotalSafeChecks();
  unsigned unsafe = getTotalErrorChecks();
  unsigned warning = getTotalWarningChecks();
  std::vector<unsigned> cnts = {safe, unsafe, warning};
  unsigned MaxValLen = 0;
  for (auto c : cnts)
    MaxValLen = std::max(MaxValLen, (unsigned)std::to_string(c).size());
  o << std::string((int)MaxValLen - std::to_string(safe).size(), ' ') << safe
    << std::string(2, ' ') << "Number of total safe checks\n"
    << std::string((int)MaxValLen - std::to_string(unsafe).size(), ' ')
    << unsafe << std::string(2, ' ') << "Number of total error checks\n"
    << std::string((int)MaxValLen - std::to_string(warning).size(), ' ')
    << warning << std::string(2, ' ') << "Number of total warning checks\n";
}

REGISTER_DOMAIN(CrabDomain::INTERVALS, interval_domain_t)
REGISTER_DOMAIN(CrabDomain::ZONES_SPLIT_DBM, split_dbm_domain_t)
REGISTER_DOMAIN(CrabDomain::DIS_INTERVALS, dis_interval_domain_t)
#if defined(HAVE_APRON) || defined(HAVE_ELINA)
REGISTER_DOMAIN(CrabDomain::OCT, oct_domain_t)
REGISTER_DOMAIN(CrabDomain::PK, pk_domain_t)
#endif
REGISTER_DOMAIN(CrabDomain::INTERVALS_CONGRUENCES, ric_domain_t)
REGISTER_DOMAIN(CrabDomain::TERMS_INTERVALS, term_int_domain_t)
REGISTER_DOMAIN(CrabDomain::TERMS_DIS_INTERVALS, term_dis_int_domain_t)
REGISTER_DOMAIN(CrabDomain::TERMS_ZONES, num_domain_t)
#ifdef HAVE_LDD
REGISTER_DOMAIN(CrabDomain::BOXES, boxes_domain_t)
#endif
REGISTER_DOMAIN(CrabDomain::WRAPPED_INTERVALS, wrapped_interval_domain_t)

char clam::ClamPass::ID = 0;
} // namespace clam

static RegisterPass<clam::ClamPass> X("clam", "Infer invariants using Crab",
                                      false, false);
