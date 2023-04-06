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
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

#include "clam/config.h"
#include "clam/CfgBuilder.hh"
#include "clam/Clam.hh"
#include "clam/ClamAnalysisParams.hh"
#include "clam/CrabDomainParser.hh"
#include "clam/DummyHeapAbstraction.hh"
#include "clam/RegisterAnalysis.hh"
#include "clam/SeaDsaHeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "clam/Support/NameValues.hh"
#include "ClamQueryCache.hh"
#include "crab/path_analysis/path_analyzer.hpp"
#include "crab/output/crabir/cfg_printer.hpp"
#include "crab/output/json/write_json.hh"

#include "seadsa/AllocWrapInfo.hh"
#include "seadsa/CompleteCallGraph.hh"
#include "seadsa/DsaLibFuncInfo.hh"
#include "seadsa/InitializePasses.hh"
#include "seadsa/Printer.hh"
#include "seadsa/support/Debug.h"

#include "crab/config.h"
#include "crab/analysis/bwd_analyzer.hpp"
#include "crab/analysis/dataflow/assumptions.hpp"
#include "crab/analysis/dataflow/assertion_crawler.hpp"
#include "crab/analysis/fwd_analyzer.hpp"
#include "crab/analysis/inter/inter_params.hpp"
#include "crab/analysis/inter/top_down_inter_analyzer.hpp"
#include "crab/cfg/cfg_to_dot.hpp"
#include "crab/cg/cg.hpp"
#include "crab/cg/cg_bgl.hpp"
#include "crab/checkers/assertion.hpp"
#include "crab/checkers/checker.hpp"
#include "crab/domains/abstract_domain_params.hpp"
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
using edges_set =
      std::set<std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>>;
// -- live symbols
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
    crab::analyzer::inter_analyzer_parameters<cg_t>;
using inter_analyzer_t =
    crab::analyzer::top_down_inter_analyzer<cg_t, clam_abstract_domain>;
// -- path analyzer
using path_analyzer_t =
    crab::analyzer::path_analyzer<cfg_ref_t, clam_abstract_domain>;
// -- for pretty-printing 
using block_annotation_t = crab_pretty_printer::block_annotation; // super class
using invariant_annotation_t = crab_pretty_printer::invariant_annotation;
using unproven_assume_annotation_t = crab_pretty_printer::unproven_assumption_annotation;
using voi_annotation_t = crab_pretty_printer::voi_annotation;  
using assumption_analysis_t = crab::analyzer::assumption_dataflow_analysis<cfg_ref_t>;
using voi_analysis_t = crab::analyzer::inter_assertion_crawler<cg_t>;
/** =========== End typedefs =============**/

class AnalysisResults {
  using edges_set =
      std::set<std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>>;

public:
  // invariants that hold at the entry of a block
  abs_dom_map_t &premap;
  // invariants that hold at the exit of a block
  abs_dom_map_t &postmap;
  // infeasible edges 
  edges_set &infeasible_edges;
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

static std::unique_ptr<llvm::ToolOutputFile>
openOutputFile(StringRef filename,
	       llvm::sys::fs::OpenFlags flags = llvm::sys::fs::OF_None) {
  std::error_code error_code;
  auto output = std::make_unique<llvm::ToolOutputFile>(filename, error_code, flags); 
  if (error_code) {
    if (llvm::errs().has_colors()) {
      llvm::errs().changeColor(llvm::raw_ostream::RED);
    } 
    llvm::errs() << "CLAM ERROR: could not open " << filename  << ": "
		 << error_code.message() << "\n";
    if (llvm::errs().has_colors()) {
      llvm::errs().resetColor();
    }
    return nullptr;
  } else {
    return output;
  }
}

static std::string appendFunctionNameToFileName(const std::string &filename, StringRef functionName) {
  StringRef ext = sys::path::extension(filename);
  StringRef filenameRef(filename);
  SmallString<128> path(filenameRef);
  sys::path::replace_extension(path, functionName + ext);
  return path.str().str();
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
      // HACK: we need to create a typed variable.  This is okay
      // because forget operation in Crab abstract domains just care
      // about variable names but this is dangerous and it might
      // create problems in the future.
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

void AnalysisParams::write(raw_ostream &o) const {
  o << "Analysis options:\n";
  o << "\tabstract domain: " <<  dom.desc() << "\n";
  o << "\trun inter-procedural analysis: " << run_inter << "\n";
  o << "\t\tmaximum number of calling contexts: " << max_calling_contexts << "\n";
  o << "\t\tprecise analysis of recursive calls: " << analyze_recursive_functions << "\n";
  o << "\t\texact summary reuse: " << exact_summary_reuse << "\n";
  o << "\t\tonly main as entry point: " << inter_entry_main << "\n";
  o << "\tFixpoint parameters" <<  "\n";
  o << "\t\twidening delay: " << widening_delay << "\n";
  o << "\t\tnarrowing iterations: " << narrowing_iters  << "\n";
  o << "\t\tsize of the widening jumpset: " << widening_jumpset << "\n";
  o << "\tSwitch to non-relation domain if max number of live variables >= " << relational_threshold << "\n";
  o << "\tPretty printing options" << "\n";
  o << "\t\tprint invariants: ";
  switch(print_invars) {
  case InvariantPrinterOptions::NONE: o << "none\n"; break;
  case InvariantPrinterOptions::BLOCKS: o << "at each basic block\n"; break;
  case InvariantPrinterOptions::LOOPS: o << "only at loop headers\n"; break;       
  }
  o << "\t\tprint unjustified assumptions: " << print_unjustified_assumptions  << "\n";
  o << "\t\tprint variables-of-influence wrt assertions: " << print_voi << "\n";
  o << "\tstore invariants for clam clients: " << store_invariants << "\n";
  o << "\trun checker: " << (check == CheckerKind::ASSERTION ? "1" : "0") << "\n";
  o << "\trun backward analysis: " << run_backward << "\n";
  o << "\trun liveness analysis: " << run_liveness << "\n";
}

/**
 * IntraClamImpl: internal implementation of the intra-procedural
 *                analysis.
 **/
class IntraClam;  
class IntraClamImpl {
  friend class IntraClam;
public:
  IntraClamImpl(const Function &fun, CrabBuilderManager &man)
    : m_cfg_builder_man(man), m_cfg_builder(nullptr),
      m_fun(fun), m_vfac(man.getVarFactory()) {

    if (isTrackable(m_fun)) {
      if (!man.hasCfg(m_fun)) {
        CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                               << "Started Crab CFG construction for "
			       << fun.getName().str() << "\n");
        m_cfg_builder = &(man.mkCfgBuilder(m_fun));
        CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                               << "Finished Crab CFG construction for "
			       << fun.getName().str() << "\n");
      } else {
        m_cfg_builder = man.getCfgBuilder(m_fun);
	assert(m_cfg_builder);
      }
      
      if (man.getCfgBuilderParams().dot_cfg) {
	auto &cfg = man.getCfg(m_fun);
	crab::cfg::cfg_to_dot(cfg);
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
      analyzeImpl(params, entry, DomainRegistry::at(params.dom),
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
      pathAnalyzeImpl(path, DomainRegistry::at(params.dom), core,
                      layered_solving, populate_inv_map, post, res);
    } else {
      CLAM_ERROR("Path analysis for  " << params.dom.name() << " not found.");
    }
    return res;
  }

  void clear() {
    m_pre_map.clear();
    m_post_map.clear();
    m_checks_db.clear();
    m_infeasible_edges.clear();
  }
  
private:
  CrabBuilderManager &m_cfg_builder_man;  
  CfgBuilder *m_cfg_builder;
  const Function &m_fun;
  variable_factory_t &m_vfac;
  // To store analysis results
  abs_dom_map_t m_pre_map;
  abs_dom_map_t m_post_map;
  edges_set m_infeasible_edges;
  checks_db_t m_checks_db;

  void storeAndOutputResults(cfg_ref_t cfg, const AnalysisParams &params,
			     const intra_analyzer_t &analyzer, AnalysisResults &results) {
    /**
     * There are two output formats: crabir and json
     *
     * By default, crabir format prints the CFGs and checks (if
     * any). The json format prints the checks (if any).
     * 
     * In addition, the user can choose to print invariants or not in
     * both formats.
     *
     * The crabir format can be printed to either standard output or to a file
     * The json format is always output to a file
     **/
    
    bool processInvariants = (params.store_invariants ||
			      params.print_invars != InvariantPrinterOptions::NONE);

    if (!processInvariants && params.output_json == "" && params.output_crabir == "" ) {
      return;
    }
    
    if (processInvariants) {
      CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Storing analysis results.\n");
      for (basic_block_label_t bl : llvm::make_range(cfg.label_begin(), cfg.label_end())) {
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
      CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Finished storing analysis results.\n");
    }

    bool dumpCrabIR = (params.output_crabir != "");
    bool dumpJson   = (params.output_json != "");
    bool dumpStdout = (!dumpCrabIR && !dumpJson && (params.print_invars != InvariantPrinterOptions::NONE));
    
    crab::crab_string_os crabir_os;

    if (dumpCrabIR || dumpStdout) {
      std::vector<std::unique_ptr<block_annotation_t>> annotations;
      if (params.print_invars != InvariantPrinterOptions::NONE) {
	std::vector<varname_t> shadow_varnames;
	assumption_analysis_t unproven_assumption_analyzer(cfg);
	
	if (!params.keep_shadow_vars) {
	  shadow_varnames = m_vfac.get_shadow_vars();
	}
	annotations.emplace_back(std::make_unique<invariant_annotation_t>(
		  results.premap, results.postmap, shadow_varnames, &lookup));
      
	if (params.print_unjustified_assumptions) {
	  // -- run first the analysis
	  unproven_assumption_analyzer.exec();
	  annotations.emplace_back(
            std::make_unique<unproven_assume_annotation_t>(
                cfg, unproven_assumption_analyzer));
	}
      }
    
      crab_pretty_printer::print_annotated_cfg(crabir_os, cfg, results.checksdb, annotations);
    }
    

    if (dumpCrabIR) {
      std::string adjustedOutputCrabIr = appendFunctionNameToFileName(params.output_crabir, m_fun.getName());
      std::unique_ptr<llvm::ToolOutputFile> output = openOutputFile(adjustedOutputCrabIr);
      output->os() << crabir_os.str();
      output->keep();
      llvm::errs() << "Created file " << adjustedOutputCrabIr << " with analysis results\n";	
    }
    
    if (dumpJson) {
      std::string adjustedOutputJson = appendFunctionNameToFileName(params.output_json, m_fun.getName());      
      std::unique_ptr<llvm::ToolOutputFile> output = openOutputFile(adjustedOutputJson);
      json::json_report json_report;
      json_report.write(cfg, params, results.premap, results.checksdb);
      output->os() << json_report.generate();
      output->keep();
      llvm::errs() << "Created file " << adjustedOutputJson << " with analysis results\n";
    }

    if (dumpStdout) {
      llvm::outs() << crabir_os.str();
    }
  }

  
  void analyzeImpl(const AnalysisParams &params, const BasicBlock *entry,
                   clam_abstract_domain entry_abs,
                   const abs_dom_map_t &abs_dom_assumptions,
                   const lin_csts_map_t &lin_csts_assumptions,
                   const liveness_t *live, AnalysisResults &results) {

    auto &cfg = m_cfg_builder->getCfg();
    CRAB_VERBOSE_IF(1, auto fdecl = cfg.get_func_decl();
                    crab::get_msg_stream()
                    << "Running intra-procedural analysis with "
                    << "\"" << entry_abs.domain_name() << "\""
                    << " for " << fdecl.get_func_name() << "  ... \n";);

    // -- run intra-procedural analysis
    intra_analyzer_t analyzer(cfg, entry_abs);
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
    
    // -- (optionally) store and output results
    storeAndOutputResults(cfg, params, analyzer, results);
    
    return;
  }

  // res is false iff the analysis of the path implies bottom
  void pathAnalyzeImpl(const std::vector<basic_block_label_t> &path,
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
    : m_impl(nullptr) {
  DomainRegistry::registerAllDomains();  
  m_impl = std::make_unique<IntraClamImpl>(fun, man);
}

IntraClam::~IntraClam() = default;

void IntraClam::clear() {
  m_impl->clear();
}

CrabBuilderManager &IntraClam::getCfgBuilderMan() {
  return m_impl->m_cfg_builder_man;
}

const CrabBuilderManager &IntraClam::getCfgBuilderMan() const {
  return m_impl->m_cfg_builder_man;
}
  
void IntraClam::analyze(AnalysisParams &params,
                        const abs_dom_map_t &assumptions) {
  AnalysisResults results =
    {m_impl->m_pre_map, m_impl->m_post_map,
     m_impl->m_infeasible_edges,
     m_impl->m_checks_db};
  lin_csts_map_t lin_csts_assumptions;
  m_impl->analyze(params, &(m_impl->m_fun.getEntryBlock()), assumptions,
                  lin_csts_assumptions, results);
}

void IntraClam::analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
                        const abs_dom_map_t &assumptions) {
  AnalysisResults results =
    {m_impl->m_pre_map, m_impl->m_post_map,
     m_impl->m_infeasible_edges,
     m_impl->m_checks_db};
  lin_csts_map_t lin_csts_assumptions;
  m_impl->analyze(params, entry, assumptions, lin_csts_assumptions, results);
}

void IntraClam::analyze(AnalysisParams &params,
                        const lin_csts_map_t &assumptions) {
  AnalysisResults results =
    {m_impl->m_pre_map, m_impl->m_post_map,
     m_impl->m_infeasible_edges,
     m_impl->m_checks_db};
  abs_dom_map_t abs_dom_assumptions;
  m_impl->analyze(params, &(m_impl->m_fun.getEntryBlock()), abs_dom_assumptions,
                  assumptions, results);
}

void IntraClam::analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
                        const lin_csts_map_t &assumptions) {
  AnalysisResults results =
    {m_impl->m_pre_map, m_impl->m_post_map,
     m_impl->m_infeasible_edges,
     m_impl->m_checks_db};
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
  auto &vfac = m_impl->m_cfg_builder_man.getVarFactory();
  if (!keep_shadows) {
    shadows = vfac.get_shadow_vars();
  }
  return lookup(m_impl->m_pre_map, *block, shadows);
}

llvm::Optional<clam_abstract_domain>
IntraClam::getPost(const llvm::BasicBlock *block, bool keep_shadows) const {
  std::vector<varname_t> shadows;
  auto &vfac = m_impl->m_cfg_builder_man.getVarFactory();
  if (!keep_shadows) {
    shadows = vfac.get_shadow_vars();
  }
  return lookup(m_impl->m_post_map, *block, shadows);
}

bool IntraClam::hasFeasibleEdge(const llvm::BasicBlock *b1,
                                const llvm::BasicBlock *b2) const {
  return !(m_impl->m_infeasible_edges.count({b1, b2}) > 0);
}
		     
const checks_db_t &IntraClam::getChecksDB() const {
  return m_impl->m_checks_db;
}
  
/**
 * IntraGlobalClamImpl: internal implementation of the global
 * intra-procedural analysis
 **/
class IntraGlobalClamImpl {
public:  
  IntraGlobalClamImpl(const llvm::Module &module, CrabBuilderManager &man)
    : m_module(module), m_builder_man(man), m_query_cache(m_builder_man) {}

  ~IntraGlobalClamImpl() = default;

  CrabBuilderManager &getCfgBuilderMan() {
    return m_builder_man;
  }

  const CrabBuilderManager &getCfgBuilderMan() const {
    return m_builder_man;
  }

  void clear() {
    m_pre_map.clear();
    m_post_map.clear();
    m_checks_db.clear();
    m_infeasible_edges.clear();  
  }
    

  void analyze(AnalysisParams &params, const abs_dom_map_t &abs_dom_assumptions) {
    if (params.run_inter) {
      CLAM_WARNING("Analysis is intra-procedural but user wants inter-procedural. "
		   << "Running intra-procedural analysis.");
    }
    
    unsigned num_analyzed_funcs = 0;
    CRAB_VERBOSE_IF(1,
		    for (auto &F: m_module) {
		      if (isTrackable(F)) num_analyzed_funcs++;
		    });
    unsigned fun_counter = 1;
    for (auto &F : m_module) {
      if (isTrackable(F)) {
	CRAB_VERBOSE_IF(1, crab::get_msg_stream()
			<< "###Function " << fun_counter << "/"
			<< num_analyzed_funcs << "###\n";);
	++fun_counter;
	IntraClamImpl intra_crab(F, m_builder_man);
	AnalysisResults results =
	  {m_pre_map, m_post_map,
	   m_infeasible_edges, m_checks_db};
	lin_csts_map_t lin_csts_assumptions/*unused*/;
	intra_crab.analyze(params, &F.getEntryBlock(), abs_dom_assumptions,
			   lin_csts_assumptions, results);
      }
    }
    if (params.stats) {
      crab::CrabStats::PrintBrunch(crab::outs());
    }
  }

  Optional<clam_abstract_domain> getPre(const BasicBlock *bb,
					bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows) {
      shadows = m_builder_man.getVarFactory().get_shadow_vars();
    }
    return lookup(m_pre_map, *bb, shadows);
  }
  
  Optional<clam_abstract_domain> getPost(const BasicBlock *bb,
					 bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows) {
      shadows = m_builder_man.getVarFactory().get_shadow_vars();
    }
    return lookup(m_post_map, *bb, shadows);
  }

  const checks_db_t &getChecksDB() const {
    return m_checks_db;
  }

  bool hasFeasibleEdge(const BasicBlock *b1,
                       const BasicBlock *b2) const {
    return !(m_infeasible_edges.count({b1, b2}) > 0);    
  }

  AliasResult alias(const MemoryLocation &l1, const MemoryLocation &l2,
		    AAQueryInfo &AAQI) {
    return m_query_cache.alias(l1, l2, AAQI);
  }
  
  ConstantRange range(const Instruction &I) {
    return m_query_cache.range(I, getPre(I.getParent(), false));
  }
  
  ConstantRange range(const BasicBlock &B, const Value &V) {
    return m_query_cache.range(B, V, getPre(&B, false));
  }

  Optional<ClamQueryAPI::TagVector> tags(const Instruction &I) {
    return m_query_cache.tags(I, getPre(I.getParent(), false));
  }
  
  Optional<ClamQueryAPI::TagVector> tags(const BasicBlock &B, const Value &V) {
    return m_query_cache.tags(B, V, getPre(&B, false));
  }
  
private:
  const llvm::Module& m_module;
  CrabBuilderManager &m_builder_man;
  // To store analysis results
  abs_dom_map_t m_pre_map;
  abs_dom_map_t m_post_map;
  edges_set m_infeasible_edges;
  checks_db_t m_checks_db;
  // To answer analysis queries
  ClamQueryCache m_query_cache;
};
  
/**
 *  InterGlobalClamImpl: internal implementation of the global
 *  inter-procedural analysis.
 **/
class InterGlobalClam;
class InterGlobalClamImpl {
  friend class InterGlobalClam;
public:
  InterGlobalClamImpl(const Module &M, CrabBuilderManager &man)
    : m_cg(nullptr), m_crab_builder_man(man), m_M(M),
      m_query_cache(m_crab_builder_man) {
    std::vector<cfg_ref_t> cfg_ref_vector;
    for (auto const &F : m_M) {
      if (isTrackable(F)) {
        if (!man.hasCfg(F)) {
          m_crab_builder_man.mkCfgBuilder(F);
        }
        cfg_t *cfg = &(m_crab_builder_man.getCfg(F));
        cfg_ref_vector.push_back(*cfg);

	if(man.getCfgBuilderParams().dot_cfg) {
	  crab::cfg::cfg_to_dot(*cfg);
	}
      }
    }
    // build call graph
    m_cg = std::make_unique<cg_t>(cfg_ref_vector.begin(), cfg_ref_vector.end());
  }

  void analyze(AnalysisParams &params,
	       const abs_dom_map_t &assumptions) {
    AnalysisResults results =
      {m_pre_map, m_post_map,
       m_infeasible_edges,
       m_checks_db};
    lin_csts_map_t lin_csts_assumptions;
    analyze(params, assumptions, lin_csts_assumptions, results);
    if (params.stats) {
      crab::CrabStats::PrintBrunch(crab::outs());
    }  
  }

  void analyze(AnalysisParams &params,
	       const lin_csts_map_t &assumptions) {
    AnalysisResults results =
      {m_pre_map, m_post_map,
       m_infeasible_edges,
       m_checks_db};
    abs_dom_map_t abs_dom_assumptions;
    analyze(params, abs_dom_assumptions, assumptions, results);
    if (params.stats) {
    crab::CrabStats::PrintBrunch(crab::outs());
    }  
  }

  Optional<clam_abstract_domain>
  getPre(const BasicBlock *block, bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows) {
      shadows = m_crab_builder_man.getVarFactory().get_shadow_vars();
    }
    return lookup(m_pre_map, *block, shadows);
  }

  Optional<clam_abstract_domain>
  getPost(const BasicBlock *block, bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows) {
      shadows = m_crab_builder_man.getVarFactory().get_shadow_vars();
    }
    return lookup(m_post_map, *block, shadows);
  }
  
  AliasResult alias(const MemoryLocation &l1, const MemoryLocation &l2,
		    AAQueryInfo &AAQI) {
    return m_query_cache.alias(l1, l2, AAQI);    
  }
  
  ConstantRange range(const Instruction &I) {
    return m_query_cache.range(I, getPre(I.getParent(), false));    
  }
  
  ConstantRange range(const BasicBlock &B, const Value &V) {
    return m_query_cache.range(B, V, getPre(&B, false));    
  }

  Optional<ClamQueryAPI::TagVector> tags(const Instruction &I) {
    return m_query_cache.tags(I, getPre(I.getParent(), false));
  }
  
  Optional<ClamQueryAPI::TagVector> tags(const BasicBlock &B, const Value &V) {
    return m_query_cache.tags(B, V, getPre(&B, false));
  }
  
  void clear() {
    m_pre_map.clear();
    m_post_map.clear();
    m_checks_db.clear();
    m_infeasible_edges.clear();  
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
  // To store analysis results
  abs_dom_map_t m_pre_map;
  abs_dom_map_t m_post_map;
  edges_set m_infeasible_edges;
  checks_db_t m_checks_db;
  // To answer analysis queries
  ClamQueryCache m_query_cache;

  
  basic_block_label_t getCrabBasicBlock(const BasicBlock *bb) const {
    const Function *f = bb->getParent();
    if (auto builder = m_crab_builder_man.getCfgBuilder(*f)) {
      return builder->getCrabBasicBlock(bb);
    } else {
      CLAM_ERROR("Cannot find crab cfg for " <<  f->getName());      
    }
  }

  /** Run inter-procedural analysis on the whole call graph **/  
  void analyze(AnalysisParams &params,
               // assumptions can be provided in abs_dom format or
               // as linear constraints.
               const abs_dom_map_t &abs_dom_assumptions /*unused*/,
               const lin_csts_map_t &lin_csts_assumptions /*unused*/,
               AnalysisResults &results) {

    CRAB_VERBOSE_IF(1,
		    params.write(llvm::errs());
		    crab::outs() << "\n";
		    crab::domains::crab_domain_params_man::get().write(crab::outs());
		    crab::outs() << "\n";);
    
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


  void storeAndOutputResults(cg_t &cg, const AnalysisParams &params,
			     const inter_analyzer_t &analyzer, AnalysisResults &results) {
     /**
     * There are two output formats: crabir and json
     *
     * By default, crabir format prints the CFGs and checks (if
     * any). The json format prints the checks (if any).
     * 
     * In addition, the user can choose to print invariants or not in
     * both formats.
     *
     * The crabir format can be printed to either standard output or to a file
     * The json format is always output to a file
     **/
    
    bool processInvariants = (params.store_invariants ||
			      params.print_invars != InvariantPrinterOptions::NONE);

    if (!processInvariants && params.output_json == "" && params.output_crabir == "" ) {
      return;
    }

    std::unique_ptr<voi_analysis_t> voi = nullptr;
    if (params.print_voi) {
      voi.reset(new voi_analysis_t(cg,
				   true /*only data*/,
				   true /*ignore region offsets*/));
      voi->run();
    }

    std::unique_ptr<llvm::ToolOutputFile> crabir_output = nullptr;
    crab::crab_string_os crabir_os;
    if (params.output_crabir != "") {
      crabir_output = openOutputFile(params.output_crabir);
    }

    std::unique_ptr<llvm::ToolOutputFile> json_output = nullptr;
    json::json_report json_report;
    if (params.output_json != "") {      
      json_output = openOutputFile(params.output_json);
    }

    std::vector<varname_t> shadow_varnames;	  
    if (params.print_invars != InvariantPrinterOptions::NONE && !params.keep_shadow_vars) {
      shadow_varnames = m_crab_builder_man.getVarFactory().get_shadow_vars();
    }

    bool dumpCrabIR = crabir_output != nullptr ;
    bool dumpJson   = json_output != nullptr;
    bool dumpStdout = (!dumpCrabIR && !dumpJson && (params.print_invars != InvariantPrinterOptions::NONE));
    
    
    for (auto &n : llvm::make_range(vertices(cg))) {
      cfg_ref_t cfg = n.get_cfg();
      if (const Function *F = m_M.getFunction(n.name())) {
	if (processInvariants) {
	  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
			  << "Storing analysis results for "
			  << F->getName().str() << ".\n");
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
	  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
			  << "Finished storing analysis results for "
			  << F->getName().str() << ".\n");
	}

	
	if (dumpCrabIR || dumpStdout) {
	    std::vector<std::unique_ptr<block_annotation_t>> annotations;	
	    if (params.print_invars != InvariantPrinterOptions::NONE) {
	      annotations.emplace_back(std::make_unique<invariant_annotation_t>(
		      results.premap, results.postmap, shadow_varnames, &lookup));
	      // -- variables of interest that might affect each assertion
	      if (params.print_voi) {
		annotations.emplace_back(std::make_unique<voi_annotation_t>(cfg, *voi));
	      }
	    }

	  crab_pretty_printer::print_annotated_cfg(crabir_os, cfg, results.checksdb, annotations);
	}

	if (dumpJson) {
	  // results is a whole-program datastructure so we make sure
	  // that the checks are counted only once.
	  if (n == cg.entry()) {
	    json_report.write(cfg, params, results.premap, results.checksdb);
	  } else {
	    checks_db_t empty;
	    json_report.write(cfg, params, results.premap, empty);
	  }
	}
      }
    } // end for
    
    if (dumpCrabIR) {
      assert(crabir_output != nullptr);
      crabir_output->os() << crabir_os.str();	    
      crabir_output->keep();
      llvm::errs() << "Created file " << params.output_crabir << " with analysis results\n";            
    } 

    if (dumpJson) {
      assert(json_output != nullptr);
      json_output->os() << json_report.generate();
      json_output->keep();
      llvm::errs() << "Created file " << params.output_json << " with analysis results\n";      
    }

    if (dumpStdout) {
      llvm::outs() << crabir_os.str();
    }

  }

  
    
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
      params.store_invariants || params.print_invars != InvariantPrinterOptions::NONE;        
    inter_params.max_call_contexts = params.max_calling_contexts;
    inter_params.exact_summary_reuse = params.exact_summary_reuse;
    inter_params.analyze_recursive_functions =
        params.analyze_recursive_functions;
    inter_params.only_main_as_entry = params.inter_entry_main;
    inter_params.live_map = (params.run_liveness ? &m_live_map : nullptr);
    inter_params.widening_delay = params.widening_delay;
    inter_params.descending_iters = params.narrowing_iters;
    inter_params.thresholds_size = params.widening_jumpset;
    
    inter_analyzer_t analyzer(*m_cg, init, inter_params);
    analyzer.run(init);
    if (inter_params.run_checker) {
      results.checksdb += analyzer.get_all_checks();
    }
    storeAndOutputResults(*m_cg, params, analyzer, results);   
  }
};

  
/*****************************************************************/
/*              Begin IntraGlobalClam methods                    */
/*****************************************************************/ 
IntraGlobalClam::IntraGlobalClam(const Module &module, CrabBuilderManager &man)
  : m_impl(nullptr) {
  DomainRegistry::registerAllDomains();
  m_impl = std::make_unique<IntraGlobalClamImpl>(module, man);
}

IntraGlobalClam::~IntraGlobalClam(){}
  
void IntraGlobalClam::clear() {
  m_impl->clear();
}

CrabBuilderManager &IntraGlobalClam::getCfgBuilderMan() {
  return m_impl->getCfgBuilderMan();
}

const CrabBuilderManager &IntraGlobalClam::getCfgBuilderMan() const {
  return m_impl->getCfgBuilderMan();
}
  
void IntraGlobalClam::analyze(AnalysisParams &params,
			      const abs_dom_map_t &abs_dom_assumptions) {
  CRAB_VERBOSE_IF(1,
		  params.write(llvm::errs());
		  crab::outs() << "\n";
		  crab::domains::crab_domain_params_man::get().write(crab::outs());
		  crab::outs() << "\n";);
  
  m_impl->analyze(params, abs_dom_assumptions);
}

Optional<clam_abstract_domain>
IntraGlobalClam::getPre(const BasicBlock *block, bool keep_shadows) const {
  return m_impl->getPre(block, keep_shadows);
}

Optional<clam_abstract_domain>
IntraGlobalClam::getPost(const BasicBlock *block, bool keep_shadows) const {
  return m_impl->getPost(block, keep_shadows);  
}

const checks_db_t &IntraGlobalClam::getChecksDB() const {
  return m_impl->getChecksDB();
}
  
bool IntraGlobalClam::hasFeasibleEdge(const BasicBlock *b1,
				      const BasicBlock *b2) const {
  return m_impl->hasFeasibleEdge(b1, b2);
}

AliasResult IntraGlobalClam::alias(const MemoryLocation &l1, const MemoryLocation &l2,
				   AAQueryInfo &AAQI) {
  return m_impl->alias(l1, l2, AAQI);
}
  
ConstantRange IntraGlobalClam::range(const Instruction &I) {
  return m_impl->range(I);
}
  
ConstantRange IntraGlobalClam::range(const BasicBlock &B, const Value &V) {
  return m_impl->range(B, V);
}

Optional<ClamQueryAPI::TagVector> IntraGlobalClam::tags(const Instruction &I) {
  return m_impl->tags(I);
}

Optional<ClamQueryAPI::TagVector> IntraGlobalClam::tags(const BasicBlock &B, const Value &V) {
  return m_impl->tags(B,V);
}

/*****************************************************************/
/*              Begin InterGlobalClam methods                    */
/*****************************************************************/ 
InterGlobalClam::InterGlobalClam(const Module &module, CrabBuilderManager &man)
  : m_impl(nullptr) {
  DomainRegistry::registerAllDomains();  
  m_impl = std::make_unique<InterGlobalClamImpl>(module, man);
}

InterGlobalClam::~InterGlobalClam(){}  
  
void InterGlobalClam::clear() {
  m_impl->clear();
}

CrabBuilderManager &InterGlobalClam::getCfgBuilderMan() {
  return m_impl->m_crab_builder_man;
}

const CrabBuilderManager &InterGlobalClam::getCfgBuilderMan() const {
  return m_impl->m_crab_builder_man;
}
  
void InterGlobalClam::analyze(AnalysisParams &params,
			      const abs_dom_map_t &assumptions) {
  m_impl->analyze(params, assumptions);
}

void InterGlobalClam::analyze(AnalysisParams &params,
			      const lin_csts_map_t &assumptions) {
  m_impl->analyze(params, assumptions);
}

Optional<clam_abstract_domain>
InterGlobalClam::getPre(const BasicBlock *bb, bool keep_shadows) const {
  return m_impl->getPre(bb, keep_shadows);
}

Optional<clam_abstract_domain>
InterGlobalClam::getPost(const BasicBlock *bb, bool keep_shadows) const {
  return m_impl->getPost(bb, keep_shadows);
}

const checks_db_t &InterGlobalClam::getChecksDB() const {
  return m_impl->m_checks_db;
}

bool InterGlobalClam::hasFeasibleEdge(const BasicBlock *b1,
				      const BasicBlock *b2) const {
  return !(m_impl->m_infeasible_edges.count({b1, b2}) > 0);
}

AliasResult InterGlobalClam::alias(const MemoryLocation &l1, const MemoryLocation &l2,
				   AAQueryInfo &AAQI) {
  return m_impl->alias(l1, l2, AAQI);
}
  
ConstantRange InterGlobalClam::range(const Instruction &I) {
  return m_impl->range(I);
}
  
ConstantRange InterGlobalClam::range(const BasicBlock &B, const Value &V) {
  return m_impl->range(B, V);
}

Optional<ClamQueryAPI::TagVector> InterGlobalClam::tags(const Instruction &I) {
  return m_impl->tags(I);
}

Optional<ClamQueryAPI::TagVector> InterGlobalClam::tags(const BasicBlock &B, const Value &V) {
  return m_impl->tags(B,V);
}

  
/*****************************************************************/
/*                       ClamPass methods                        */
/*****************************************************************/

ClamPass::ClamPass():
  ModulePass(ID), m_cfg_builder_man(nullptr), m_ga(nullptr) {
  // initialize sea-dsa dependencies
  llvm::initializeAllocWrapInfoPass(*llvm::PassRegistry::getPassRegistry());
  llvm::initializeCompleteCallGraphPass(*llvm::PassRegistry::getPassRegistry());
}

void ClamPass::releaseMemory() {
  m_ga->clear();
}

bool ClamPass::runOnModule(Module &M) {
  /// Translate the module to Crab CFGs
  CrabBuilderParams builder_params;
  builder_params.precision_level = CrabTrackLev;
  builder_params.simplify = CrabCFGSimplify;
  builder_params.lower_singleton_aliases = CrabEnableUniqueScalars;
  builder_params.include_useless_havoc = CrabIncludeHavoc;
  builder_params.enable_bignums = CrabEnableBignums;
  if (CrabLowerUnsignedICmp) {
    builder_params.lowerUnsignedICmpIntoSigned();
  }
  builder_params.lower_arithmetic_with_overflow_intrinsics =
    CrabLowerWithOverflowIntrinsics;
  builder_params.allocate_global_values = CrabAllocateGlobals;
  builder_params.add_pointer_assumptions = CrabAddPtrAssumptions;
  builder_params.add_null_checks = CrabNullChecks;
  builder_params.add_uaf_checks = CrabUafChecks;
  builder_params.add_bounds_checks = CrabBoundsChecks;
  builder_params.add_is_deref = CrabIsDeref;  
  builder_params.check_only_typed_regions = CrabCheckOnlyTyped;
  builder_params.check_only_noncyclic_regions = CrabCheckOnlyNonCyclic;
  builder_params.print_cfg = CrabPrintCFG;
  builder_params.dot_cfg = CrabDotCFG;

  auto &tli = getAnalysis<TargetLibraryInfoWrapperPass>();

  /// Create the CFG builder manager
  std::unique_ptr<HeapAbstraction> mem(new DummyHeapAbstraction());
  switch (CrabHeapAnalysis) {
  case heap_analysis_t::CI_SEA_DSA:
  case heap_analysis_t::CS_SEA_DSA: {
    CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Started sea-dsa analysis\n";);
    seadsa::CompleteCallGraph &ccg = getAnalysis<seadsa::CompleteCallGraph>();
    CallGraph &cg = ccg.getCompleteCallGraph();
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

    if (CrabDsaDot) {
      seadsa::DsaPrinter
	printer(*(static_cast<SeaDsaHeapAbstraction*>(&*mem)->getSeaDsa()), &ccg);
      printer.runOnModule(M);
    }
    break;
  }
  case heap_analysis_t::NONE:
    CLAM_WARNING("running clam without heap analysis");
  }
  m_cfg_builder_man.reset(
      new CrabBuilderManager(builder_params, tli, std::move(mem)));
   
  /// Run the analysis

  m_params.dom = ClamDomain;
  m_params.run_backward = CrabBackward;
  m_params.run_inter = CrabInter;
  m_params.max_calling_contexts = CrabInterMaxSummaries;
  m_params.analyze_recursive_functions = CrabInterRecursiveFunctions;
  m_params.exact_summary_reuse = CrabInterExactSummaryReuse;
  m_params.inter_entry_main = CrabInterStartFromMain;
  m_params.run_liveness = CrabLive;
  m_params.relational_threshold = CrabRelationalThreshold;
  m_params.widening_delay = CrabWideningDelay;
  m_params.narrowing_iters = CrabNarrowingIters;
  m_params.widening_jumpset = CrabWideningJumpSet;
  m_params.stats = crab::CrabStatsFlag /*CrabStats*/;
  m_params.print_invars = CrabPrintInvariants;
  m_params.print_unjustified_assumptions = CrabPrintUnjustifiedAssumptions;
  m_params.print_voi = CrabPrintVoi;
  m_params.output_crabir = CrabIRToFile;
  m_params.output_json = CrabResultsToJSON;    
  m_params.store_invariants = CrabStoreInvariants;
  m_params.keep_shadow_vars = CrabKeepShadows;
  m_params.check = (CrabCheck ?
		    clam::CheckerKind::ASSERTION : clam::CheckerKind::NOCHECKS);
  m_params.check_verbose = CrabCheckVerbose;
  
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
  
  if (m_params.run_inter) {
    m_ga.reset(new InterGlobalClam(M, *m_cfg_builder_man));
  } else {
    m_ga.reset(new IntraGlobalClam(M, *m_cfg_builder_man));
  }
  abs_dom_map_t abs_dom_assumptions /*no assumptions*/;    
  m_ga->analyze(m_params, abs_dom_assumptions);

  
//   if (builder_params.dot_cfg) {
//     for (auto &F : M) {
//       if (m_cfg_builder_man->hasCfg(F)) {
//         cfg_t &cfg = m_cfg_builder_man->getCfg(F);
// #if 1
// 	// Print invariants 
//         auto pre_fn = [this](const basic_block_label_t &node)
//             -> boost::optional<clam_abstract_domain> {
//           if (const BasicBlock *BB = node.get_basic_block()) {
//             llvm::Optional<clam_abstract_domain> res = getPre(BB);
//             if (res.hasValue()) {
//               return res.getValue();
//             }
//           }
//           return boost::optional<clam_abstract_domain>();
//         };
//         auto post_fn = [this](const basic_block_label_t &node)
//             -> boost::optional<clam_abstract_domain> {
//           if (const BasicBlock *BB = node.get_basic_block()) {
//             llvm::Optional<clam_abstract_domain> res = getPost(BB);
//             if (res.hasValue()) {
//               return res.getValue();
//             }
//           }
//           return boost::optional<clam_abstract_domain>();
//         };

// 	crab::cfg::cfg_to_dot<cfg_t, clam_abstract_domain>(cfg, pre_fn, post_fn,
// 							   m_ga->getChecksDB());
// #else
// 	// Only CFG
// 	crab::cfg::cfg_to_dot(cfg);
// #endif
//       }
//     }
//   }
  
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
    AU.addRequired<seadsa::CompleteCallGraph>();
  }

  AU.addRequired<UnifyFunctionExitNodesLegacyPass>();
  AU.addRequired<clam::NameValues>();
}

/**
 * For clam clients
 **/

ClamGlobalAnalysis& ClamPass::getClamGlobalAnalysis() {
  assert(m_ga);
  return *m_ga;
}

const ClamGlobalAnalysis& ClamPass::getClamGlobalAnalysis() const {
  assert(m_ga);
  return *m_ga;
}

bool ClamPass::hasCfg(llvm::Function &F) {
  return m_cfg_builder_man->hasCfg(F);
}

cfg_ref_t ClamPass::getCfg(llvm::Function &F) {
  assert(m_cfg_builder_man->hasCfg(F));
  return m_cfg_builder_man->getCfg(F);
}

CrabBuilderManager &ClamPass::getCfgBuilderMan() {
  return *m_cfg_builder_man;
}

const CrabBuilderManager &ClamPass::getCfgBuilderMan() const {
  return *m_cfg_builder_man;
}
  
// return invariants that hold at the entry of block
llvm::Optional<clam_abstract_domain>
ClamPass::getPre(const llvm::BasicBlock *block, bool keep_shadows) const {
  return m_ga->getPre(block, keep_shadows);
}

// return invariants that hold at the exit of block
llvm::Optional<clam_abstract_domain>
ClamPass::getPost(const llvm::BasicBlock *block, bool keep_shadows) const {
  return m_ga->getPost(block, keep_shadows);  
}

bool ClamPass::hasFeasibleEdge(const llvm::BasicBlock *b1,
                               const llvm::BasicBlock *b2) const {
  return m_ga->hasFeasibleEdge(b1, b2);
}

/**
 * For assertion checking
 **/

unsigned ClamPass::getTotalChecks() const {
  return getTotalSafeChecks() + getTotalErrorChecks() + getTotalWarningChecks();
}

unsigned ClamPass::getTotalSafeChecks() const {
  return m_ga->getChecksDB().get_total_safe();
}

unsigned ClamPass::getTotalErrorChecks() const {
  return m_ga->getChecksDB().get_total_error();
}

unsigned ClamPass::getTotalWarningChecks() const {
  return m_ga->getChecksDB().get_total_warning();
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

char clam::ClamPass::ID = 0;
} // namespace clam

static RegisterPass<clam::ClamPass> X("clam", "Infer invariants using Crab",
                                      false, false);
