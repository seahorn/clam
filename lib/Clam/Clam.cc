#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

#include "clam/CfgBuilder.hh"
#include "clam/Clam.hh"
#include "clam/crab/crab_lang.hh"
#include "clam/crab/crab_domains.hh"
#include "clam/Support/Debug.hh"
#include "clam/Support/NameValues.hh"
#include "clam/config.h"
#include "./crab/path_analyzer.hpp"
/** Wrappers for pointer analyses **/
#include "clam/DummyHeapAbstraction.hh"
#include "clam/SeaDsaHeapAbstraction.hh"

#include "seadsa/AllocWrapInfo.hh"
#include "seadsa/ShadowMem.hh"

#include "crab/analysis/bwd_analyzer.hpp"
#include "crab/analysis/fwd_analyzer.hpp"
#include "crab/analysis/inter/top_down_inter_analyzer.hpp"
#include "crab/analysis/dataflow/assumptions.hpp"
#include "crab/analysis/dataflow/liveness.hpp"
#include "crab/cg/cg.hpp"
#include "crab/cg/cg_bgl.hpp"
#include "crab/checkers/assertion.hpp"
#include "crab/checkers/checker.hpp"
#include "crab/support/debug.hpp"
#include "crab/support/stats.hpp"

#include <functional>
#include <map>
#include <memory>
#include <unordered_map>
#include <unordered_set>

using namespace llvm;
using namespace clam;
// TO BE REMOVED?
using namespace crab::analyzer;
using namespace crab::checker;

#include "ClamOptions.def"

// In C++11 we need to pass to std::bind "this" (if class member
// functions are called) as well as all placeholders for each
// argument. This is wrapper to avoid that and improve readability.
template <class C, typename Ret, typename ... Ts>
static std::function<Ret(Ts...)> bind_this(C* c, Ret(C::*m)(Ts...)) {
  return [=](Ts &&... args) {
    return (c->*m)(std::forward<decltype(args)>(args)...);
  };
}

namespace clam {
  
  /** Begin typedefs **/
  typedef crab::analyzer::live_and_dead_analysis<cfg_ref_t> liveness_t;
  typedef std::unordered_map<cfg_ref_t, const liveness_t*> liveness_map_t;
  typedef typename IntraClam::checks_db_t checks_db_t;
  typedef typename IntraClam::abs_dom_map_t abs_dom_map_t;
  typedef typename IntraClam::lin_csts_map_t lin_csts_map_t;
  /** End typedefs **/

  #if 0
  /** Begin global counters **/
  static unsigned num_invars; // some measure for the size of invariants
  static unsigned num_nontrivial_blocks;
  /** End global counters **/
  #endif

  static bool isRelationalDomain(CrabDomain dom) {
  return (dom == ZONES_SPLIT_DBM || dom == OCT || dom == PK ||
          dom == TERMS_ZONES);
  }

  static bool isTrackable(const Function &fun) {
    return !fun.isDeclaration() && !fun.empty() && !fun.isVarArg();
  }

  /** convenient wrapper for the analysis datastructures **/
  struct AnalysisResults {
    using edges_set =
      std::set<std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>>;
    
    // invariants that hold at the entry of a block
    abs_dom_map_t &premap;
    // invariants that hold at the exit of a block
    abs_dom_map_t &postmap;
    // infeasible edges (for debugging or pretty-printing)
    edges_set infeasible_edges;
    // database with all the checks
    checks_db_t &checksdb;

    AnalysisResults(abs_dom_map_t &pre, abs_dom_map_t &post, checks_db_t &db)
      : premap(pre), postmap(post), checksdb(db) {} 
  };

  /** return invariant for block in table but filtering out shadow_varnames **/
  static llvm::Optional<clam_abstract_domain>
  lookup(const abs_dom_map_t &table,
	 const llvm::BasicBlock &block,
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
      for(unsigned i=0; i<shadow_vars.size(); ++i) {
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
      
  /** Pretty-printer utilities **/
  namespace pretty_printer_impl {

    /** Generic class for a block annotation **/
    class block_annotation {
    public:
      typedef typename cfg_ref_t::statement_t statement_t;
      
      block_annotation() {}
      virtual ~block_annotation() {}

      virtual std::string name() const = 0;
      virtual void print_begin(basic_block_label_t bbl, crab::crab_os &o) const {}
      virtual void print_end(basic_block_label_t bbl, crab::crab_os &o) const {}
      virtual void print_begin(const statement_t &s, crab::crab_os &o) const {}
      virtual void print_end(const statement_t &s, crab::crab_os &o) const {}
    };

    /** Annotation for invariants **/
    class invariant_annotation: public block_annotation {
    private:
      const abs_dom_map_t &m_premap;
      const abs_dom_map_t &m_postmap;
      std::vector<varname_t> m_shadow_vars;
      
    public:
      invariant_annotation(const llvm_variable_factory &vfac,
			   const abs_dom_map_t &premap,
			   const abs_dom_map_t &postmap, const bool keep_shadows)
	: block_annotation(), m_premap(premap), m_postmap(postmap) {
	if (keep_shadows) {
	  m_shadow_vars.reserve(std::distance(vfac.get_shadow_vars().begin(),
					      vfac.get_shadow_vars().end()));
	  m_shadow_vars.insert(m_shadow_vars.begin(),
			       vfac.get_shadow_vars().begin(),
			       vfac.get_shadow_vars().end());
	}
      }
      
      std::string name() const { return "INVARIANTS";}
      
      void print_begin(basic_block_label_t bbl, crab::crab_os &o) const {
	if (const llvm::BasicBlock *bb = bbl.get_basic_block()) {
	  auto pre = lookup(m_premap, *bb, m_shadow_vars);
	  o << "  " << name() << ": ";
	  if (pre.hasValue()){
	    o << pre.getValue() << "\n";
	  } else {
	    o << "null\n";
	  }
	}
      }
      
      void print_end(basic_block_label_t bbl, crab::crab_os &o) const {
	if (const llvm::BasicBlock *bb = bbl.get_basic_block()) {
	  auto post = lookup(m_postmap, *bb, m_shadow_vars);
	  o << "  " << name() << ": ";
	  if (post.hasValue()) {
	    o << post.getValue() << "\n";
	  } else {
	    o << "null\n";
	  }
	}
      }
    };

    /** Annotation for unjustified assumptions done by the analysis **/
    class unjust_assumption_annotation: public block_annotation {
    private:
      typedef
      typename assumption_analysis<cfg_ref_t>::assumption_ptr assumption_ptr;
      
    public:
      typedef assumption_analysis<cfg_ref_t> unjust_assumption_analysis_t;
      
    private:
      typedef typename cfg_ref_t::statement_t statement_t;
      
      cfg_ref_t m_cfg;
      unjust_assumption_analysis_t *m_analyzer;
      
    public:
      unjust_assumption_annotation(cfg_ref_t cfg,
				   unjust_assumption_analysis_t *analyzer)
	: block_annotation(), m_cfg(cfg), m_analyzer(analyzer) { }
      
      std::string name() const { return "UNJUSTIFIED ASSUMPTIONS";}
      
      void print_begin(const statement_t &s, crab::crab_os &o) const {
	std::vector<assumption_ptr> assumes;
	if (s.is_assert()) {
	  typedef typename cfg_ref_t::basic_block_t::assert_t assert_t;
	  m_analyzer->get_assumptions(static_cast<const assert_t *>(&s), assumes);
	  if (!assumes.empty()) {
	    o << "  /** assert verified as ";
	    for (std::vector<assumption_ptr>::iterator it = assumes.begin(),
		   et = assumes.end();
             it != et;) {
	      o << (*it)->get_id_str();
	      ++it;
	      if (it != et)
		o << ",";
	      else
		o << ";";
	    }
	    o << "**/\n";
	  }
	} else {
	  m_analyzer->get_originated_assumptions(&s, assumes);
	  for (auto assume_ptr: assumes) {
        o << "  /** ";
        assume_ptr->write(o);
        o << "**/\n";
	  }
	}
      }
    };
    
    /** Print a block together with its annotations **/
    class print_block {
      cfg_ref_t m_cfg;
      crab::crab_os &m_o;
      const std::vector<std::unique_ptr<block_annotation>> &m_annotations;

    public:
      print_block(cfg_ref_t cfg, crab::crab_os &o,
		   const std::vector<std::unique_ptr<block_annotation>> &annotations)
	: m_cfg(cfg), m_o(o), m_annotations(annotations) {} 

      void operator()(basic_block_label_t bbl) const {
	// do not print block if no annotations
	if (m_annotations.empty())
	  return;
	
	m_o << bbl.get_name() << ":\n";
	
	crab::crab_string_os o;
	for (auto& p: m_annotations) {
	  p->print_begin(bbl,o);
	}
	if (o.str() != "") {
	  m_o << "/**\n" << o.str() << "**/\n";
	}
	
	const basic_block_t &bb = m_cfg.get_node(bbl);
	bool empty_block = (std::distance(bb.begin(), bb.end()) == 0);
	for (auto const &s: bb) {
	  for (auto& p: m_annotations) {
	    p->print_begin(s, m_o);
	  }	  
	  m_o << "  " << s << ";\n";
	  for (auto& p: m_annotations) {
	    p->print_end(s, m_o);
	  }	  
	}
	if (!empty_block) {
	  crab::crab_string_os o;
	  for (auto& p: m_annotations) {
	    p->print_end(bbl, o);
	  }
	  if (o.str() != "") {
	    m_o << "/**\n" << o.str() << "**/\n";
	  }
	}

    std::pair<cfg_ref_t::const_succ_iterator, cfg_ref_t::const_succ_iterator>
        p = bb.next_blocks();
	cfg_ref_t::const_succ_iterator it = p.first;
	cfg_ref_t::const_succ_iterator et = p.second;
	if (it != et) {
	  m_o << "  "
          << "goto ";
	  for (; it != et; ) {
	    m_o << crab::basic_block_traits<basic_block_t>::to_string(*it);
	    ++it;
	    if (it == et) {
	      m_o << ";";
	    } else {
	      m_o << ",";
	    }
	  }
	}
	m_o << "\n";
      }
    };

    typedef std::unordered_set<basic_block_label_t> visited_t;
    template<typename T>
    void dfs_rec(cfg_ref_t cfg, basic_block_label_t curId, visited_t &visited,
		 T f) {
      if (visited.find(curId) != visited.end())
	return;
      visited.insert(curId);
      const basic_block_t &cur = cfg.get_node(curId);
      f(curId);
      for (auto const n : llvm::make_range(cur.next_blocks())) {
    	dfs_rec(cfg, n, visited, f);
      }
    }
    
    template <typename T> void dfs(cfg_ref_t cfg, T f) {
      visited_t visited;
      dfs_rec(cfg, cfg.entry(), visited, f);
    }
    
    void print_annotations(cfg_ref_t cfg,
			   const std::vector<std::unique_ptr<block_annotation>> &annotations) {
      print_block f(cfg, crab::outs(), annotations);
      dfs(cfg, f);
    }
} // namespace pretty_printer_impl


  struct intra_analysis {
    std::function<void(const AnalysisParams &, const BasicBlock *,
		       clam_abstract_domain,
		       const abs_dom_map_t &, const lin_csts_map_t &,
		       const liveness_t *, AnalysisResults &)>
    analyze;
    clam_abstract_domain entry;      
    std::string name;
  };

  static std::map<CrabDomain, intra_analysis> s_intra_analyses;


  std::string AnalysisParams::abs_dom_to_str() const {
    switch (dom) {
    case INTERVALS: {
      interval_domain_t abs;
      return abs.domain_name();
    }
    case INTERVALS_CONGRUENCES: {
      ric_domain_t abs;
      return abs.domain_name();
    }
    case BOXES:{
      boxes_domain_t abs;
      return abs.domain_name();      
    }
    case DIS_INTERVALS: {
      dis_interval_domain_t abs;
      return abs.domain_name();            
    }
    case ZONES_SPLIT_DBM: {
      split_dbm_domain_t abs;
      return abs.domain_name();            
    }
    case TERMS_DIS_INTERVALS: {
      term_dis_int_domain_t abs;
      return abs.domain_name();            
    }
    case TERMS_ZONES: {
      num_domain_t abs;
      return abs.domain_name();            
    }
    case OCT: {
      oct_domain_t abs;
      return abs.domain_name();            
    }
    case PK: {
      pk_domain_t abs;
      return abs.domain_name();            
    }
    case WRAPPED_INTERVALS: {
      wrapped_interval_domain_t abs;
      return abs.domain_name();            
    }
    default:
      return "none";
    }
  }    

  /**
   * Internal implementation of the intra-procedural analysis
   **/
  class IntraClam_Impl {
  public:
    IntraClam_Impl(const Function &fun, CrabBuilderManager &man)
      : m_cfg_builder(nullptr), m_fun(fun), m_vfac(man.get_var_factory()) {
      
      if (isTrackable(m_fun)) {
	if (!man.has_cfg(m_fun)) {
	  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
			  << "Started Crab CFG construction for "
			  << fun.getName() << "\n");	  
	  m_cfg_builder = man.mk_cfg_builder(m_fun);
	  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
			  << "Finished Crab CFG construction for "
			  << fun.getName() << "\n");	
	} else {
	  m_cfg_builder = man.get_cfg_builder(m_fun);
	}
      } else {
	CRAB_VERBOSE_IF(1, llvm::outs() << "Cannot build CFG for "
			                << fun.getName() << "\n");
      }

      // Register the analyses

      interval_domain_t interval_domain_val;
      registerIntraAnalysis(INTERVALS, "intervals",
			    interval_domain_val);

      split_dbm_domain_t split_dbm_domain_val;
      registerIntraAnalysis(ZONES_SPLIT_DBM, "zones",
			    split_dbm_domain_val);
    }

    void Analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
		 // assumptions can be provided in abs_dom format or
		 // as linear constraints.
		 const abs_dom_map_t  &abs_dom_assumptions,
		 const lin_csts_map_t &lin_csts_assumptions,
		 AnalysisResults &results) {

      if (!m_cfg_builder) {
	CRAB_VERBOSE_IF(1, llvm::outs() << "Skipped analysis for "
			                << m_fun.getName() << "\n");
	return;
      }

      const liveness_t* live = nullptr;
      if (params.run_liveness || isRelationalDomain(params.dom)) {
	// -- run liveness
	m_cfg_builder->compute_live_symbols();
	if (isRelationalDomain(params.dom)) {
	  live = m_cfg_builder->get_live_symbols();
	  assert(live);	  
	  unsigned total_live, avg_live_per_blk, max_live_per_blk;
	  live->get_stats(total_live, max_live_per_blk, avg_live_per_blk);
        CRAB_VERBOSE_IF(
            1, crab::outs()
                   << "Max live per block: " << max_live_per_blk << "\n"
                   << "Threshold: " << params.relational_threshold << "\n");
	  if (max_live_per_blk > params.relational_threshold) {
	    // default domain
	    params.dom = INTERVALS;
	  }
	}
      }

      if (CrabBuildOnlyCFG) {
	return;
      }
      
      if (s_intra_analyses.count(params.dom)) {
	s_intra_analyses.at(params.dom)
	.analyze(params, entry, s_intra_analyses.at(params.dom).entry,
		 abs_dom_assumptions, lin_csts_assumptions,
		 (params.run_liveness) ? live : nullptr, results);
      } else {
      	CLAM_WARNING("Warning: abstract domain not found or enabled.");
      }
    }
    
    bool pathAnalyze(const AnalysisParams& params,
		     const std::vector<const llvm::BasicBlock*>& blocks,
		     bool layered_solving, 
		     std::vector<statement_t*>& core,
		     bool populate_inv_map, abs_dom_map_t& post) const {
#if 0 /* TO_BE_UPDATED */      
      assert(m_cfg_builder);

      // build the full path (included internal basic blocks added
      // during the translation to Crab)
      std::vector<basic_block_label_t> path;
      path.reserve(blocks.size());
      for(unsigned i=0; i < blocks.size(); ++i) {
	path.push_back(m_cfg_builder->get_crab_basic_block(blocks[i]));
	if (i < blocks.size() - 1) {
	  if (const basic_block_label_t* edge_bb =
	      m_cfg_builder->get_crab_basic_block(blocks[i], blocks[i+1])) {
	    path.push_back(*edge_bb);
	  }
	}
      }

      bool res;
      if (path_analyses.count(params.dom)) {
      path_analyses.at(params.dom)
          .analyze(path, core, layered_solving, populate_inv_map, post, res);
      } else {
      	CLAM_WARNING("Warning: abstract domain not found or enabled.");
      }
      return res;
#else
      return true;
#endif      
    }
    
  private:
    CrabBuilderManager::CfgBuilderPtr m_cfg_builder;
    const Function &m_fun;
    llvm_variable_factory &m_vfac;

    // helper to get a reference to a crab cfg from the builder
    cfg_t& get_cfg() { return m_cfg_builder->get_cfg(); }
    
    void analyzeCfg(const AnalysisParams &params,
		    const BasicBlock *entry,
		    clam_abstract_domain entry_abs,
		    const abs_dom_map_t &abs_dom_assumptions,
		    const lin_csts_map_t &lin_csts_assumptions,
		    const liveness_t *live, AnalysisResults &results) {
      
      // -- we use the combined forward/backward analyzer
      using intra_analyzer_t =
	crab::analyzer::intra_forward_backward_analyzer<cfg_ref_t, clam_abstract_domain>;
      // -- property checker
      using intra_checker_t =
	crab::checker::intra_checker<intra_analyzer_t>;
      
      CRAB_VERBOSE_IF(1, auto fdecl = get_cfg().get_func_decl();
		      crab::get_msg_stream()
		      << "Running intra-procedural analysis with "
		      << "\"" << entry_abs.domain_name()  << "\""
		      << " for " << fdecl.get_func_name() << "  ... \n";);
      
      // -- run intra-procedural analysis
      intra_analyzer_t analyzer(get_cfg(), entry_abs);
      typename intra_analyzer_t::assumption_map_t crab_assumptions;

      // Reconstruct a crab assumption map from an abs_dom_map_t
      for (auto &kv: abs_dom_assumptions) {
	crab_assumptions.insert(
	  {m_cfg_builder->get_crab_basic_block(kv.first), kv.second});
      }
      
      // Reconstruct a crab assumption map from a lin_csts_map_t
      for (auto &kv: lin_csts_assumptions) {
	clam_abstract_domain absval = entry_abs.make_top();
	absval += kv.second;
	crab_assumptions.insert(
          {m_cfg_builder->get_crab_basic_block(kv.first), absval});
      }
      
      analyzer.run(m_cfg_builder->get_crab_basic_block(entry), entry_abs, 
		   !params.run_backward, crab_assumptions, live,
		   params.widening_delay, params.narrowing_iters,
		   params.widening_jumpset);
      CRAB_VERBOSE_IF(1, crab::get_msg_stream()
		      << "Finished intra-procedural analysis.\n");

      // -- store invariants
      if (params.store_invariants || params.print_invars) {
	CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Storing invariants.\n");       
	for (basic_block_label_t bl :
	       llvm::make_range(get_cfg().label_begin(), get_cfg().label_end())) {
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
	    update(results.premap, *B,  analyzer.get_pre(bl));
	    // --- invariants that hold at the exit of the blocks
	    update(results.postmap, *B, analyzer.get_post(bl));
	    #if 0
	    if (params.stats) {
	      unsigned num_block_invars = 0;
	      // XXX: for boxes it would be more useful to get a measure
	      // from to_disjunctive_linear_constraint_system() but it
	      // can be really slow. 
	      num_block_invars += pre.to_linear_constraint_system().size();
	      num_invars += num_block_invars;
	      if (num_block_invars > 0) num_nontrivial_blocks++;
	    }
            #endif 
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
	
	typedef pretty_printer_impl::block_annotation block_annotation_t;
	typedef pretty_printer_impl::invariant_annotation inv_annotation_t;
	typedef pretty_printer_impl::unjust_assumption_annotation
          unjust_assume_annotation_t;
	std::vector<std::unique_ptr<block_annotation_t>> pool_annotations;
	
	if (get_cfg().has_func_decl()) {
	  auto fdecl = get_cfg().get_func_decl();
	  crab::outs() << "\n" << fdecl << "\n";
	} else {
	  llvm::outs() << "\n"
		       << "function " << m_fun.getName() << "\n";
	}
	if (params.print_invars) {
	  pool_annotations.emplace_back(std::make_unique<inv_annotation_t>(
            m_vfac, results.premap, results.postmap, params.keep_shadow_vars));
	}

	// XXX: it must be alive when print_annotations is called.
	#if 0
	assumption_naive_analysis<cfg_ref_t> unjust_assumption_analyzer(get_cfg());
	#else
	assumption_dataflow_analysis<cfg_ref_t> unjust_assumption_analyzer(get_cfg());
	#endif 
	
	if (params.print_unjustified_assumptions) {
	  // -- run first the analysis
	  unjust_assumption_analyzer.exec();
        pool_annotations.emplace_back(std::make_unique<unjust_assume_annotation_t>(
            get_cfg(), &unjust_assumption_analyzer));
	}

	pretty_printer_impl::print_annotations(get_cfg(), pool_annotations);
      }
          
      if (params.check) {
	// --- checking assertions and collecting data
	CRAB_VERBOSE_IF(1, crab::get_msg_stream()
			<< "Checking assertions ... \n");
	typename intra_checker_t::prop_checker_ptr
	  prop(std::make_shared<assert_property_checker<intra_analyzer_t>>(params.check_verbose));
	intra_checker_t checker(analyzer, {prop});
	checker.run();
	CRAB_VERBOSE_IF(1, llvm::outs() << "Function " << m_fun.getName() << "\n";
			checker.show(crab::outs()));
	results.checksdb += checker.get_all_checks();
	CRAB_VERBOSE_IF(1, crab::get_msg_stream()
			<< "Finished assert checking.\n");
      }
      return;
    }

    template<typename AbsDom>
    void wrapperPathAnalyze(const std::vector<basic_block_label_t>& path,
			    std::vector<statement_t*>& core,
			    bool layered_solving, bool populate_inv_map,
			    abs_dom_map_t& post, bool &res) {
     #if 0 /* TO_BE_UPDATED */
      using path_analyzer_t = path_analyzer<cfg_ref_t, AbsDom>;
      
      AbsDom init;
      path_analyzer_t path_analyzer(get_cfg(), init);
      res = path_analyzer.solve(path, layered_solving);
      if (populate_inv_map) {
	for(auto n: path) {
	  if (const llvm::BasicBlock* bb = n.get_basic_block()) {
	    AbsDom abs_val = path_analyzer.get_fwd_constraints(n);
	    post.insert({bb, mkGenericAbsDomWrapper(abs_val)});
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
      #endif 
    }


    #if 0 /* TO_BE_UPDATED */
    struct path_analysis {
      std::function<void(const std::vector<basic_block_label_t>&,
			 std::vector<statement_t*> &, bool, bool,
			 abs_dom_map_t &, bool &)>
        analyze;
      std::string name;
    };
    #endif
    
    
    // const std::map<CrabDomain, intra_analysis> intra_analyses {
    //   {ZONES_SPLIT_DBM,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<split_dbm_domain_t>),
    //     "zones"}},
    //   {INTERVALS_CONGRUENCES,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<ric_domain_t>),
    //     "reduced product of intervals and congruences"}},
    //   {DIS_INTERVALS,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<dis_interval_domain_t>),
    //     "disjunctive intervals"}},
    //   {TERMS_INTERVALS,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<term_int_domain_t>),
    //     "terms with intervals"}},
    //   {WRAPPED_INTERVALS,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<wrapped_interval_domain_t>),
    //     "wrapped intervals"}},
    //   {TERMS_ZONES,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<num_domain_t>),
    //     "terms with zones"}},
    //   {TERMS_DIS_INTERVALS,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<term_dis_int_domain_t>),
    //     "terms with disjunctive intervals"}},
    //   {OCT,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<oct_domain_t>),
    //     "octagons"}},
    //   {BOXES,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<boxes_domain_t>), "boxes"}},
    //   {PK,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<pk_domain_t>),
    //     "polyhedra"}},
    //   {INTERVALS,
    //    {bind_this(this, &IntraClam_Impl::analyzeCfg<interval_domain_t>),
    //     "classical intervals"}}
    // };

    template<typename AbsDom>
    void registerIntraAnalysis(CrabDomain abs_dom_kind,
			       std::string analysis_name,
			       AbsDom dom) {
      auto it = s_intra_analyses.find(abs_dom_kind);
      if (it != s_intra_analyses.end()) {
	CLAM_ERROR("Clam analysis ", analysis_name, " registered twice");
      }
      
      clam_abstract_domain abs_wrapper(dom.make_top());
      s_intra_analyses.insert({abs_dom_kind,
			       {bind_this(this, &IntraClam_Impl::analyzeCfg),
				abs_wrapper,
				analysis_name}});
      crab::outs() << "Registered clam analysis " << analysis_name << "\n";
    }

#if 0 /* TO_BE_UPDATED */	
    // Domains used for path-based analysis
    const std::map<CrabDomain, path_analysis> path_analyses {
      {ZONES_SPLIT_DBM,
       {bind_this(this,
                  &IntraClam_Impl::wrapperPathAnalyze<split_dbm_domain_t>),
        "zones"}}
      ,
      {INTERVALS,
       {bind_this(this, &IntraClam_Impl::wrapperPathAnalyze<interval_domain_t>),
        "classical intervals"}},
      {TERMS_INTERVALS,
       {bind_this(this, &IntraClam_Impl::wrapperPathAnalyze<term_int_domain_t>),
        "terms with intervals"}},
      {WRAPPED_INTERVALS,
       {bind_this(
            this,
            &IntraClam_Impl::wrapperPathAnalyze<wrapped_interval_domain_t>),
        "wrapped intervals"}},
      {TERMS_ZONES,
       {bind_this(this, &IntraClam_Impl::wrapperPathAnalyze<num_domain_t>),
        "terms with zones"}},
      {BOXES,
       {bind_this(this, &IntraClam_Impl::wrapperPathAnalyze<boxes_domain_t>),
        "boxes"}}
      /* 
	 To add new domains here make sure you add an explicit
	 instantiation in crab/path_analyzer.cc 
      */
      //, { TERMS_DIS_INTERVALS , { bind_this(this,
      //&IntraClam_Impl::wrapperPathAnalyze<term_dis_int_domain_t>), "terms with
      // disjunctive intervals" }}
    };
#endif 	
  }; // end class

  /**
   *   Begin IntraClam methods
   **/
  IntraClam::IntraClam(const Function &fun, CrabBuilderManager &man)
    : m_impl(nullptr), m_fun(fun), m_builder_man(man) {
  m_impl = std::make_unique<IntraClam_Impl>(m_fun, m_builder_man);
  }

  IntraClam::~IntraClam() {}

  void IntraClam::clear() {
    m_pre_map.clear();
    m_post_map.clear();
    m_checks_db.clear();
  }

CrabBuilderManager &IntraClam::get_cfg_builder_man() { return m_builder_man; }
  
  void IntraClam::analyze(AnalysisParams &params,
			  const abs_dom_map_t &assumptions) {    
  AnalysisResults results = {m_pre_map, m_post_map, m_checks_db}; 
  lin_csts_map_t lin_csts_assumptions;
  m_impl->Analyze(params, &(m_fun.getEntryBlock()), assumptions,
                  lin_csts_assumptions, results);
  }

  void IntraClam::analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
			  const abs_dom_map_t &assumptions) {
    AnalysisResults results = {m_pre_map, m_post_map, m_checks_db};
    lin_csts_map_t lin_csts_assumptions;    
    m_impl->Analyze(params, entry, assumptions, lin_csts_assumptions, results);
  }

  void IntraClam::analyze(AnalysisParams &params,
			  const lin_csts_map_t &assumptions) {    
    AnalysisResults results = {m_pre_map, m_post_map, m_checks_db};
    abs_dom_map_t abs_dom_assumptions;
    m_impl->Analyze(params, &(m_fun.getEntryBlock()), abs_dom_assumptions,
		    assumptions, results);
  }

  void IntraClam::analyze(AnalysisParams &params, const llvm::BasicBlock *entry,
			  const lin_csts_map_t  &assumptions) {
    AnalysisResults results = {m_pre_map, m_post_map, m_checks_db};
    abs_dom_map_t abs_dom_assumptions;    
    m_impl->Analyze(params, entry, abs_dom_assumptions, assumptions, results);
  }

  bool IntraClam::path_analyze(
    const AnalysisParams &params,
    const std::vector<const llvm::BasicBlock *> &path, bool layered_solving,
    std::vector<statement_t*>& core) const {
    abs_dom_map_t post_conditions;
    return m_impl->pathAnalyze(params, path, layered_solving, core, false,
			       post_conditions);
  }

  bool IntraClam::path_analyze(const AnalysisParams& params,
			       const std::vector<const llvm::BasicBlock*>& path,
			       bool layered_solving, 
			       std::vector<statement_t*>& core,
			       abs_dom_map_t& post_conditions) const {
  return m_impl->pathAnalyze(params, path, layered_solving, core, true,
                             post_conditions);
  }

  llvm::Optional<clam_abstract_domain> IntraClam::get_pre(const llvm::BasicBlock *block,
							  bool keep_shadows) const {
    std::vector<varname_t> shadows;
    auto &vfac = m_builder_man.get_var_factory();    
    if (!keep_shadows)
      shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
				       vfac.get_shadow_vars().end());
    return lookup(m_pre_map, *block, shadows);
  }   

  llvm::Optional<clam_abstract_domain> IntraClam::get_post(const llvm::BasicBlock *block,
							   bool keep_shadows) const {
    std::vector<varname_t> shadows;
    auto &vfac = m_builder_man.get_var_factory();        
    if (!keep_shadows)
      shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
				       vfac.get_shadow_vars().end());
    return lookup(m_post_map, *block, shadows);
  }

  const checks_db_t& IntraClam::get_checks_db() const { return m_checks_db;}
  
  /**
   *   End IntraClam methods
   **/

  /**
   *   Internal implementation of the inter-procedural analysis
   **/
  class InterClam_Impl {
  public:
    InterClam_Impl(const Module& M, CrabBuilderManager &man)
      : m_cg(nullptr), m_crab_builder_man(man), m_M(M)  {

      std::vector<cfg_ref_t> cfg_ref_vector;
      for (auto const &F : m_M) {
        if (isTrackable(F)) {
	  // -- build cfg's
	  if (!man.has_cfg(F)) {
	    m_crab_builder_man.mk_cfg_builder(F);
	  }
	  cfg_t* cfg = &(m_crab_builder_man.get_cfg(F));
	  cfg_ref_vector.push_back(*cfg);
        CRAB_VERBOSE_IF(1, llvm::outs()
                               << "Built Crab CFG for " << F.getName() << "\n");
	} else {
	  CRAB_VERBOSE_IF(1, llvm::outs() << "Cannot build CFG for "
			                  << F.getName() << "\n");
	}
      }
      // build call graph
    m_cg =
        std::make_unique<cg_t>(cfg_ref_vector.begin(), cfg_ref_vector.end());
    }
    
    void Analyze(AnalysisParams &params,
		 // assumptions can be provided in abs_dom format or
		 // as linear constraints.
		 const abs_dom_map_t &abs_dom_assumptions /*unused*/,
		 const lin_csts_map_t &lin_csts_assumptions /*unused*/,
		 AnalysisResults &results) {
#if 0 /* TO_BE_UPDATED */
      // If the number of live variables per block is too high we
      // switch to a cheap domain regardless what the user wants.
      CrabDomain absdom =  params.dom;
      
      /* Compute liveness information and choose statically the
	 abstract domain */

      if (params.run_liveness || isRelationalDomain(absdom)) {
	unsigned max_live_per_blk = 0;
	for (auto cg_node: llvm::make_range(vertices(*m_cg))) {
	  const liveness_t* live = nullptr;

	  // Get the cfg builder to run liveness
	  if (const Function *fun = m_M.getFunction(cg_node.name())) {
	    auto cfg_builder = m_crab_builder_man.get_cfg_builder(*fun);
	    assert(cfg_builder);
	    // run liveness
	    cfg_builder->compute_live_symbols();
	    live = cfg_builder->get_live_symbols();
	    // update max number of live variables for whole cg
	    unsigned total_live, max_live_per_blk_, avg_live_per_blk;
	    live->get_stats(total_live, max_live_per_blk_, avg_live_per_blk);
	    max_live_per_blk = std::max(max_live_per_blk, max_live_per_blk_);
	  }

	  if (isRelationalDomain(absdom)) {
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
	      absdom = INTERVALS;
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
	if (inter_analyses.count(params.dom)) {
	  inter_analyses.at(params.dom).analyze(params, results);
	} else {
	  if (inter_analyses.count(ZONES_SPLIT_DBM)) {
	    crab::outs() << "Warning: abstract domains not found or enabled.\n"
			 << "Compile with -DALL_DOMAINS=ON.\n";	    
	  } else {
          crab::outs()
              << "Warning: inter-procedural analysis is not enabled.\n"
			 << "Compile with -DENABLE_INTER=ON or do not use --crab-inter\n";
	  }
	}
      }
#endif       
    }
    
  private:
    // crab call graph 
    std::unique_ptr<cg_t> m_cg;
    // crab cfg builder manager
    CrabBuilderManager& m_crab_builder_man;
    // the LLVM module
    const Module& m_M;    
    // live symbols
    liveness_map_t m_live_map;

    basic_block_label_t get_crab_basic_block(const BasicBlock* bb) const {
      const Function*f = bb->getParent();
      auto builder = m_crab_builder_man.get_cfg_builder(*f);
      return builder->get_crab_basic_block(bb);
    }

#if 0 /* TO_BE_UPDATED */    
    /** Run inter-procedural analysis on the whole call graph **/
    template<typename Dom>
    void analyzeCg(const AnalysisParams &params, AnalysisResults &results) {

    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Running top-down inter-procedural analysis "
		                             << "with domain:" 
		                             << "\"" << Dom::getDomainName() << "\""
		                             << "  ...\n";);

      typedef top_down_inter_analyzer<cg_ref_t, Dom> inter_analyzer_t;
      typedef top_down_inter_analyzer_parameters<cg_ref_t> inter_params_t;      

      inter_params_t inter_params;
      inter_params.run_checker = params.check;
      inter_params.checker_verbosity  = params.check_verbose;
      inter_params.minimize_invariants = true;
      inter_params.max_call_contexts = params.max_calling_contexts;
      inter_params.live_map = (params.run_liveness ? &m_live_map : nullptr);
      inter_params.widening_delay = params.widening_delay;
      inter_params.descending_iters = params.narrowing_iters;
      inter_params.thresholds_size = params.widening_jumpset;
      inter_analyzer_t analyzer(*m_cg, inter_params);
      analyzer.run(Dom::top());
      if (inter_params.run_checker) {
	results.checksdb += analyzer.get_all_checks();
      }
    
    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Finished inter-procedural analysis.\n");
      
      // -- store invariants
      if (params.store_invariants || params.print_invars) {
	CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Storing invariants.\n");
      }
      
      for (auto &n: llvm::make_range(vertices(*m_cg))) {
	cfg_ref_t cfg = n.get_cfg();
	if (const Function *F = m_M.getFunction(n.name())) {
	  if (params.store_invariants || params.print_invars) {
	    for (basic_block_label_t bl:
		   llvm::make_range(cfg.label_begin(),cfg.label_end())) {
	      if (bl.is_edge()) {
		// Note that we use get_post instead of get_pre:
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
		auto pre = analyzer.get_pre(cfg, get_crab_basic_block(B));
		update(results.premap, *B, mkGenericAbsDomWrapper(pre));
		// --- invariants that hold at the exit of the blocks
		auto post = analyzer.get_post(cfg, get_crab_basic_block(B));
		update(results.postmap, *B, mkGenericAbsDomWrapper(post));

		#if 0
		if (params.stats) {
		  unsigned num_block_invars = 0;
		  // XXX: for boxes we should use
		  // to_disjunctive_linear_constraint_system() but it
		  // can be very expensive.
		  num_block_invars += pre.to_linear_constraint_system().size();
		  num_invars += num_block_invars;
		  if (num_block_invars > 0) num_nontrivial_blocks++;
		}
		#endif 
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
            std::vector<std::unique_ptr<pretty_printer_impl::block_annotation>>
                annotations;
            annotations.emplace_back(
                std::make_unique<pretty_printer_impl::invariant_annotation>(
                    m_crab_builder_man.get_var_factory(), results.premap,
                    results.postmap, params.keep_shadow_vars));
	      pretty_printer_impl::print_annotations(cfg, annotations);	    
	    }
	  }

	}
      }
      
      if (params.store_invariants || params.print_invars) {	
	CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "All invariants stored.\n");
      }

      return;
    }

    // Domains used for inter-procedural analysis
    struct inter_analysis {
      std::function<void(const AnalysisParams&, AnalysisResults&)> analyze;
      std::string name;
    };

    // Domains used for intra-procedural analysis
    const std::map<CrabDomain, inter_analysis> inter_analyses {
      { ZONES_SPLIT_DBM,
       {bind_this(this, &InterClam_Impl::analyzeCg<split_dbm_domain_t>),
        "zones"}},
      {INTERVALS,
       {bind_this(this, &InterClam_Impl::analyzeCg<interval_domain_t>),
        "intervals"}},
      {WRAPPED_INTERVALS,
       {bind_this(this, &InterClam_Impl::analyzeCg<wrapped_interval_domain_t>),
        "wrapped intervals"}},
      {OCT, {bind_this(this, &InterClam_Impl::analyzeCg<oct_domain_t>), "oct"}},
      {TERMS_ZONES,
       {bind_this(this, &InterClam_Impl::analyzeCg<num_domain_t>),
        "terms+zones"}},
      {TERMS_DIS_INTERVALS,
       {bind_this(this, &InterClam_Impl::analyzeCg<term_dis_int_domain_t>),
        "terms+dis_intervals"}},
      {BOXES,
       {bind_this(this, &InterClam_Impl::analyzeCg<boxes_domain_t>), "boxes"}},
      {PK, {bind_this(this, &InterClam_Impl::analyzeCg<pk_domain_t>), "pk"}}
    };
#endif     
  };

  /**
   *   Begin InterClam methods
   **/
  InterClam::InterClam(const Module &module, CrabBuilderManager &man)		        
    : m_impl(nullptr), m_builder_man(man) {
  m_impl = std::make_unique<InterClam_Impl>(module, m_builder_man);
  }

  InterClam::~InterClam() {}

  void InterClam::clear() {
    m_pre_map.clear();
    m_post_map.clear();
    m_checks_db.clear();
  }

CrabBuilderManager &InterClam::get_cfg_builder_man() { return m_builder_man; }
  
  void InterClam::analyze(AnalysisParams &params,
			  const abs_dom_map_t &assumptions) {
    AnalysisResults results = {m_pre_map, m_post_map, m_checks_db}; 
    lin_csts_map_t lin_csts_assumptions;
    m_impl->Analyze(params, assumptions, lin_csts_assumptions, results);
  }

  void InterClam::analyze(AnalysisParams &params,
			  const lin_csts_map_t &assumptions) {
    AnalysisResults results = {m_pre_map, m_post_map, m_checks_db}; 
    abs_dom_map_t abs_dom_assumptions;
    m_impl->Analyze(params, abs_dom_assumptions, assumptions, results);
  }

  llvm::Optional<clam_abstract_domain> InterClam::get_pre(const llvm::BasicBlock *block,
							  bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
    shadows = std::vector<varname_t>(
        m_builder_man.get_var_factory().get_shadow_vars().begin(),
        m_builder_man.get_var_factory().get_shadow_vars().end());
    return lookup(m_pre_map, *block, shadows);
  }   

  llvm::Optional<clam_abstract_domain> InterClam::get_post(const llvm::BasicBlock *block,
							   bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(
        m_builder_man.get_var_factory().get_shadow_vars().begin(),
        m_builder_man.get_var_factory().get_shadow_vars().end());
    return lookup(m_post_map, *block, shadows);
  }

  const checks_db_t& InterClam::get_checks_db() const { return m_checks_db;}
  
  /**
   * End InterClam methods
   **/
  
  /**
   * Begin ClamPass methods
   **/
ClamPass::ClamPass() : llvm::ModulePass(ID), m_cfg_builder_man(nullptr) {}
  
  void ClamPass::releaseMemory() {
    m_pre_map.clear(); 
    m_post_map.clear();
    m_checks_db.clear();
  }
  
  bool ClamPass::runOnModule(Module &M) {

    /// Translate the module to Crab CFGs
    CrabBuilderParams params(CrabTrackLev, CrabCFGSimplify, true,
			     CrabEnableUniqueScalars, 
			     CrabIncludeHavoc,
			     CrabEnableBignums, CrabPrintCFG);
    
    auto &tli = getAnalysis<TargetLibraryInfoWrapperPass>();

    /// Create the CFG builder manager
    if (!CrabMemShadows) {
      std::unique_ptr<HeapAbstraction> mem(new DummyHeapAbstraction());    
      // If CrabMemShadows is enabled then we don't run any heap
      // analysis.
      switch(CrabHeapAnalysis) {
      case heap_analysis_t::CI_SEA_DSA:
      case heap_analysis_t::CS_SEA_DSA: {
      CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                             << "Started sea-dsa analysis\n";);
	CallGraph& cg = getAnalysis<CallGraphWrapperPass>().getCallGraph();      
	const DataLayout& dl = M.getDataLayout();
      seadsa::AllocWrapInfo *allocWrapInfo =
          &getAnalysis<seadsa::AllocWrapInfo>();
      mem.reset(new LegacySeaDsaHeapAbstraction(
          M, cg, dl, tli, *allocWrapInfo,
					   (CrabHeapAnalysis == heap_analysis_t::CS_SEA_DSA),
					   CrabDsaDisambiguateUnknown,
					   CrabDsaDisambiguatePtrCast,
					   CrabDsaDisambiguateExternal));
	CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Finished sea-dsa analysis\n";);      
	break;
      }
      case heap_analysis_t::NONE:
      default:
	CLAM_WARNING("running clam without heap analysis");
      }
      m_cfg_builder_man.reset(new CrabBuilderManager(params, tli, std::move(mem)));
    } else {
      if (auto smp = getAnalysisIfAvailable<seadsa::ShadowMemPass>()) {
	m_cfg_builder_man.reset(
          new CrabBuilderManager(params, tli, smp->getShadowMem()));
      } else {
	std::unique_ptr<HeapAbstraction> mem(new DummyHeapAbstraction());	
	m_cfg_builder_man.reset(
          new CrabBuilderManager(params, tli, std::move(mem)));
	if (CrabMemShadows) 
	  CLAM_WARNING("getAnalysisIfAvailable<ShadowMemPass> returned null");
      }
    }
        
    /// Run the analysis 
						  
    m_params.dom = ClamDomain;
    m_params.run_backward = CrabBackward;
    m_params.run_inter = CrabInter;
    m_params.max_calling_contexts = CrabInterMaxSummaries;
    m_params.run_liveness = CrabLive;
    m_params.relational_threshold = CrabRelationalThreshold;
    m_params.widening_delay = CrabWideningDelay;
    m_params.narrowing_iters = CrabNarrowingIters;
    m_params.widening_jumpset = CrabWideningJumpSet;
    m_params.stats = CrabStats;
    m_params.print_invars = CrabPrintAns;
    m_params.print_unjustified_assumptions = CrabPrintUnjustifiedAssumptions;
    m_params.print_summaries = CrabPrintSumm;
    m_params.store_invariants = CrabStoreInvariants;
    m_params.keep_shadow_vars = CrabKeepShadows;
    m_params.check = CrabCheck;
    m_params.check_verbose = CrabCheckVerbose;
            
    unsigned num_analyzed_funcs = 0;
    for (auto &F : M) {
    if (!isTrackable(F))
      continue;
      num_analyzed_funcs++;
    }
    
  CRAB_VERBOSE_IF(1, crab::get_msg_stream() << "Started clam\n";
                  crab::get_msg_stream()
                  << "Total number of analyzed functions:" << num_analyzed_funcs
                  << "\n";);

    if (CrabInter){
      InterClam_Impl inter_crab(M, *m_cfg_builder_man);
      AnalysisResults results = {m_pre_map, m_post_map, m_checks_db};
      /* -- empty assumptions */      
      abs_dom_map_t abs_dom_assumptions;
      lin_csts_map_t lin_csts_assumptions;      
      inter_crab.Analyze(m_params, abs_dom_assumptions, lin_csts_assumptions, results);
    } else {
      unsigned fun_counter = 1;
      for (auto &F : M) {
	if (!CrabInter && isTrackable(F)) {
        CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                               << "###Function " << fun_counter << "/"
                               << num_analyzed_funcs << "###\n";);
	  ++fun_counter;
	  runOnFunction(F); 
	}
      }
    }

    if (CrabStats) {
      crab::CrabStats::PrintBrunch(crab::outs());
    }
    
    if (CrabCheck) {
      llvm::outs() << "\n************** ANALYSIS RESULTS ****************\n";
      print_checks(llvm::outs());
      llvm::outs() << "************** ANALYSIS RESULTS END*************\n";
		      
      if (CrabStats) {		     
        llvm::outs() << "\n************** BRUNCH STATS ********************\n";
        if (get_total_checks() == 0) {
	  llvm::outs() << "BRUNCH_STAT Result NOCHECKS\n";
        } else if (get_total_error_checks() > 0) {
 	  llvm::outs() << "BRUNCH_STAT Result FALSE\n";
        } else if (get_total_warning_checks() == 0) {
	  llvm::outs() << "BRUNCH_STAT Result TRUE\n";
        } else {
  	  llvm::outs() << "BRUNCH_STAT Result INCONCLUSIVE\n";
        }
	#if 0
        llvm::outs() << "BRUNCH_STAT NumOfBlocksWithInvariants "
 	 	     << num_nontrivial_blocks << "\n";
        llvm::outs() << "BRUNCH_STAT SizeOfInvariants "       
		     << num_invars << "\n";
	#endif 
        llvm::outs() << "************** BRUNCH STATS END *****************\n\n";
      }
    }
    
   return false;
  }

  bool ClamPass::runOnFunction(Function &F) {
    IntraClam_Impl intra_crab(F, *m_cfg_builder_man);
    AnalysisResults results = {m_pre_map, m_post_map, m_checks_db};
    /* -- empty assumptions */
    abs_dom_map_t abs_dom_assumptions;
    lin_csts_map_t lin_csts_assumptions;          
    intra_crab.Analyze(m_params, &F.getEntryBlock(), abs_dom_assumptions,
                       lin_csts_assumptions, results);
    return false;
  }
  
  void ClamPass::getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<TargetLibraryInfoWrapperPass>();

    bool runSeaDsa = (CrabHeapAnalysis == heap_analysis_t::CI_SEA_DSA ||
		      CrabHeapAnalysis == heap_analysis_t::CS_SEA_DSA);
    
    if (!CrabMemShadows && runSeaDsa) {
      AU.addRequired<seadsa::AllocWrapInfo>();
    }
    
    if (CrabMemShadows) {
      AU.addRequired<seadsa::ShadowMemPass>();
    }
    AU.addRequired<UnifyFunctionExitNodes>();
    AU.addRequired<clam::NameValues>();
    AU.addRequired<CallGraphWrapperPass>();
  } 
  
  /**
   * For clam clients
   **/

  bool ClamPass::has_cfg(llvm::Function &F) {
    return m_cfg_builder_man->has_cfg(F);
  }
  
  cfg_ref_t ClamPass::get_cfg(llvm::Function &F) {
    assert(m_cfg_builder_man->has_cfg(F));
    return m_cfg_builder_man->get_cfg(F);
  }

  CrabBuilderManager& ClamPass::get_cfg_builder_man() {
    return *m_cfg_builder_man;
  }
  
  // return invariants that hold at the entry of block
  llvm::Optional<clam_abstract_domain> ClamPass::get_pre(const llvm::BasicBlock *block,
							 bool keep_shadows) const {
    std::vector<varname_t> shadows;
    auto &vfac = m_cfg_builder_man->get_var_factory();
    if (!keep_shadows)
      shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
				       vfac.get_shadow_vars().end());
    return lookup(m_pre_map, *block, shadows);
  }   

  // return invariants that hold at the exit of block
  llvm::Optional<clam_abstract_domain> ClamPass::get_post(const llvm::BasicBlock *block,
							  bool keep_shadows) const {
    std::vector<varname_t> shadows;
    auto &vfac = m_cfg_builder_man->get_var_factory();    
    if (!keep_shadows)
      shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
				       vfac.get_shadow_vars().end());
    return lookup(m_post_map, *block, shadows);
  }

  /**
   * For assertion checking
   **/
  
  unsigned ClamPass::get_total_checks() const {
  return get_total_safe_checks() + get_total_error_checks() +
           get_total_warning_checks();
  }

  unsigned ClamPass::get_total_safe_checks() const {
    return m_checks_db.get_total_safe();
  }

  unsigned ClamPass::get_total_error_checks() const {
    return m_checks_db.get_total_error();
  }

  unsigned ClamPass::get_total_warning_checks() const {
    return m_checks_db.get_total_warning();
  }

  void ClamPass::print_checks(raw_ostream &o) const {
    unsigned safe = get_total_safe_checks();
    unsigned unsafe = get_total_error_checks();
    unsigned warning = get_total_warning_checks();
    std::vector<unsigned> cnts = { safe, unsafe, warning};
    unsigned MaxValLen = 0;
    for (auto c: cnts)
    MaxValLen = std::max(MaxValLen, (unsigned)std::to_string(c).size());
  o << std::string((int)MaxValLen - std::to_string(safe).size(), ' ') << safe
    << std::string(2, ' ') << "Number of total safe checks\n"
      << std::string((int) MaxValLen - std::to_string(unsafe).size(), ' ') 
      << unsafe << std::string(2, ' ') << "Number of total error checks\n"
      << std::string((int) MaxValLen - std::to_string(warning).size(), ' ') 
      << warning << std::string(2, ' ') << "Number of total warning checks\n";
  }

  char clam::ClamPass::ID = 0;
  
} // namespace clam

static RegisterPass<clam::ClamPass> X("clam", "Infer invariants using Crab",
                                      false, false);

