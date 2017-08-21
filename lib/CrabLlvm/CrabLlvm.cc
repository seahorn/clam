#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "crab_llvm/config.h"
#include "crab_llvm/crab_domains.hh"
#include "crab_llvm/wrapper_domain.hh"
#include "crab_llvm/CrabLlvm.hh"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/HeapAbstraction.hh"
#include "crab_llvm/Support/NameValues.hh"

#include "crab/common/debug.hpp"
#include "crab/common/stats.hpp"
#include "crab/analysis/fwd_analyzer.hpp"
#include "crab/analysis/bwd_analyzer.hpp"
#include "crab/analysis/inter_fwd_analyzer.hpp"
#include "crab/analysis/dataflow/liveness.hpp"
#include "crab/analysis/dataflow/assumptions.hpp"
#include "crab/checkers/assertion.hpp"
#include "crab/checkers/null.hpp"
#include "crab/checkers/checker.hpp"
#include "crab/cg/cg.hpp"
#include "crab/cg/cg_bgl.hpp"

#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>
#include <boost/range/iterator_range.hpp>

#include <memory>

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

// Compile time can be slow due to template instantiation. We enable
// by default this option to mitigate this problem by disabling some
// abstract domains. Comment it out for compiling all domains.
//#define FASTER_COMPILATION

using namespace llvm;
using namespace crab_llvm;
using namespace crab::cfg;

cl::opt<bool>
CrabPrintAns ("crab-print-invariants", 
              cl::desc ("Print Crab invariants"),
              cl::init (false));

cl::opt<bool>
CrabPrintSumm ("crab-print-summaries", 
               cl::desc ("Print Crab function summaries"),
               cl::init (false));

cl::opt<bool>
CrabPrintPreCond ("crab-print-preconditions", 
               cl::desc ("Print Crab necessary preconditions"),
               cl::init (false));

cl::opt<bool>
CrabStats ("crab-stats", 
           cl::desc ("Show Crab statistics and analysis results"),
           cl::init (false));

cl::opt<bool>
CrabPrintAssumptions ("crab-print-unjustified-assumptions", 
	cl::desc ("Print unjustified assumptions done by Crab (for now only integer overflow)"),
	cl::init (false));

cl::opt<unsigned int>
CrabWideningDelay("crab-widening-delay", 
   cl::desc("Max number of fixpoint iterations until widening is applied"),
   cl::init (1));

cl::opt<unsigned int>
CrabNarrowingIters("crab-narrowing-iterations", 
                   cl::desc("Max number of narrowing iterations"),
                   cl::init (10));

cl::opt<unsigned int>
CrabWideningJumpSet("crab-widening-jump-set", 
                    cl::desc("Size of the jump set used for widening"),
                    cl::init (0));

cl::opt<CrabDomain>
CrabLlvmDomain("crab-dom",
      cl::desc ("Crab numerical abstract domain used to infer invariants"),
      cl::values 
      (clEnumValN (INTERVALS, "int",
		   "Classical interval domain (default)"),
       clEnumValN (TERMS_INTERVALS, "term-int",
		   "Intervals with uninterpreted functions."),       
       clEnumValN (INTERVALS_CONGRUENCES, "ric",
		   "Reduced product of intervals with congruences"),
       clEnumValN (DIS_INTERVALS, "dis-int",
		   "Disjunctive intervals based on Clousot's DisInt domain"),
       clEnumValN (TERMS_DIS_INTERVALS, "term-dis-int",
		   "Disjunctive Intervals with uninterpreted functions."),
       clEnumValN (BOXES, "boxes",
		   "Disjunctive intervals based on ldds"),
       clEnumValN (ZONES_SPLIT_DBM, "zones",
		   "Zones domain with Sparse DBMs in Split Normal Form"),
       clEnumValN (OPT_OCT_APRON, "oct",
		   "Optimized octagons domain using Elina"),
       clEnumValN (PK_APRON, "pk",
		   "Polyhedra domain using Apron library"),
       clEnumValN (TERMS_ZONES, "rtz",
		   "Reduced product of term-dis-int and zones."),
       clEnumValEnd),
       cl::init (INTERVALS));

cl::opt<bool>
CrabBackward ("crab-backward", 
           cl::desc ("Perform an iterative forward/backward analysis\n"
		     "(Only intra-procedural version implemented)"),
           cl::init (false));

// If domain is num
cl::opt<unsigned>
CrabRelationalThreshold("crab-relational-threshold", 
   cl::desc("Max number of live vars per block before switching "
	    "to a non-relational domain"),
   cl::init (10000),
   cl::Hidden);

cl::opt<bool>
CrabLive("crab-live", 
	 cl::desc("Run Crab with live ranges. "
		  "It can lose precision if relational domains"),
	 cl::init (false));

cl::opt<bool>
CrabInter ("crab-inter",
           cl::desc ("Crab Inter-procedural analysis"), 
           cl::init (false));

// It does not make much sense to have non-relational domains here.
cl::opt<CrabDomain>
CrabSummDomain("crab-inter-sum-dom",
    cl::desc ("Crab relational domain to generate function summaries"),
    cl::values 
    (clEnumValN (ZONES_SPLIT_DBM, "zones",
		 "Zones domain with sparse DBMs in Split Normal Form"),
     clEnumValN (OPT_OCT_APRON, "oct",
		 "Optimized octagons using Elina"),
     clEnumValN (TERMS_ZONES, "rtz",
		 "Reduced product of term-dis-int and zones."),
     clEnumValEnd),
    cl::init (ZONES_SPLIT_DBM));

cl::opt<enum tracked_precision>
CrabTrackLev("crab-track",
   cl::desc ("Track abstraction level of the Crab Cfg"),
   cl::values
    (clEnumValN (NUM, "num", "Integer and Boolean registers only"),
     clEnumValN (PTR, "ptr", "num + pointer offsets"),
     clEnumValN (ARR, "arr", "ptr + memory contents via array abstraction"),
     clEnumValEnd),
   cl::init (tracked_precision::NUM));


cl::opt<assert_check_kind_t>
CrabCheck ("crab-check", 
	   cl::desc ("Check user assertions"),
	   cl::values(
	       clEnumValN (NOCHECKS  , "none"  , "None"),
	       clEnumValN (ASSERTION , "assert", "User assertions"),
	       clEnumValN (NULLITY   , "null"  , "Null dereference"),
	       clEnumValEnd),
	   cl::init (assert_check_kind_t::NOCHECKS));

cl::opt<unsigned int>
CrabCheckVerbose ("crab-check-verbose", 
                 cl::desc ("Print verbose information about checks"),
                 cl::init (0));

// Important to crab-llvm clients (e.g., SeaHorn):
// Shadow variables are variables that cannot be mapped back to a
// const Value*. These are created for instance for memory heaps.
cl::opt<bool>
CrabKeepShadows ("crab-keep-shadows",
    cl::desc ("Preserve shadow variables in invariants, summaries, and preconditions"), 
    cl::init (false),
    cl::Hidden);


namespace crab_llvm {

  // TODO: refactoring inter-procedural analysis. Decouple the
  // inter-procedural analysis from the llvm pass.
  
  using namespace crab::analyzer;
  using namespace crab::checker;
  using namespace crab::cg;

  /** Begin typedefs **/
  typedef crab::analyzer::liveness<cfg_ref_t> liveness_t;
  typedef crab::cg::call_graph<cfg_ref_t> call_graph_t; 
  typedef crab::cg::call_graph_ref<call_graph_t> call_graph_ref_t;
  typedef boost::unordered_map<cfg_ref_t, const liveness_t*> liveness_map_t;
  typedef DenseMap<const BasicBlock*, z_lin_cst_sys_t> assumption_map_t;
  typedef typename IntraCrabLlvm::wrapper_dom_ptr wrapper_dom_ptr;    
  typedef typename IntraCrabLlvm::checks_db_t checks_db_t;
  typedef typename IntraCrabLlvm::checks_db_ptr checks_db_ptr;
  typedef typename IntraCrabLlvm::invariant_map_t invariant_map_t;
  typedef typename IntraCrabLlvm::heap_abs_ptr heap_abs_ptr;
  typedef DenseMap<llvm::Function*, cfg_ptr_t> cfg_map_t;    
  /** End typedefs **/
  
  /** Begin global counters **/
  static unsigned num_invars; // some measure for the size of invariants
  static unsigned num_nontrivial_blocks;
  /** End global counters **/
  
  static bool isRelationalDomain(CrabDomain dom) {
    return (dom == ZONES_SPLIT_DBM || dom == OPT_OCT_APRON ||
	    dom == PK_APRON        || dom == TERMS_ZONES);
  }

  static bool isTrackable(const Function &fun) {
    return !fun.isDeclaration () && !fun.empty () && !fun.isVarArg ();
  }
  
  /** return invariant for block in table but filtering out shadow_vars **/
  static wrapper_dom_ptr lookup(const invariant_map_t &table,
				const llvm::BasicBlock &block,
				const std::vector<varname_t> &shadow_vars) {
    auto it = table.find (&block);
    assert (it != table.end ());
    if (shadow_vars.empty()) {
      return it->second;
    } else {
      auto invs = it->second->clone();
      invs->forget(shadow_vars); 
      return invs;
    }
  }   

  static bool update(invariant_map_t &table, 
		     const llvm::BasicBlock &block, wrapper_dom_ptr absval) {
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
  
  /** print invariants that hold at the entry and exit of block **/
  static void printInvariants(const BasicBlock &block,
			      const llvm_variable_factory &vfac,
			      const invariant_map_t &premap,
			      const invariant_map_t &postmap,
			      const bool keep_shadows,
			      raw_ostream &o) {
    o << block.getName() << ": ";
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(vfac.get_shadow_vars().begin(),
				       vfac.get_shadow_vars().end());    
    auto pre = lookup(premap, block, shadows);
    auto post = lookup(postmap, block, shadows);
    o << pre << " ==> " << post << "\n";
  }
  
  struct isLlvmBlock: std::unary_function<const BasicBlock&, bool> {
    boost::unordered_set<const BasicBlock*> m_llvm_blocks;
    isLlvmBlock(const Function &fun) {
      for (auto &b: boost::make_iterator_range(fun.begin(), fun.end()))
	m_llvm_blocks.insert(&b);
    }
    bool operator()(const BasicBlock *b) const {
      return m_llvm_blocks.find(b) != m_llvm_blocks.end();
    }
  };

  struct AnalysisResults {
    invariant_map_t &premap;
    invariant_map_t &postmap;
    checks_db_ptr &checksdb;

    AnalysisResults(invariant_map_t &pre, invariant_map_t &post, checks_db_ptr &db)
      : premap(pre), postmap(post), checksdb(db) {}
  };

  /**
   * Internal implementation of the intra-procedural analysis
   **/
  class IntraCrabLlvm_Impl {
    
    cfg_ptr_t m_cfg;
    Function &m_fun;
    llvm_variable_factory &m_vfac;
    
    template<typename Dom>
    void analyzeCfg(const AnalysisParams &params,
		    const assumption_map_t &assumptions, liveness_t *live,
		    AnalysisResults &results) {
      
      // -- we use the combined forward/backward analyzer
      typedef intra_forward_backward_analyzer<cfg_ref_t,Dom> intra_analyzer_t;
      // -- checkers for assertions and nullity
      typedef intra_checker<intra_analyzer_t> intra_checker_t;
      typedef assert_property_checker<intra_analyzer_t> assert_prop_t;
      typedef null_property_checker<intra_analyzer_t> null_prop_t;
      
      CRAB_LOG("crabllvm",
	       auto fdecl = m_cfg->get_func_decl ();            
	       assert (fdecl);
	       crab::outs() << "Running intra-procedural analysis with " 
	                    << "\"" << Dom::getDomainName ()  << "\""
	                    << " for "  << (*fdecl).get_func_name ()
	                    << "  ... ";);
      
      // -- run intra-procedural analysis
      intra_analyzer_t analyzer (*m_cfg);
      typename intra_analyzer_t::assumption_map_t crab_assumptions;
      typedef typename intra_analyzer_t::assumption_map_t::value_type binding_t;
      // reconstruct a crab assumption map from our assumption DenseMap
      for (auto &kv: assumptions) {
	Dom absval = Dom::top();
	absval += kv.second;
	crab_assumptions.insert(binding_t(kv.first, absval));
      }
      // TODO: This analysis ignores live
      analyzer.run(Dom::top(), Dom::top(), !params.run_backward, crab_assumptions,
		   params.widening_delay, params.narrowing_iters, params.widening_jumpset);
      CRAB_LOG("crabllvm", crab::outs() << "DONE\n"); 

      isLlvmBlock is_llvm_block(m_fun);
      // -- store invariants 
      for (const BasicBlock* B: boost::make_iterator_range(m_cfg->label_begin(),
      							   m_cfg->label_end())) {
      	if (!is_llvm_block(B)) continue;

	// --- invariants that hold at the entry of the blocks
	auto pre = analyzer.get_pre (B);
	update(results.premap, *B,  mkGenericAbsDomWrapper(pre));
	// --- invariants that hold at the exit of the blocks
	auto post = analyzer.get_post (B);
	update(results.postmap, *B,  mkGenericAbsDomWrapper(post));	
	if (params.stats) {
	  unsigned num_block_invars = 0;
	  // TODO CRAB: for boxes we would like to use
	  // to_disjunctive_linear_constraint_system() but it needs to
	  // be exposed to all domains
	  num_block_invars += pre.to_linear_constraint_system().size();
	  num_invars += num_block_invars;
	  if (num_block_invars > 0) num_nontrivial_blocks++;
	}
      }
      
      if (params.print_invars) {
	// -- print invariants
	llvm::outs() << "\nInvariants for " << m_fun.getName() << "\n";
	for (const BasicBlock* B: boost::make_iterator_range(m_cfg->label_begin(),
							     m_cfg->label_end())) {
	  if (!is_llvm_block(B)) continue;
	  
	  llvm::outs() << "\t";
	  printInvariants(*B, m_vfac, results.premap, results.postmap,
			   params.keep_shadow_vars, llvm::outs());
	}
      }
    
      if (params.print_preconds && params.run_backward) {
	// --- print preconditions
	llvm::outs() << "\nNecessary preconditions for " << m_fun.getName() << "\n";
	for (const BasicBlock* B : boost::make_iterator_range(m_cfg->label_begin(),
							      m_cfg->label_end())) {
	  if (!is_llvm_block(B)) continue;

	  auto pre = analyzer.get_preconditions(B);
	  llvm::outs() << "\t" << B->getName () << ": ";
	  crab::outs() << pre << "\n";
	}
	llvm::outs() <<  "\n";
      }
      
      if (params.check) {
	// --- checking assertions and collecting data
	CRAB_LOG("crabllvm", crab::outs() << "Checking assertions ... \n"); 
	typename intra_checker_t::prop_checker_ptr
	  prop (new assert_prop_t (params.check_verbose));
	if (params.check == NULLITY)
	  prop.reset (new null_prop_t(params.check_verbose));
	intra_checker_t checker (analyzer, {prop});
	checker.run ();
	CRAB_LOG("crabllvm",
		 llvm::outs() << "Function " << m_fun.getName() << "\n";
		 checker.show (crab::outs()));
	results.checksdb = boost::make_shared<checks_db_t>();
	(*results.checksdb) += checker.get_all_checks();
	CRAB_LOG("crabllvm", crab::outs() << "DONE!\n");      
      }

      if (params.print_assumptions) {
	// Print all the unjustified assumptions done by the analyzer
	// while proving assertions.
	// XXX: currently only integer overflows
	
	//assumption_naive_analysis<cfg_ref_t> assumption_analyzer(*m_cfg);
	assumption_dataflow_analysis<cfg_ref_t> assumption_analyzer(*m_cfg);
	assumption_analyzer.exec();
	crab::outs() << "\n" << assumption_analyzer;
      }
      
      return;
    }

    void wrapperAnalyze(const AnalysisParams &params,
			const assumption_map_t &assumptions, liveness_t *live,
			AnalysisResults &results) {
      
      switch (params.dom) {
      #ifndef FASTER_COMPILATION
      case INTERVALS_CONGRUENCES:
	analyzeCfg<ric_domain_t>(params, assumptions, live, results);
        break;
      case DIS_INTERVALS:
	analyzeCfg<dis_interval_domain_t>(params, assumptions, live, results);
        break;
      case TERMS_INTERVALS:
	analyzeCfg<term_int_domain_t>(params, assumptions, live, results);
        break;
      #endif 
      case TERMS_DIS_INTERVALS:
	analyzeCfg<term_dis_int_domain_t>(params, assumptions, live, results);
        break;
      case ZONES_SPLIT_DBM:
	analyzeCfg<split_dbm_domain_t>(params, assumptions, live, results);
        break;
      case BOXES:
	analyzeCfg<boxes_domain_t>(params, assumptions, live, results);
        break;
      case OPT_OCT_APRON:
	analyzeCfg<opt_oct_apron_domain_t>(params, assumptions, live, results);
        break;
      case PK_APRON:
	analyzeCfg<pk_apron_domain_t>(params, assumptions, live, results);
        break;
      case TERMS_ZONES:
	analyzeCfg<num_domain_t>(params, assumptions, live, results);
        break;
      default: 
        if (params.dom != INTERVALS) {
          crab::outs() << "Warning: abstract domain not found.\n"
		       << "If FASTER_COMPILATION is enabled then "
		       << "some domains are not available.\n"
                       << "Running intervals ...\n"; 
        }
	analyzeCfg<interval_domain_t>(params, assumptions, live, results);
      }
    }

  public:
    
    IntraCrabLlvm_Impl(Function &fun,
		       crab::cfg::tracked_precision cfg_precision,
		       heap_abs_ptr mem, llvm_variable_factory &vfac,
		       const TargetLibraryInfo &tli)
      : m_cfg(nullptr), m_fun(fun), m_vfac(vfac) {
      if (isTrackable(m_fun)) {
	// -- build a crab cfg for func
	CfgBuilder builder(m_fun, m_vfac, *mem, cfg_precision, true, &tli);
	  CRAB_LOG("crabllvm",
		   llvm::outs () << "Built Crab CFG for " << fun.getName() << "\n");
	  m_cfg = builder.getCfg();
      } else {
	  CRAB_LOG("crabllvm",
		   llvm::outs () << "Cannot build CFG for " << fun.getName() << "\n");
      }
    }

    cfg_ptr_t Cfg () { return m_cfg; }

    void Analyze(AnalysisParams &params, const assumption_map_t &assumptions,
		 AnalysisResults &results) {

      if (!m_cfg) {
	CRAB_LOG("crabllvm",
		 llvm::outs () << "Skipped analysis for " << m_fun.getName() << "\n");
	return;
      }
      
      // -- run liveness
      liveness_t* live = nullptr;
      if (params.run_liveness || isRelationalDomain(params.dom)) {
	CRAB_LOG("crabllvm",
		 auto fdecl = m_cfg->get_func_decl ();            
		 assert (fdecl);
		 crab::outs() << "Running liveness analysis for " 
		              << (*fdecl).get_func_name ()
		              << "  ... ";);
	liveness_t ls (*m_cfg);
	ls.exec ();
	CRAB_LOG("crabllvm", crab::outs() << "DONE!\n");
	// some stats
	unsigned total_live, avg_live_per_blk, max_live_per_blk;
	ls.get_stats (total_live, max_live_per_blk, avg_live_per_blk);
	CRAB_LOG("crabllvm", 
		 crab::outs() << "-- Max number of out live vars per block=" 
                              << max_live_per_blk << "\n"
                              << "-- Avg number of out live vars per block=" 
                              << avg_live_per_blk << "\n";);
	crab::CrabStats::count_max ("Liveness.count.maxOutVars",
				    max_live_per_blk);

	if (isRelationalDomain(params.dom)) {
	  CRAB_LOG("crabllvm", 
		   crab::outs() << "Max live per block: "
		                << max_live_per_blk << "\n"
		                << "Threshold: "
		                << params.relational_threshold << "\n");
	  if (max_live_per_blk > params.relational_threshold) {
          #ifdef FASTER_COMPILATION
	  params.dom = INTERVALS;
	  #else
	  params.dom = DIS_INTERVALS;
          #endif 
	  }
	}
	if (params.run_liveness) live = &ls;
      }
      wrapperAnalyze(params, assumptions, live, results);
    }
  }; // end class

  /**
   *   Begin IntraCrabLlvm methods
   **/
  IntraCrabLlvm::IntraCrabLlvm(Function &fun, const TargetLibraryInfo &tli,
			       crab::cfg::tracked_precision cfg_precision,
			       heap_abs_ptr heap_abs) : m_impl(nullptr) {
    if (!heap_abs)
      heap_abs = boost::make_shared<DummyHeapAbstraction>();
    
    m_impl = make_unique<IntraCrabLlvm_Impl>(fun, cfg_precision,
					     heap_abs, m_vfac, tli);
  }

  IntraCrabLlvm::~IntraCrabLlvm() {}
    
  void IntraCrabLlvm::analyze(AnalysisParams &params,
			      const assumption_map_t &assumptions) {
    checks_db_ptr checksdb = nullptr;
    AnalysisResults results = { m_pre_map, m_post_map, checksdb};
    m_impl->Analyze(params, assumptions, results);
  }

  wrapper_dom_ptr IntraCrabLlvm::get_pre(const llvm::BasicBlock *block,
					 bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(m_vfac.get_shadow_vars().begin(),
				       m_vfac.get_shadow_vars().end());    
    return lookup(m_pre_map, *block, shadows);
  }   

  wrapper_dom_ptr IntraCrabLlvm::get_post(const llvm::BasicBlock *block,
					  bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(m_vfac.get_shadow_vars().begin(),
				       m_vfac.get_shadow_vars().end());    
    return lookup(m_post_map, *block, shadows);
  }
  /**
   *   End IntraCrabLlvm methods
   **/
    
  /** Run inter-procedural analysis on the whole call graph **/
  template<typename BUDom, typename TDDom>
  static void analyze_cg (call_graph_ref_t cg, const Module &M,
			  const liveness_map_t& live,
			  invariant_map_t &premap, invariant_map_t &postmap,
			  const llvm_variable_factory &vfac,
			  checks_db_ptr &checkdb) {
    
    typedef inter_fwd_analyzer<call_graph_ref_t, BUDom, TDDom> inter_analyzer_t;
    typedef inter_checker<inter_analyzer_t> inter_checker_t;
    typedef assert_property_checker<inter_analyzer_t> assert_prop_t;
    typedef null_property_checker<inter_analyzer_t> null_prop_t;
    
    CRAB_LOG("crabllvm", 
	     crab::outs() << "Running inter-procedural analysis with " 
	                  << "forward domain:" 
	                  << "\"" << TDDom::getDomainName () << "\""
	                  << " and bottom-up domain:" 
                          << "\"" << BUDom::getDomainName () << "\"" 
                          << "  ... ";);
    
    inter_analyzer_t analyzer(cg, (CrabLive ? &live : nullptr),
			      CrabWideningDelay, 
			      CrabNarrowingIters, 
			      CrabWideningJumpSet);
    analyzer.Run (TDDom::top ());
    
    CRAB_LOG("crabllvm", crab::outs() << "DONE\n");
    
    // -- store invariants     
    for (auto &n: boost::make_iterator_range (vertices (cg))) {
      cfg_ref_t cfg = n.get_cfg ();
      boost::optional<const Value *> v = n.name ().get ();
      if (v) {
	if (const Function *F = dyn_cast<Function> (*v)) {
	  for (auto &B : *F) {
	    // --- invariants that hold at the entry of the blocks
	    auto pre = analyzer.get_pre (cfg, &B);
	    premap.insert(std::make_pair(&B, mkGenericAbsDomWrapper(pre)));
	    // --- invariants that hold at the exit of the blocks
	    auto post = analyzer.get_post (cfg, &B);
	    postmap.insert(std::make_pair(&B,mkGenericAbsDomWrapper(post)));
	    
	    if (CrabStats) {
	      unsigned num_block_invars = 0;
	      // TODO CRAB: for boxes we would like to use
	      // to_disjunctive_linear_constraint_system() but it needs to
	      // be exposed to all domains
	      num_block_invars += pre.to_linear_constraint_system().size();
	      num_invars += num_block_invars;
	      if (num_block_invars > 0) num_nontrivial_blocks++;
	    }
	  }
	  
	  // --- print invariants and summaries
	  if (CrabPrintAns && isTrackable(*F)) {
	    llvm::outs() << "\nInvariants for " << F->getName () << "\n";
	    for (auto &B : *F) {
	      llvm::outs() << "\t";
	      printInvariants(B, vfac, premap, postmap,
			      CrabKeepShadows, llvm::outs());
	    }
	  }

	  // Summaries are not currently stored but it would be easy to do so.	    
	  if (CrabPrintSumm && analyzer.has_summary (cfg)) {
	      auto summ = analyzer.get_summary (cfg);
	      crab::outs() << "SUMMARY " << *summ << "\n";
	  }
	}
      }
    }
    
    // --- checking assertions and collecting data
    if (CrabCheck) {
      CRAB_LOG("crabllvm", crab::outs() << "Checking assertions ... \n"); 
      typename inter_checker_t::prop_checker_ptr
	prop(new assert_prop_t(CrabCheckVerbose));
      if (CrabCheck == NULLITY)
	prop.reset (new null_prop_t(CrabCheckVerbose));      
      inter_checker_t checker (analyzer, {prop});
      checker.run ();
      CRAB_LOG("crabllvm", checker.show (crab::outs()));
      
      checkdb = boost::make_shared<checks_db_t>();
      (*checkdb) += checker.get_all_checks();
      CRAB_LOG("crabllvm", crab::outs() << "DONE!\n"); 
    }
    return;
  }
  
  #ifdef FASTER_COMPILATION
  #define INTER_ANALYZE(DOM,CG,M,LIVE,PRE,POST,VFAC,CHECKS)		       \
   switch (CrabSummDomain){                                                    \
     default:                                                                  \
       if (CrabSummDomain != ZONES_SPLIT_DBM)                                  \
         crab::outs() << "Warning: choosing zones to compute summaries\n";     \
       analyze_cg<split_dbm_domain_t,DOM> (CG, M, LIVE, PRE, POST, VFAC, CHECKS);}
  #else
  #define INTER_ANALYZE(DOM,CG,M,LIVE,PRE,POST,VFAC,CHECKS)		       \
  switch (CrabSummDomain){                                                     \
    case OPT_OCT_APRON:                                                        \
      analyze_cg<opt_oct_apron_domain_t,DOM>(CG, M, LIVE, PRE, POST, VFAC, CHECKS); \
      break;                                                                   \
    case TERMS_ZONES:                                                          \
      analyze_cg<num_domain_t,DOM>(CG, M, LIVE, PRE, POST, VFAC, CHECKS);      \
      break;						                       \
    default:                                                                   \
      analyze_cg<split_dbm_domain_t,DOM>(CG, M, LIVE, PRE, POST, VFAC, CHECKS);}
  #endif 

  /**
   * Begin CrabLlvmPass methods
   **/
  CrabLlvmPass::CrabLlvmPass ()
    : llvm::ModulePass (ID), 
      m_mem (boost::make_shared<DummyHeapAbstraction>()),
      m_checks_db (nullptr) { }

  void CrabLlvmPass::releaseMemory () {
    m_pre_map.clear(); 
    m_post_map.clear(); 
    m_cfg_map.clear ();
  }
  
  bool CrabLlvmPass::runOnModule (Module &M) {
    #ifdef HAVE_DSA
    m_mem.reset
      (new LlvmDsaHeapAbstraction(M,&getAnalysis<SteensgaardDataStructures>()));
    #endif     

    CRAB_LOG("crabllvm",
             unsigned num_analyzed_funcs = 0;
             for (auto &F : M) {
	       if (!isTrackable(F)) continue;
               num_analyzed_funcs++;
             }
             crab::outs() << "Total number of analyzed functions:" 
                          << num_analyzed_funcs << "\n";);

    if (CrabInter){

      CrabDomain absdom =  CrabLlvmDomain;
      std::vector<cfg_ref_t> cfgs;
      liveness_map_t live_map;
      unsigned max_live_per_blk = 0;
      for (auto &F : M) {
        // -- skip functions without a body or variadic functions
        if (!isTrackable(F)) continue;

        // -- build cfg
        CfgBuilder B (F, m_vfac, *m_mem, CrabTrackLev,
                      /*include function decls and callsites*/
                      true,  &getAnalysis<TargetLibraryInfo>());

        auto cfg_ptr = B.getCfg();
        m_cfg_map [&F] = cfg_ptr;
        cfgs.push_back (*cfg_ptr);

        // -- build liveness
        if (CrabLive || isRelationalDomain(absdom)) {
          CRAB_LOG("crabllvm",
                   auto fdecl = cfg_ptr->get_func_decl ();            
                   assert (fdecl);
                   crab::outs() << "Running liveness analysis for " 
                                << (*fdecl).get_func_name () << "  ... ";);

	  liveness_t* live = new liveness_t (*cfg_ptr);
          live->exec ();
          CRAB_LOG("crabllvm", crab::outs() << "DONE!\n";);
          // some stats
          unsigned total_live, max_live_per_blk_, avg_live_per_blk;
          live->get_stats (total_live, max_live_per_blk_, avg_live_per_blk);
          max_live_per_blk = std::max (max_live_per_blk, max_live_per_blk_);
          CRAB_LOG("crabllvm",
                   crab::outs() << "-- Max number of out live vars per block=" 
                                << max_live_per_blk_ << "\n";
                   crab::outs() << "-- Avg number of out live vars per block=" 
                                << avg_live_per_blk << "\n";);
          crab::CrabStats::count_max ("Liveness.count.maxOutVars",
				      max_live_per_blk);


	  if (isRelationalDomain(absdom)) {
	    // FIXME: the selection of the final domain is fixed for the
	    //        whole program. That is, if there is one function that
	    //        exceeds the threshold then the cheaper domain will be
	    //        used for all functions. We should be able to change
	    //        from one function to another.
	    CRAB_LOG("crabllvm",
		     crab::outs() << "Max live per block: "
		                  << max_live_per_blk << "\n"
		                  << "Threshold: "
		                  << CrabRelationalThreshold << "\n");
	    if (max_live_per_blk > CrabRelationalThreshold) {
              #ifdef FASTER_COMPILATION
	      absdom = INTERVALS;
	      #else
	      absdom = DIS_INTERVALS;
	      #endif 
	    }
	  }
	  
	  if (CrabLive) {
	    live_map.insert(std::make_pair(cfg_ref_t(*cfg_ptr), live));
	  }
        }
      }

      // -- build call graph
      boost::scoped_ptr<call_graph_t> cg(new call_graph_t(cfgs));
      // -- run the interprocedural analysis            
      switch (absdom) {
        #ifndef FASTER_COMPILATION
        case INTERVALS_CONGRUENCES: 
          INTER_ANALYZE(ric_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        case TERMS_INTERVALS:
          INTER_ANALYZE(term_int_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        case DIS_INTERVALS:
          INTER_ANALYZE(dis_interval_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        #endif 
        case TERMS_DIS_INTERVALS:
          INTER_ANALYZE(term_dis_int_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        case ZONES_SPLIT_DBM: 
          INTER_ANALYZE(split_dbm_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        case OPT_OCT_APRON:
          INTER_ANALYZE(opt_oct_apron_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        case PK_APRON:
          INTER_ANALYZE(pk_apron_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        case TERMS_ZONES: 
          INTER_ANALYZE(num_domain_t, *cg, M, live_map,
			m_pre_map, m_post_map, m_vfac, m_checks_db); 
          break;
        default: 
          if (absdom != INTERVALS)
            crab::outs() << "Warning: either abstract domain not found or "
                         << "inter-procedural version not implemented.\n"
                         << "If FASTER_COMPILATION is enabled then "
	                 << "some domains are not available.\n"
                         << "Running intervals ...\n"; 
          INTER_ANALYZE (interval_domain_t, *cg, M, live_map,
			 m_pre_map, m_post_map, m_vfac, m_checks_db);
      }
      
      // free liveness map
      if (CrabLive)
        for (auto &p : live_map)
          delete p.second;
    }
    else {
      // -- run intra-procedural analysis
      for (auto &f : M) {
        runOnFunction (f); 
      }
    }

    if (CrabStats) {
      crab::CrabStats::PrintBrunch (crab::outs());
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
        llvm::outs() << "BRUNCH_STAT NumOfBlocksWithInvariants "
 	 	     << num_nontrivial_blocks << "\n";
        llvm::outs() << "BRUNCH_STAT SizeOfInvariants "       
		     << num_invars << "\n";
        llvm::outs() << "************** BRUNCH STATS END *****************\n\n";
      }
    }
   return false;
  }


  bool CrabLlvmPass::runOnFunction (Function &F) {
    if (!CrabInter && isTrackable(F)) {
      const TargetLibraryInfo &tli = getAnalysis<TargetLibraryInfo>();
      IntraCrabLlvm_Impl crab(F, CrabTrackLev, m_mem, m_vfac, tli);
      m_cfg_map[&F] = crab.Cfg();
      AnalysisParams params;
      params.dom = CrabLlvmDomain;
      params.run_backward = CrabBackward;
      params.run_liveness = CrabLive;
      params.relational_threshold = CrabRelationalThreshold;
      params.widening_delay = CrabWideningDelay;
      params.narrowing_iters = CrabNarrowingIters;
      params.widening_jumpset = CrabWideningJumpSet;
      params.stats = CrabStats;
      params.print_invars = CrabPrintAns;
      params.print_preconds = CrabPrintPreCond;
      params.print_assumptions = CrabPrintAssumptions;
      params.keep_shadow_vars = CrabKeepShadows;
      params.check = CrabCheck;
      params.check_verbose = CrabCheckVerbose;
      AnalysisResults results = { m_pre_map, m_post_map, m_checks_db};
      crab.Analyze(params, assumption_map_t(), results);
    }
    
    return false;
  }

  void CrabLlvmPass::getAnalysisUsage (AnalysisUsage &AU) const {
    AU.setPreservesAll ();
    #ifdef HAVE_DSA
    AU.addRequiredTransitive<SteensgaardDataStructures> ();
    #endif 
    AU.addRequired<DataLayoutPass>();
    AU.addRequired<TargetLibraryInfo>();
    AU.addRequired<UnifyFunctionExitNodes>();
    AU.addRequired<crab_llvm::NameValues>();
  } 
  
  /**
   * For crab-llvm clients
   **/
  
  cfg_ptr_t CrabLlvmPass::get_cfg(llvm::Function* F) {
    auto it = m_cfg_map.find (F);
    if (it != m_cfg_map.end ())
      return it->second;
    return nullptr;
  }
  
  // return invariants that hold at the entry of block
  wrapper_dom_ptr
  CrabLlvmPass::get_pre(const llvm::BasicBlock *block, bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(m_vfac.get_shadow_vars().begin(),
				       m_vfac.get_shadow_vars().end());    
    return lookup(m_pre_map, *block, shadows);
  }   

  // return invariants that hold at the exit of block
  wrapper_dom_ptr
  CrabLlvmPass::get_post(const llvm::BasicBlock *block, bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(m_vfac.get_shadow_vars().begin(),
				       m_vfac.get_shadow_vars().end());    
    return lookup(m_post_map, *block, shadows);
  }

  /**
   * For assertion checking
   **/
  
  unsigned CrabLlvmPass::get_total_checks() const {
    return get_total_safe_checks() +  
           get_total_error_checks() + 
           get_total_warning_checks();
  }

  unsigned CrabLlvmPass::get_total_safe_checks () const {
    return (m_checks_db ? m_checks_db->get_total_safe() : 0);
  }

  unsigned CrabLlvmPass::get_total_error_checks () const {
    return (m_checks_db ? m_checks_db->get_total_error() : 0);
  }

  unsigned CrabLlvmPass::get_total_warning_checks () const {
    return (m_checks_db ? m_checks_db->get_total_warning() : 0);
  }

  void CrabLlvmPass::print_checks (raw_ostream &o) const {
    unsigned safe = get_total_safe_checks();
    unsigned unsafe = get_total_error_checks ();
    unsigned warning = get_total_warning_checks ();
    std::vector<unsigned> cnts = { safe, unsafe, warning};
    unsigned MaxValLen = 0;
    for (auto c: cnts)
      MaxValLen = std::max(MaxValLen,
			   (unsigned)std::to_string(c).size());
    o << std::string((int) MaxValLen - std::to_string(safe).size(), ' ') 
      << safe << std::string (2, ' ') << "Number of total safe checks\n"
      << std::string((int) MaxValLen - std::to_string(unsafe).size(), ' ') 
      << unsafe << std::string (2, ' ') << "Number of total error checks\n"
      << std::string((int) MaxValLen - std::to_string(warning).size(), ' ') 
      << warning << std::string(2, ' ') << "Number of total warning checks\n";
  }

  char crab_llvm::CrabLlvmPass::ID = 0;
  
} // end namespace 

static RegisterPass<crab_llvm::CrabLlvmPass> 
X ("crab-llvm", "Infer invariants using Crab", false, false);
  
   


