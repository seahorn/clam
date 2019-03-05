#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CallGraph.h"
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
#include "crab_llvm/Support/NameValues.hh"
/** Wrappers for pointer analyses **/
#include "crab_llvm/DummyHeapAbstraction.hh"
#include "crab_llvm/LlvmDsaHeapAbstraction.hh"
#include "crab_llvm/SeaDsaHeapAbstraction.hh"
#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif
#include "sea_dsa/AllocWrapInfo.hh"

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
#include "./crab/path_analyzer.hpp"

#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>
#include <boost/range/iterator_range.hpp>
#include <memory>
#include <functional>
#include <map>


using namespace llvm;
using namespace crab_llvm;
using namespace crab::cfg;

cl::opt<bool>
CrabPrintAns("crab-print-invariants", 
              cl::desc("Print Crab invariants"),
              cl::init(false));

cl::opt<bool>
CrabPrintSumm("crab-print-summaries", 
               cl::desc("Print Crab function summaries"),
               cl::init(false));

cl::opt<bool>
CrabPrintPreCond("crab-print-preconditions", 
               cl::desc("Print Crab necessary preconditions"),
               cl::init(false));

cl::opt<bool>
CrabStoreInvariants("crab-store-invariants", 
               cl::desc("Store invariants"),
               cl::init(true));

cl::opt<bool>
CrabStats("crab-stats", 
           cl::desc("Show Crab statistics and analysis results"),
           cl::init(false));

cl::opt<bool>
CrabBuildOnlyCFG("crab-only-cfg", 
           cl::desc("Build Crab CFG without running the analysis"),
           cl::init(false));

cl::opt<bool>
CrabPrintUnjustifiedAssumptions("crab-print-unjustified-assumptions", 
cl::desc("Print unjustified assumptions done by Crab (experimental: only integer overflow)"),
cl::init(false));

cl::opt<unsigned int>
CrabWideningDelay("crab-widening-delay", 
   cl::desc("Max number of fixpoint iterations until widening is applied"),
   cl::init(1));

cl::opt<unsigned int>
CrabNarrowingIters("crab-narrowing-iterations", 
                   cl::desc("Max number of narrowing iterations"),
                   cl::init(10));

cl::opt<unsigned int>
CrabWideningJumpSet("crab-widening-jump-set", 
                    cl::desc("Size of the jump set used for widening"),
                    cl::init(0));

cl::opt<CrabDomain>
CrabLlvmDomain("crab-dom",
      cl::desc("Crab numerical abstract domain used to infer invariants"),
      cl::values 
      (clEnumValN(INTERVALS, "int",
		   "Classical interval domain (default)"),
       clEnumValN(TERMS_INTERVALS, "term-int",
		   "Intervals with uninterpreted functions."),       
       clEnumValN(INTERVALS_CONGRUENCES, "ric",
		   "Reduced product of intervals with congruences"),
       clEnumValN(DIS_INTERVALS, "dis-int",
		   "Disjunctive intervals based on Clousot's DisInt domain"),
       clEnumValN(TERMS_DIS_INTERVALS, "term-dis-int",
		   "Disjunctive Intervals with uninterpreted functions."),
       clEnumValN(BOXES, "boxes",
		   "Disjunctive intervals based on ldds"),
       clEnumValN(ZONES_SPLIT_DBM, "zones",
		   "Zones domain with Sparse DBMs in Split Normal Form"),
       clEnumValN(OCT, "oct", "Octagons domain"),
       clEnumValN(PK, "pk", "Polyhedra domain"),
       clEnumValN(TERMS_ZONES, "rtz",
		   "Reduced product of term-dis-int and zones."),
       clEnumValN(WRAPPED_INTERVALS, "w-int",
		  "Wrapped interval domain")),
       cl::init(INTERVALS));

cl::opt<bool>
CrabBackward("crab-backward", 
           cl::desc("Perform an iterative forward/backward analysis (very experimental)\n"
		     "(Only intra-procedural version implemented)"),
           cl::init(false));

// If domain is num
cl::opt<unsigned>
CrabRelationalThreshold("crab-relational-threshold", 
   cl::desc("Max number of live vars per block before switching "
	    "to a non-relational domain"),
   cl::init(10000),
   cl::Hidden);

cl::opt<bool>
CrabLive("crab-live", 
	 cl::desc("Run Crab with live ranges. "
		  "It can lose precision if relational domains"),
	 cl::init(false));

cl::opt<bool>
CrabInter("crab-inter",
           cl::desc("Crab Inter-procedural analysis"), 
           cl::init(false));

// It does not make much sense to have non-relational domains here.
cl::opt<CrabDomain>
CrabSummDomain("crab-inter-sum-dom",
    cl::desc("Crab relational domain to generate function summaries"),
    cl::values 
    (clEnumValN(ZONES_SPLIT_DBM, "zones",
		 "Zones domain with sparse DBMs in Split Normal Form"),
     clEnumValN (OCT, "oct", "Octagons domain"),
     clEnumValN (TERMS_ZONES, "rtz",
		 "Reduced product of term-dis-int and zones.")),
    cl::init (ZONES_SPLIT_DBM));

cl::opt<enum tracked_precision>
CrabTrackLev("crab-track",
   cl::desc("Track abstraction level of the Crab Cfg"),
   cl::values
    (clEnumValN (NUM, "num", "Integer and Boolean registers only"),
     clEnumValN (PTR, "ptr", "num + pointer offsets"),
     clEnumValN (ARR, "arr", "ptr + memory contents via array abstraction")),
   cl::init (tracked_precision::NUM));

cl::opt<enum heap_analysis_t>
CrabHeapAnalysis("crab-heap-analysis",
   cl::desc("Heap analysis used for memory disambiguation"),
   cl::values
    (clEnumValN(LLVM_DSA  , "llvm-dsa"  , "context-insensitive llvm dsa"),
     clEnumValN(CI_SEA_DSA, "ci-sea-dsa", "context-insensitive sea dsa"),
     clEnumValN(CS_SEA_DSA, "cs-sea-dsa", "context-sensitive sea dsa")),
   cl::init(heap_analysis_t::LLVM_DSA));

// Specific llvm-dsa/sea-dsa options
cl::opt<bool>
CrabDsaDisambiguateUnknown("crab-dsa-disambiguate-unknown",
    cl::desc("Disambiguate unknown pointers (unsound)"), 
    cl::init(false),
    cl::Hidden);

cl::opt<bool>
CrabDsaDisambiguatePtrCast("crab-dsa-disambiguate-ptr-cast",
    cl::desc("Disambiguate pointers that have been casted from/to integers (unsound)"), 
    cl::init(false),
    cl::Hidden);

cl::opt<bool>
CrabDsaDisambiguateExternal("crab-dsa-disambiguate-external",
    cl::desc("Disambiguate pointers that have been passed to external functions (unsound)"), 
    cl::init(false),
    cl::Hidden);

// Prove assertions
cl::opt<assert_check_kind_t>
CrabCheck("crab-check", 
	   cl::desc("Check user assertions"),
	   cl::values(
	       clEnumValN (NOCHECKS  , "none"  , "None"),
	       clEnumValN (ASSERTION , "assert", "User assertions"),
	       clEnumValN (NULLITY   , "null"  , "Null dereference (unused/untested)")),
	   cl::init (assert_check_kind_t::NOCHECKS));

cl::opt<unsigned int>
CrabCheckVerbose("crab-check-verbose", 
                 cl::desc("Print verbose information about checks"),
                 cl::init(0));

// Important to crab-llvm clients (e.g., SeaHorn):
// Shadow variables are variables that cannot be mapped back to a
// const Value*. These are created for instance for memory heaps.
cl::opt<bool>
CrabKeepShadows("crab-keep-shadows",
    cl::desc("Preserve shadow variables in invariants, summaries, and preconditions"), 
    cl::init(false),
    cl::Hidden);

// In C++11 we need to pass to std::bind "this" (if class member
// functions are called) as well as all placeholders for each
// argument. This is wrapper to avoid that and improve readability.
template <class C, typename Ret, typename ... Ts>
static std::function<Ret(Ts...)> bind_this(C* c, Ret (C::*m)(Ts...)) {
  return [=](Ts&&... args) { return (c->*m)(std::forward<decltype(args)>(args)...); };
}

namespace crab_llvm {

  static crab::crab_os& get_crab_os(bool show_time = true) {
    crab::crab_os* result = &crab::outs();
    if (show_time) {
      time_t now = time(0);
      struct tm tstruct;
      char buf[80];
      tstruct = *localtime(&now);
      strftime(buf, sizeof(buf), "[%Y-%m-%d.%X] ", &tstruct);
      *result << buf;
    }
    return *result;
  }
  
  using namespace crab::analyzer;
  using namespace crab::checker;
  using namespace crab::cg;

  /** Begin typedefs **/
  typedef crab::analyzer::liveness<cfg_ref_t> liveness_t;
  typedef crab::cg::call_graph<cfg_ref_t> call_graph_t; 
  typedef crab::cg::call_graph_ref<call_graph_t> call_graph_ref_t;
  typedef boost::unordered_map<cfg_ref_t, const liveness_t*> liveness_map_t;
  typedef DenseMap<const BasicBlock*, lin_cst_sys_t> assumption_map_t;
  typedef typename IntraCrabLlvm::wrapper_dom_ptr wrapper_dom_ptr;    
  typedef typename IntraCrabLlvm::checks_db_t checks_db_t;
  typedef typename IntraCrabLlvm::invariant_map_t invariant_map_t;
  typedef typename IntraCrabLlvm::heap_abs_ptr heap_abs_ptr;
  /** End typedefs **/
  
  /** Begin global counters **/
  static unsigned num_invars; // some measure for the size of invariants
  static unsigned num_nontrivial_blocks;
  /** End global counters **/
  
  static bool isRelationalDomain(CrabDomain dom) {
    return (dom == ZONES_SPLIT_DBM || dom == OCT ||
	    dom == PK || dom == TERMS_ZONES);
  }

  static bool isTrackable(const Function &fun) {
    return !fun.isDeclaration () && !fun.empty () && !fun.isVarArg ();
  }

  /** convenient wrapper for the invariance analysis datastructures **/
  struct InvarianceAnalysisResults {
    // invariants that hold at the entry of a block
    invariant_map_t &premap;
    // invariants that hold at the exit of a block
    invariant_map_t &postmap;
    // database with all the checks
    checks_db_t &checksdb;

    InvarianceAnalysisResults(invariant_map_t &pre, invariant_map_t &post,
			      checks_db_t &db)
      : premap(pre), postmap(post), checksdb(db) {}
  };

  /** return invariant for block in table but filtering out shadow_varnames **/
  static wrapper_dom_ptr lookup(const invariant_map_t &table,
				const llvm::BasicBlock &block,
				const std::vector<varname_t> &shadow_varnames) {
    auto it = table.find (&block);
    if (it == table.end()) {
      return nullptr;
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
      auto invs = it->second->clone();
      invs->forget(shadow_vars); 
      return invs;
    }
  }   

  /** update table with pre or post invariants **/
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
      const invariant_map_t &m_premap;
      const invariant_map_t &m_postmap;
      std::vector<varname_t> m_shadow_vars;
      
    public:
      invariant_annotation (const llvm_variable_factory &vfac,
			    const invariant_map_t &premap,
			    const invariant_map_t &postmap,
			    const bool keep_shadows)
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
	  wrapper_dom_ptr pre = lookup(m_premap, *bb, m_shadow_vars);
	  o << "  " << name() << ": ";
	  if (pre){
	    o << pre << "\n";
	  } else {
	    o << "null\n";
	  }
	}
      }
      
      void print_end(basic_block_label_t bbl, crab::crab_os &o) const {
	if (const llvm::BasicBlock *bb = bbl.get_basic_block()) {
	  wrapper_dom_ptr post = lookup(m_postmap, *bb, m_shadow_vars);
	  o << "  " << name() << ": ";
	  if (post) {
	    o << post << "\n";
	  } else {
	    o << "null\n";
	  }
	}
      }
    };

    /** Annotation for neccesary_preconditions **/
    template<typename Analyzer>
    class nec_precondition_annotation: public block_annotation {
    private:
      Analyzer &m_analyzer;
      
    public:
      nec_precondition_annotation (Analyzer &analyzer)
	: block_annotation(), m_analyzer(analyzer) {}
      
      std::string name() const { return "NECESSARY PRECONDITIONS";}
      
      void print_begin(basic_block_label_t bbl, crab::crab_os &o) const {
	auto pre = m_analyzer.get_preconditions(bbl);
	o << "  " << name() << ": " << pre << "\n";	
      }
    };

    /** Annotation for unjustified assumptions done by the analysis **/
    class unjust_assumption_annotation: public block_annotation {
    private:
      typedef typename assumption_analysis<cfg_ref_t>::assumption_ptr assumption_ptr;
      
    public:
      typedef assumption_analysis<cfg_ref_t> unjust_assumption_analysis_t;
      
    private:
      typedef typename cfg_ref_t::statement_t statement_t;
      
      cfg_ref_t m_cfg;
      unjust_assumption_analysis_t *m_analyzer;
      
    public:
      unjust_assumption_annotation (cfg_ref_t cfg, unjust_assumption_analysis_t *analyzer)
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
		   et = assumes.end(); it!=et;) {
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
	    o << "  /** "; assume_ptr->write(o); o << "**/\n";
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
      
      print_block (cfg_ref_t cfg, crab::crab_os &o,
		   const std::vector<std::unique_ptr<block_annotation>> &annotations)
	: m_cfg(cfg), m_o(o), m_annotations(annotations) {} 

      void operator()(basic_block_label_t bbl) const {
	// do not print block if no annotations
	if (m_annotations.empty()) return;
	
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

	std::pair<cfg_ref_t::const_succ_iterator, 
		  cfg_ref_t::const_succ_iterator> p = bb.next_blocks();
	cfg_ref_t::const_succ_iterator it = p.first;
	cfg_ref_t::const_succ_iterator et = p.second;
	if (it != et) {
	  m_o << "  " << "goto ";
	  for (; it != et; ) {
	    m_o << crab::cfg_impl::get_label_str (*it);
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

    typedef boost::unordered_set<basic_block_label_t> visited_t;
    template<typename T>
    void dfs_rec (cfg_ref_t cfg, basic_block_label_t curId, visited_t &visited, T f) {
      if (visited.find (curId) != visited.end ()) return;
      visited.insert (curId);
      const basic_block_t &cur = cfg.get_node(curId);
      f (curId);
      for (auto const n : boost::make_iterator_range (cur.next_blocks ())) {
    	dfs_rec (cfg, n, visited, f);
      }
    }
    
    template<typename T>
    void dfs (cfg_ref_t cfg, T f) {
      visited_t visited;
      dfs_rec (cfg, cfg.entry(), visited, f);
    }

    void print_annotations(cfg_ref_t cfg, const std::vector<std::unique_ptr<block_annotation>> &annotations) {
      print_block f(cfg, crab::outs(), annotations);
      dfs(cfg, f);
    }
  } //end namespace

  static std::string dom_to_str(CrabDomain dom) {
    switch (dom) {
    case INTERVALS:             return interval_domain_t::getDomainName();
    case INTERVALS_CONGRUENCES: return ric_domain_t::getDomainName();
    case BOXES:                 return boxes_domain_t::getDomainName();
    case DIS_INTERVALS:         return dis_interval_domain_t::getDomainName();
    case ZONES_SPLIT_DBM:       return split_dbm_domain_t::getDomainName();
    case TERMS_DIS_INTERVALS:   return term_dis_int_domain_t::getDomainName();
    case TERMS_ZONES:           return num_domain_t::getDomainName();
    case OCT:                   return oct_domain_t::getDomainName();
    case PK:                    return pk_domain_t::getDomainName();
    case WRAPPED_INTERVALS:     return wrapped_interval_domain_t::getDomainName();
    default:                    return "none";
    }
  }
  
  std::string AnalysisParams::abs_dom_to_str() const {
    return dom_to_str(dom);
  }

  std::string AnalysisParams::sum_abs_dom_to_str() const {
    return dom_to_str(sum_dom);
  }
  
  /* CFG Manager class */
  CfgManager::CfgManager(){}
  CfgManager::~CfgManager(){
    for (auto &kv: m_cfg_map) {
      delete kv.second;
    }
  }
  
  bool CfgManager::has_cfg(const Function &f) const {
    return m_cfg_map.find(&f) != m_cfg_map.end();
  }
  
  cfg_ref_t CfgManager::operator[](const Function &f) const {
    assert(has_cfg(f));
    
    auto it = m_cfg_map.find(&f);
    cfg_t *cfg = it->second;
    return cfg_ref_t(*cfg);
  }
  
  void CfgManager::add(const Function &f, cfg_t *cfg) {
    if (!has_cfg(f)) {
      m_cfg_map.insert(std::make_pair(&f, cfg));
    }
  }
  
  /**
   * Internal implementation of the intra-procedural analysis
   **/
  class IntraCrabLlvm_Impl {
    
    cfg_t *m_cfg;
    Function &m_fun;
    llvm_variable_factory &m_vfac;
    typename CfgBuilder::edge_to_bb_map_t m_edge_bb_map;
    
    template<typename Dom>
    void analyzeCfg(const AnalysisParams &params,
		    const BasicBlock *entry,
		    const assumption_map_t &assumptions, liveness_t *live,
		    InvarianceAnalysisResults &results) {
      
      // -- we use the combined forward/backward analyzer
      typedef intra_forward_backward_analyzer<cfg_ref_t,Dom> intra_analyzer_t;
      // -- checkers for assertions and nullity
      typedef intra_checker<intra_analyzer_t> intra_checker_t;
      typedef assert_property_checker<intra_analyzer_t> assert_prop_t;
      typedef null_property_checker<intra_analyzer_t> null_prop_t;
      
      CRAB_VERBOSE_IF(1,
		      auto fdecl = m_cfg->get_func_decl ();            
		      assert (fdecl);
		      get_crab_os() << "Running intra-procedural analysis with " 
		                    << "\"" << Dom::getDomainName ()  << "\""
		                    << " for "  << (*fdecl).get_func_name ()
		                    << "  ... \n";);
      
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
      
      Dom post_cond = Dom::top();
      if (params.check && params.run_backward) {
	// XXX: we compute preconditions that might lead some program
	// execution to an error state. If those preconditions are
	// false then we can conclude the program is safe.
	post_cond = Dom::bottom();
      }

      // We use as initial state an assumption if exists
      Dom entry_dom = Dom::top();
      auto it = crab_assumptions.find(entry);
      if (it != crab_assumptions.end()) {
	entry_dom = it->second;
      }
      
      analyzer.run(basic_block_label_t(entry), entry_dom, post_cond,
		   !params.run_backward, crab_assumptions, live,
		   params.widening_delay, params.narrowing_iters, params.widening_jumpset);
      CRAB_VERBOSE_IF(1, get_crab_os() << "Finished intra-procedural analysis.\n"); 

      // -- store invariants
      if (params.store_invariants || params.print_invars) {
	CRAB_VERBOSE_IF(1, get_crab_os() << "Storing invariants.\n");       
	for (basic_block_label_t bl: boost::make_iterator_range(m_cfg->label_begin(),
								m_cfg->label_end())) {
	  const BasicBlock *B = bl.get_basic_block();
	  if (!B) continue; // we only store those which correspond to llvm basic blocks
	  
	  // --- invariants that hold at the entry of the blocks
	  auto pre = analyzer.get_pre (bl);
	  update(results.premap, *B,  mkGenericAbsDomWrapper(pre));
	  // --- invariants that hold at the exit of the blocks
	  auto post = analyzer.get_post (bl);
	  update(results.postmap, *B,  mkGenericAbsDomWrapper(post));	
	  if (params.stats) {
	    unsigned num_block_invars = 0;
	    // XXX: for boxes it would be more useful to get a measure
	    // from to_disjunctive_linear_constraint_system() but it
	    // can be really slow. 
	    num_block_invars += pre.to_linear_constraint_system().size();
	    num_invars += num_block_invars;
	    if (num_block_invars > 0) num_nontrivial_blocks++;
	  }
	}
	CRAB_VERBOSE_IF(1, get_crab_os() << "All invariants stored.\n");
      }
      
      // -- print all cfg annotations (if any)
      if (params.print_invars ||
	  (params.print_preconds && params.run_backward) ||
	  params.print_unjustified_assumptions) {

	typedef pretty_printer_impl::block_annotation block_annotation_t;
	typedef pretty_printer_impl::invariant_annotation inv_annotation_t;
	typedef pretty_printer_impl::nec_precondition_annotation<intra_analyzer_t> pre_annotation_t;
	typedef pretty_printer_impl::unjust_assumption_annotation unjust_assume_annotation_t;
	std::vector<std::unique_ptr<block_annotation_t>> pool_annotations;

	if (auto f_decl = m_cfg->get_func_decl()) {
	  crab::outs() << "\n" << *f_decl << "\n";
	} else {
	  llvm::outs() << "\n" << "function " << m_fun.getName() << "\n";
	}
	if (params.print_invars) {
	  pool_annotations.emplace_back(
	       make_unique<inv_annotation_t>(m_vfac, results.premap, results.postmap, 
					     params.keep_shadow_vars));
	}

	if (params.print_preconds && params.run_backward) {
	  pool_annotations.emplace_back(make_unique<pre_annotation_t>(analyzer));
	}

	// XXX: it must be alive when print_annotations is called.
	#if 0
	assumption_naive_analysis<cfg_ref_t> unjust_assumption_analyzer(*m_cfg);
	#else
	assumption_dataflow_analysis<cfg_ref_t> unjust_assumption_analyzer(*m_cfg);
	#endif 
	
	if (params.print_unjustified_assumptions) {
	  // -- run first the analysis
	  unjust_assumption_analyzer.exec();
	  pool_annotations.emplace_back(
	       make_unique<unjust_assume_annotation_t>(*m_cfg, &unjust_assumption_analyzer));
	}

	pretty_printer_impl::print_annotations(*m_cfg, pool_annotations);
      }
          
      if (params.check) {
	// --- checking assertions and collecting data
	CRAB_VERBOSE_IF(1, get_crab_os() << "Checking assertions ... \n"); 
	typename intra_checker_t::prop_checker_ptr
	  prop (new assert_prop_t (params.check_verbose));
	if (params.check == NULLITY)
	  prop.reset (new null_prop_t(params.check_verbose));
	intra_checker_t checker (analyzer, {prop});
	checker.run ();
	CRAB_VERBOSE_IF(1,
			llvm::outs() << "Function " << m_fun.getName() << "\n";
			checker.show (crab::outs()));
	results.checksdb += checker.get_all_checks();
	CRAB_VERBOSE_IF(1, get_crab_os() << "Finished assert checking.\n");      
      }

      
      return;
    }

    template<typename AbsDom>
    void wrapperPathAnalyze(const std::vector<llvm_basic_block_wrapper>& path,
			    std::vector<crab::cfg::statement_wrapper>& core,
			    bool layered_solving, bool populate_maps,
			    invariant_map_t& post, invariant_map_t& pre, bool &res) {
      
      typedef path_analyzer<cfg_ref_t, AbsDom> path_analyzer_t;
      AbsDom init;
      path_analyzer_t path_analyzer(*m_cfg, init);
      bool compute_preconditions = populate_maps;
      res = path_analyzer.solve(path, layered_solving, compute_preconditions);
      if (populate_maps) {
	for(auto n: path) {
	  if (const llvm::BasicBlock* bb = n.get_basic_block()) {
	    AbsDom abs_val = path_analyzer.get_fwd_constraints(n);
	    post.insert(std::make_pair(bb, mkGenericAbsDomWrapper(abs_val)));
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

	if (populate_maps) {
	  for(auto n: path) {
	    if (const llvm::BasicBlock* bb = n.get_basic_block()) {
	      AbsDom abs_val = path_analyzer.get_bwd_constraints(n);
	      pre.insert(std::make_pair(bb, mkGenericAbsDomWrapper(abs_val)));
	      if (abs_val.is_bottom()) {
		// the rest of blocks must be also bottom so we don't
		// bother storing them.
		break;
	      }
	    }
	  }
	}
      }
    }
        
    
    struct intra_analysis {
      std::function<void(const AnalysisParams&,
			 const BasicBlock*,
			 const assumption_map_t&,
			 liveness_t*,
			 InvarianceAnalysisResults&)> analyze;
      std::string name;
    };

    struct path_analysis {
      std::function<void(const std::vector<llvm_basic_block_wrapper>&,
			 std::vector<crab::cfg::statement_wrapper>&,
			 bool, bool, invariant_map_t&, invariant_map_t&,bool&)> analyze;
      std::string name;
    };
    
    // Domains used for intra-procedural analysis
    const std::map<CrabDomain, intra_analysis> intra_analyses {
      { INTERVALS               , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<interval_domain_t>), "classical intervals" } } 
      #ifdef HAVE_ALL_DOMAINS	
      , { INTERVALS_CONGRUENCES , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<ric_domain_t>), "reduced product of intervals and congruences" }}
      , { DIS_INTERVALS         , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<dis_interval_domain_t>), "disjunctive intervals" }}
      , { TERMS_INTERVALS       , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<term_int_domain_t>), "terms with intervals" }}
      #endif 	
      , { WRAPPED_INTERVALS     , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<wrapped_interval_domain_t>), "wrapped intervals" }}
      , { ZONES_SPLIT_DBM       , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<split_dbm_domain_t>), "zones" }}
      , { BOXES                 , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<boxes_domain_t>), "boxes" }}
      , { OCT                   , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<oct_domain_t>), "octagons" }}
      , { PK                    , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<pk_domain_t>), "polyhedra" }}
      , { TERMS_ZONES           , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<num_domain_t>), "terms with zones" }}
      , { TERMS_DIS_INTERVALS   , { bind_this(this, &IntraCrabLlvm_Impl::analyzeCfg<term_dis_int_domain_t>), "terms with disjunctive intervals" }}
    };


    // Domains used for path-based analysis
    const std::map<CrabDomain, path_analysis> path_analyses {
      { INTERVALS               , { bind_this(this, &IntraCrabLlvm_Impl::wrapperPathAnalyze<interval_domain_t>), "classical intervals" } } 
      #ifdef HAVE_ALL_DOMAINS	
      , { TERMS_INTERVALS       , { bind_this(this, &IntraCrabLlvm_Impl::wrapperPathAnalyze<term_int_domain_t>), "terms with intervals" }}
      #endif 	
      , { WRAPPED_INTERVALS     , { bind_this(this, &IntraCrabLlvm_Impl::wrapperPathAnalyze<wrapped_interval_domain_t>), "wrapped intervals" }}
      , { ZONES_SPLIT_DBM       , { bind_this(this, &IntraCrabLlvm_Impl::wrapperPathAnalyze<split_dbm_domain_t>), "zones" }}
      , { BOXES                  , { bind_this(this, &IntraCrabLlvm_Impl::wrapperPathAnalyze<boxes_domain_t>), "boxes" }}      
      , { TERMS_ZONES           , { bind_this(this, &IntraCrabLlvm_Impl::wrapperPathAnalyze<num_domain_t>), "terms with zones" }}
      /* 
	 To add new domains here make sure you add an explicit
	 instantiation in crab/path_analyzer.cc 
      */
      //, { TERMS_DIS_INTERVALS , { bind_this(this, &IntraCrabLlvm_Impl::wrapperPathAnalyze<term_dis_int_domain_t>), "terms with disjunctive intervals" }}
    };

  public:
    
    IntraCrabLlvm_Impl(Function &fun,
		       crab::cfg::tracked_precision cfg_precision,
		       heap_abs_ptr mem, llvm_variable_factory &vfac,
		       CfgManager &cfg_man, const TargetLibraryInfo &tli)
		       
      : m_cfg(nullptr), m_fun(fun), m_vfac(vfac) {
      CRAB_VERBOSE_IF(1, get_crab_os() << "Started Crab CFG construction for "
		                       << fun.getName() << "\n");
      if (isTrackable(m_fun)) {
	// -- build a crab cfg for func
	CfgBuilder builder(m_fun, m_vfac, *mem, cfg_precision, true, &tli);
	m_cfg = builder.get_cfg();
	m_edge_bb_map = builder.getEdgeToBBMap();
	cfg_man.add(fun, m_cfg);
	CRAB_VERBOSE_IF(1, get_crab_os() << "Finished Crab CFG construction for "
			                 << fun.getName() << "\n");	
	  
      } else {
	CRAB_VERBOSE_IF(1, llvm::outs() << "Cannot build CFG for "
			                << fun.getName() << "\n");
      }
    }

    void Analyze(AnalysisParams &params,
		 const llvm::BasicBlock *entry,
		 const assumption_map_t &assumptions,
		 InvarianceAnalysisResults &results) {

      if (!m_cfg) {
	CRAB_VERBOSE_IF(1, llvm::outs() << "Skipped analysis for "
			                << m_fun.getName() << "\n");
	return;
      }
      
      // -- run liveness
      liveness_t live(*m_cfg);
      if (params.run_liveness || isRelationalDomain(params.dom)) {
	CRAB_VERBOSE_IF(1,
			auto fdecl = m_cfg->get_func_decl ();            
			assert (fdecl);
			get_crab_os() << "Running liveness analysis for " 
			              << (*fdecl).get_func_name ()
		                      << "  ...\n";);
	live.exec ();
	CRAB_VERBOSE_IF(1, get_crab_os() << "Finished liveness analysis.\n");
	// some stats
	unsigned total_live, avg_live_per_blk, max_live_per_blk;
	live.get_stats (total_live, max_live_per_blk, avg_live_per_blk);
	CRAB_VERBOSE_IF(1, 
		  crab::outs() << "-- Max number of out live vars per block=" 
                               << max_live_per_blk << "\n"
                               << "-- Avg number of out live vars per block=" 
                               << avg_live_per_blk << "\n";);
	crab::CrabStats::count_max ("Liveness.count.maxOutVars",
				    max_live_per_blk);

	if (isRelationalDomain(params.dom)) {
	  CRAB_VERBOSE_IF(1, 
		    crab::outs() << "Max live per block: "
		                 << max_live_per_blk << "\n"
		                 << "Threshold: "
		                 << params.relational_threshold << "\n");
	  if (max_live_per_blk > params.relational_threshold) {
	    // default domain
	    params.dom = INTERVALS;
	  }
	}
      }

      if (CrabBuildOnlyCFG) {
	return;
      }
      
      if (intra_analyses.count(params.dom)) {
      	intra_analyses.at(params.dom).analyze(params, entry, assumptions,
					     (params.run_liveness)? &live : nullptr,
					     results);
      } else {
      	crab::outs() << "Warning: abstract domain not found or enabled.\n"
      		     << "Running " << intra_analyses.at(INTERVALS).name << " ...\n"; 
      	intra_analyses.at(INTERVALS).analyze(params, entry, assumptions,
					    (params.run_liveness)? &live : nullptr,
					    results);
      }
    }
    
    bool pathAnalyze(const AnalysisParams& params,
		     const std::vector<const llvm::BasicBlock*>& blocks,
		     bool layered_solving, 
		     std::vector<crab::cfg::statement_wrapper>& core,
		     bool populate_maps, 
		     invariant_map_t& post, invariant_map_t& pre) const {
      assert(m_cfg);

      // build the full path (included internal basic blocks added
      // during the translation to Crab)
      std::vector<llvm_basic_block_wrapper> path;
      path.reserve(blocks.size());
      for(unsigned i=0; i < blocks.size(); ++i) {
	path.push_back(blocks[i]);
	if (i < blocks.size() - 1) {
	  auto it = m_edge_bb_map.find(std::make_pair(blocks[i], blocks[i+1]));
	  if (it != m_edge_bb_map.end()) {
	    path.push_back(it->second);
	  }
	}
      }

      bool res;
      if (path_analyses.count(params.dom)) {
      	path_analyses.at(params.dom).analyze(path, core, layered_solving , populate_maps,
					     post, pre, res);
      } else {
      	crab::outs() << "Warning: abstract domain not found or enabled.\n"
      		     << "Running " << path_analyses.at(INTERVALS).name << " ...\n";
      	path_analyses.at(INTERVALS).analyze(path, core, layered_solving, populate_maps,
					    post, pre, res);
	
      }
      return res;
    }
    
  }; // end class

  /**
   *   Begin IntraCrabLlvm methods
   **/
  IntraCrabLlvm::IntraCrabLlvm(Function &fun, const TargetLibraryInfo &tli,
			       CfgManager &cfg_man,
			       crab::cfg::tracked_precision cfg_precision,
			       heap_abs_ptr heap_abs)
    : m_impl(nullptr), m_fun(&fun) {
    if (!heap_abs)
      heap_abs = boost::make_shared<DummyHeapAbstraction>();
    
    m_impl = make_unique<IntraCrabLlvm_Impl>(fun, cfg_precision,
					     heap_abs, m_vfac, cfg_man, tli);
  }

  IntraCrabLlvm::~IntraCrabLlvm() {}

  void IntraCrabLlvm::clear() {
    m_pre_map.clear();
    m_post_map.clear();
    m_checks_db.clear();
  }
  
  void IntraCrabLlvm::analyze(AnalysisParams &params,
			      const assumption_map_t &assumptions) {    
    InvarianceAnalysisResults results = { m_pre_map, m_post_map, m_checks_db};
    
    m_impl->Analyze(params, &(m_fun->getEntryBlock()), assumptions, results);
  }

  void IntraCrabLlvm::analyze(AnalysisParams &params,
			      const llvm::BasicBlock *entry,
			      const assumption_map_t &assumptions) {
    InvarianceAnalysisResults results = { m_pre_map, m_post_map, m_checks_db};
    m_impl->Analyze(params, entry, assumptions, results);
  }
  
  template<>
  bool IntraCrabLlvm::path_analyze(const AnalysisParams& params,
				   const std::vector<const llvm::BasicBlock*>& path,
				   bool layered_solving, 
				   std::vector<crab::cfg::statement_wrapper>& core) const {
    invariant_map_t pre_conditions, post_conditions;
    return m_impl->pathAnalyze(params, path, layered_solving, core, false,
			       post_conditions, pre_conditions);
  }

  template<>
  bool IntraCrabLlvm::path_analyze(const AnalysisParams& params,
				   const std::vector<const llvm::BasicBlock*>& path,
				   bool layered_solving, 
				   std::vector<crab::cfg::statement_wrapper>& core,
				   invariant_map_t& post_conditions,
				   invariant_map_t& pre_conditions) const {
    return m_impl->pathAnalyze(params, path, layered_solving, core, true,
			       post_conditions, pre_conditions);
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

  const checks_db_t& IntraCrabLlvm::get_checks_db() const { return m_checks_db;}
  
  /**
   *   End IntraCrabLlvm methods
   **/

  /**
   *   Internal implementation of the inter-procedural analysis
   **/
  class InterCrabLlvm_Impl {
    std::unique_ptr<call_graph_t> m_cg;
    Module& m_M;
    llvm_variable_factory &m_vfac;
    liveness_map_t m_live_map;
      
    /** Run inter-procedural analysis on the whole call graph **/
    template<typename BUDom, typename TDDom>
    void analyzeCg(const AnalysisParams &params,
		   InvarianceAnalysisResults &results) {
      
      typedef inter_fwd_analyzer<call_graph_ref_t, BUDom, TDDom> inter_analyzer_t;
      typedef inter_checker<inter_analyzer_t> inter_checker_t;
      typedef assert_property_checker<inter_analyzer_t> assert_prop_t;
      typedef null_property_checker<inter_analyzer_t> null_prop_t;
      
      CRAB_VERBOSE_IF(1, 
 		      get_crab_os() << "Running inter-procedural analysis with " 
		                    << "forward domain:" 
		                    << "\"" << TDDom::getDomainName () << "\""
		                    << " and bottom-up domain:" 
		                    << "\"" << BUDom::getDomainName () << "\"" 
		                    << "  ...\n";);
      
      inter_analyzer_t analyzer(*m_cg, (params.run_liveness ? &m_live_map : nullptr),
				params.widening_delay, 
				params.narrowing_iters, 
				params.widening_jumpset);
      analyzer.Run (TDDom::top ());
    
      CRAB_VERBOSE_IF(1, get_crab_os() << "Finished inter-procedural analysis.\n");
      
      // -- store invariants
      if (params.store_invariants || params.print_invars) {
	CRAB_VERBOSE_IF(1, get_crab_os() << "Storing invariants.\n");
      }
      
      for (auto &n: boost::make_iterator_range(vertices(*m_cg))) {
	cfg_ref_t cfg = n.get_cfg ();
	if (const Function *F = m_M.getFunction(n.name())) {
	  

	  if (params.store_invariants || params.print_invars) {
	    for (auto &B : *F) {
	      // --- invariants that hold at the entry of the blocks
	      auto pre = analyzer.get_pre (cfg, &B);
	      update(results.premap, B, mkGenericAbsDomWrapper(pre));
	      // --- invariants that hold at the exit of the blocks
	      auto post = analyzer.get_post (cfg, &B);
	      update(results.postmap, B, mkGenericAbsDomWrapper(post));
	      
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
	    
	    // --- print invariants and summaries
	    if (params.print_invars && isTrackable(*F)) {
	      if (auto f_decl = cfg.get_func_decl()) {
		crab::outs() << "\n" << *f_decl << "\n";
	      } else {
		llvm::outs() << "\n" << "function " << F->getName () << "\n";
	      }
	      std::vector<std::unique_ptr<pretty_printer_impl::block_annotation>> annotations;
	      annotations.emplace_back(make_unique<pretty_printer_impl::invariant_annotation>
				       (m_vfac, results.premap, results.postmap,
					params.keep_shadow_vars));
	      pretty_printer_impl::print_annotations(cfg, annotations);	    
	    }
	  }
	  
	  // Summaries are not currently stored but it would be easy to do so.	    
	  if (params.print_summaries && analyzer.has_summary (cfg)) {
	    auto summ = analyzer.get_summary (cfg);
	    crab::outs() << "SUMMARY " << *summ << "\n";
	  }
	}
      }
      
      if (params.store_invariants || params.print_invars) {	
	CRAB_VERBOSE_IF(1, get_crab_os() << "All invariants stored.\n");
      }
      
      // --- checking assertions and collecting data
      if (params.check) {
	CRAB_VERBOSE_IF(1, get_crab_os() << "Checking assertions ... \n"); 
	typename inter_checker_t::prop_checker_ptr
	  prop(new assert_prop_t(params.check_verbose));
	if (params.check == NULLITY)
	  prop.reset (new null_prop_t(params.check_verbose));      
	inter_checker_t checker (analyzer, {prop});
	checker.run ();
	//CRAB_VERBOSE_IF(1, checker.show (crab::outs()));
	results.checksdb += checker.get_all_checks();
	CRAB_VERBOSE_IF(1, get_crab_os() << "Finished assert checking.\n"); 
      }
      return;
    }
    // Domains used for inter-procedural analysis
    struct inter_analysis {
      std::function<void(const AnalysisParams&, InvarianceAnalysisResults&)> analyze;
      std::string name;
    };

    // Domains used for intra-procedural analysis
    const std::map<std::pair<CrabDomain,CrabDomain>, inter_analysis> inter_analyses {
        {{ZONES_SPLIT_DBM, INTERVALS},
	 { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, interval_domain_t>), "bottom-up:zones, top-down:intervals" }}
      , {{ZONES_SPLIT_DBM, WRAPPED_INTERVALS},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, wrapped_interval_domain_t>), "bottom-up:zones, top-down:wrapped intervals" }}
      , {{ZONES_SPLIT_DBM, ZONES_SPLIT_DBM},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, split_dbm_domain_t>), "bottom-up:zones, top-down:zones" }}
      , {{ZONES_SPLIT_DBM, BOXES},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, boxes_domain_t>), "bottom-up:zones, top-down:boxes" }}
      , {{ZONES_SPLIT_DBM, OCT},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, oct_domain_t>), "bottom-up:zones, top-down:oct" }}
      , {{ZONES_SPLIT_DBM, PK},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, pk_domain_t>), "bottom-up:zones, top-down:pk" }}
      , {{ZONES_SPLIT_DBM, TERMS_ZONES},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, num_domain_t>), "bottom-up:zones, top-down:terms+zones" }}
      , {{ZONES_SPLIT_DBM, TERMS_DIS_INTERVALS},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<split_dbm_domain_t, term_dis_int_domain_t>), "bottom-up:zones, top-down:terms+dis_intervals" }}
      //////////////////////////////////////////////////////
      , {{OCT, INTERVALS},
	 { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, interval_domain_t>), "bottom-up:oct, top-down:intervals" }}
      , {{OCT, WRAPPED_INTERVALS},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, wrapped_interval_domain_t>), "bottom-up:oct, top-down:wrapped intervals" }}
      , {{OCT, ZONES_SPLIT_DBM},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, split_dbm_domain_t>), "bottom-up:oct, top-down:zones" }}
      , {{OCT, BOXES},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, boxes_domain_t>), "bottom-up:oct, top-down:boxes" }}
      , {{OCT, OCT},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, oct_domain_t>), "bottom-up:oct, top-down:oct" }}
      , {{OCT, PK},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, pk_domain_t>), "bottom-up:oct, top-down:pk" }}
      , {{OCT, TERMS_ZONES},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, num_domain_t>), "bottom-up:oct, top-down:terms+zones" }}
      , {{OCT, TERMS_DIS_INTERVALS},
	  { bind_this(this, &InterCrabLlvm_Impl::analyzeCg<oct_domain_t, term_dis_int_domain_t>), "bottom-up:oct, top-down:terms+dis_intervals" }}
    };
    
  public:
    
    InterCrabLlvm_Impl(Module& M,
		       crab::cfg::tracked_precision cfg_precision,
		       heap_abs_ptr mem, llvm_variable_factory &vfac,
		       CfgManager &cfg_man, const TargetLibraryInfo &tli)
      : m_cg(nullptr), m_M(M), m_vfac(vfac) {

      std::vector<cfg_ref_t> cfg_ref_vector;
      for (auto &F : m_M) {
        if (isTrackable(F)) {
	  // -- build cfg's 
	  CfgBuilder B(F, m_vfac, *mem, cfg_precision,
		       /*include function decls and callsites*/
		       true,  &tli);
	  cfg_t *cfg = B.get_cfg();
	  cfg_man.add(F, cfg);
	  cfg_ref_vector.push_back (*cfg);
	  CRAB_VERBOSE_IF(1, llvm::outs() << "Built Crab CFG for "
			  << F.getName() << "\n");
	} else {
	  CRAB_VERBOSE_IF(1, llvm::outs() << "Cannot build CFG for "
			                  << F.getName() << "\n");
	}
      }
      // build call graph
      m_cg = make_unique<call_graph_t>(cfg_ref_vector.begin(), cfg_ref_vector.end());
    }
    
    void Analyze(AnalysisParams &params,
		 const assumption_map_t &/*assumptions*/ /*unused*/,
		 InvarianceAnalysisResults &results) {

      // If the number of live variables per block is too high we
      // switch to a cheap domain regardless what the user wants.
      CrabDomain absdom =  params.dom;
      
      /* Compute liveness information and choose statically the
	 abstract domain */
      if (params.run_liveness || isRelationalDomain(absdom)) {
	unsigned max_live_per_blk = 0;
	for (auto cg_node: boost::make_iterator_range(vertices(*m_cg))) {
	  auto cfg_ref = cg_node.get_cfg();
          CRAB_VERBOSE_IF(1,
			  auto fdecl = cfg_ref.get_func_decl ();            
			  assert (fdecl);
			  get_crab_os() << "Running liveness analysis for " 
			                << (*fdecl).get_func_name () << "  ...\n";);
	  liveness_t* live = new liveness_t (cfg_ref);
          live->exec ();
          CRAB_VERBOSE_IF(1, get_crab_os() << "Finished liveness analysis.\n";);
          // some stats
          unsigned total_live, max_live_per_blk_, avg_live_per_blk;
          live->get_stats (total_live, max_live_per_blk_, avg_live_per_blk);
          max_live_per_blk = std::max (max_live_per_blk, max_live_per_blk_);
          CRAB_VERBOSE_IF(1,
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
	    CRAB_VERBOSE_IF(1,
		      crab::outs() << "Max live per block: "
		                   << max_live_per_blk << "\n"
		                   << "Threshold: "
		                   << params.relational_threshold << "\n");
	    if (max_live_per_blk > params.relational_threshold) {
	      // default domain
	      absdom = INTERVALS;
	    }
	  }
	  
	  if (params.run_liveness) {
	    m_live_map.insert(std::make_pair(cfg_ref, live));	    
	  } else {
	    delete live;
	  }
	} // end for
      }
      
      // -- run the interprocedural analysis
      if (!CrabBuildOnlyCFG) {
	if (inter_analyses.count({params.sum_dom, params.dom})) {
	  inter_analyses.at({params.sum_dom, params.dom}).analyze(params, results);
	} else {
	  crab::outs() << "Warning: abstract domains not found or enabled.\n"
		       << "Running " << inter_analyses.at({ZONES_SPLIT_DBM, INTERVALS}).name
		       << "\n";
	  inter_analyses.at({ZONES_SPLIT_DBM, INTERVALS}).analyze(params, results);	
	}
      }
      
      // free liveness map
      if (params.run_liveness) {
        for (auto &p : m_live_map) {
          delete p.second;
	}
      }
    }
  };


  /**
   *   Begin InterCrabLlvm methods
   **/
  InterCrabLlvm::InterCrabLlvm(Module &module, const TargetLibraryInfo &tli,
			       CfgManager &cfg_man,
			       crab::cfg::tracked_precision cfg_precision,
			       heap_abs_ptr heap_abs)
    : m_impl(nullptr) {
    if (!heap_abs)
      heap_abs = boost::make_shared<DummyHeapAbstraction>();

    m_impl = make_unique<InterCrabLlvm_Impl>(module, cfg_precision,
					     heap_abs, m_vfac, cfg_man, tli);
  }

  InterCrabLlvm::~InterCrabLlvm() {}

  void InterCrabLlvm::clear() {
    m_pre_map.clear();
    m_post_map.clear();
    m_checks_db.clear();
  }
  
  void InterCrabLlvm::analyze(AnalysisParams &params,
			      const assumption_map_t &assumptions) {
    InvarianceAnalysisResults results = { m_pre_map, m_post_map, m_checks_db};
    m_impl->Analyze(params, assumptions, results);
  }

  wrapper_dom_ptr InterCrabLlvm::get_pre(const llvm::BasicBlock *block,
					 bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(m_vfac.get_shadow_vars().begin(),
				       m_vfac.get_shadow_vars().end());    
    return lookup(m_pre_map, *block, shadows);
  }   

  wrapper_dom_ptr InterCrabLlvm::get_post(const llvm::BasicBlock *block,
					  bool keep_shadows) const {
    std::vector<varname_t> shadows;
    if (!keep_shadows)
      shadows = std::vector<varname_t>(m_vfac.get_shadow_vars().begin(),
				       m_vfac.get_shadow_vars().end());    
    return lookup(m_post_map, *block, shadows);
  }

  const checks_db_t& InterCrabLlvm::get_checks_db() const { return m_checks_db;}
  
  /**
   * End InterCrabLlvm methods
   **/
  
  /**
   * Begin CrabLlvmPass methods
   **/
  CrabLlvmPass::CrabLlvmPass ()
    : llvm::ModulePass (ID), 
      m_mem(boost::make_shared<DummyHeapAbstraction>()),
      m_tli(nullptr) { }

  void CrabLlvmPass::releaseMemory () {
    m_pre_map.clear(); 
    m_post_map.clear();
    m_checks_db.clear();
  }

  bool CrabLlvmPass::runOnFunction (Function &F) {
    if (!CrabInter && isTrackable(F)) {
      IntraCrabLlvm_Impl crab(F, CrabTrackLev, m_mem, m_vfac, m_cfg_man, *m_tli);
      InvarianceAnalysisResults results = { m_pre_map, m_post_map, m_checks_db};
      crab.Analyze(m_params, &F.getEntryBlock(), assumption_map_t(), results);
    }
    return false;
  }
  
  bool CrabLlvmPass::runOnModule (Module &M) {

    CRAB_VERBOSE_IF(1,
	     get_crab_os() << "Started crab-llvm\n"; 
             unsigned num_analyzed_funcs = 0;
             for (auto &F : M) {
	       if (!isTrackable(F)) continue;
               num_analyzed_funcs++;
             }
             get_crab_os() << "Total number of analyzed functions:" 
                           << num_analyzed_funcs << "\n";);

    m_tli = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
    switch (CrabHeapAnalysis) {
    case LLVM_DSA:
      #ifdef HAVE_DSA
      CRAB_VERBOSE_IF(1, get_crab_os() << "Started llvm-dsa analysis\n";);                  
      m_mem.reset
	(new LlvmDsaHeapAbstraction(M,&getAnalysis<SteensgaardDataStructures>(),
				    CrabDsaDisambiguateUnknown,
				    CrabDsaDisambiguatePtrCast,
				    CrabDsaDisambiguateExternal));
      CRAB_VERBOSE_IF(1, get_crab_os() << "Finished llvm-dsa analysis\n";);      
      break;
      #else
      // execute CI_SEA_DSA
      #endif      
    case CI_SEA_DSA:
    case CS_SEA_DSA: {
      CRAB_VERBOSE_IF(1, get_crab_os() << "Started sea-dsa analysis\n";);
      CallGraph& cg = getAnalysis<CallGraphWrapperPass>().getCallGraph();      
      const DataLayout& dl = M.getDataLayout();
      sea_dsa::AllocWrapInfo* allocWrapInfo = &getAnalysis<sea_dsa::AllocWrapInfo>();      
      m_mem.reset
	(new SeaDsaHeapAbstraction(M, cg, dl, *m_tli, *allocWrapInfo,
				   (CrabHeapAnalysis == CS_SEA_DSA),
				   CrabDsaDisambiguateUnknown,
				   CrabDsaDisambiguatePtrCast,
				   CrabDsaDisambiguateExternal));
      CRAB_VERBOSE_IF(1, get_crab_os() << "Finished sea-dsa analysis\n";);      
      break;
    }
    default:
      errs() << "Warning: running crab-llvm without memory analysis\n";
    }

    m_params.dom = CrabLlvmDomain;
    m_params.sum_dom = CrabSummDomain;
    m_params.run_backward = CrabBackward;
    m_params.run_liveness = CrabLive;
    m_params.relational_threshold = CrabRelationalThreshold;
    m_params.widening_delay = CrabWideningDelay;
    m_params.narrowing_iters = CrabNarrowingIters;
    m_params.widening_jumpset = CrabWideningJumpSet;
    m_params.stats = CrabStats;
    m_params.print_invars = CrabPrintAns;
    m_params.print_preconds = CrabPrintPreCond;
    m_params.print_unjustified_assumptions = CrabPrintUnjustifiedAssumptions;
    m_params.print_summaries = CrabPrintSumm;
    m_params.store_invariants = CrabStoreInvariants;
    m_params.keep_shadow_vars = CrabKeepShadows;
    m_params.check = CrabCheck;
    m_params.check_verbose = CrabCheckVerbose;
        
    if (CrabInter){
      InterCrabLlvm_Impl inter_crab(M, CrabTrackLev, m_mem, m_vfac, m_cfg_man, *m_tli);
      InvarianceAnalysisResults results = { m_pre_map, m_post_map, m_checks_db};
      inter_crab.Analyze(m_params, assumption_map_t(), results);
    } else {
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

  void CrabLlvmPass::getAnalysisUsage (AnalysisUsage &AU) const {
    AU.setPreservesAll ();
    #ifdef HAVE_DSA
    AU.addRequiredTransitive<SteensgaardDataStructures> ();
    #endif 
    AU.addRequired<TargetLibraryInfoWrapperPass>();
    AU.addRequired<sea_dsa::AllocWrapInfo>();
    AU.addRequired<UnifyFunctionExitNodes>();
    AU.addRequired<crab_llvm::NameValues>();
    AU.addRequired<CallGraphWrapperPass>();
    AU.addPreserved<CallGraphWrapperPass>();
  } 
  
  /**
   * For crab-llvm clients
   **/

  bool CrabLlvmPass::has_cfg(llvm::Function &F) {
    return m_cfg_man.has_cfg(F);
  }
  
  cfg_ref_t CrabLlvmPass::get_cfg(llvm::Function &F) {
    assert(m_cfg_man.has_cfg(F));
    return m_cfg_man[F];
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
    return m_checks_db.get_total_safe();
  }

  unsigned CrabLlvmPass::get_total_error_checks () const {
    return m_checks_db.get_total_error();
  }

  unsigned CrabLlvmPass::get_total_warning_checks () const {
    return m_checks_db.get_total_warning();
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
  
   


