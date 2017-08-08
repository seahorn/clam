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
#include "crab/analysis/liveness.hpp"
#include "crab/analysis/fwd_analyzer.hpp"
#include "crab/analysis/bwd_analyzer.hpp"
#include "crab/analysis/inter_fwd_analyzer.hpp"
#include "crab/checkers/assertion.hpp"
#include "crab/checkers/null.hpp"
#include "crab/checkers/checker.hpp"
#include "crab/cg/cg.hpp"
#include "crab/cg/cg_bgl.hpp"

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

cl::opt<unsigned int>
CrabWideningDelay("crab-widening-delay", 
   cl::desc("Max number of fixpoint iterations until widening is applied"),
   cl::init (1));

cl::opt<unsigned int>
CrabNarrowingIters("crab-narrowing-iterations", 
                   cl::desc("Max number of narrowing iterations"),
                   cl::init (999999));

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

typedef enum { NONE = 0, ASSERTION = 1, NULLITY = 2} assert_check_kind_t;
cl::opt<assert_check_kind_t>
CrabAssertCheck ("crab-check", 
                 cl::desc ("Check user assertions"),
                 cl::values(
                     clEnumValN (NONE      , "none"  , "None"),
                     clEnumValN (ASSERTION , "assert", "User assertions"),
                     clEnumValN (NULLITY   , "null"  , "Null dereference"),
                     clEnumValEnd),
                 cl::init (assert_check_kind_t::NONE));

cl::opt<unsigned int>
CrabCheckVerbose ("crab-check-verbose", 
                 cl::desc ("Print verbose information about checks"),
                 cl::init (0));

// Important to crab-llvm clients (e.g., SeaHorn):
// Shadow variables are variables that cannot be mapped back to a
// const Value*. These are created for instance for memory heaps.
cl::opt<bool>
KeepShadows ("crab-keep-shadows",
    cl::desc ("Preserve shadow variables in invariants, summaries, and preconditions"), 
    cl::init (false),
    cl::Hidden);

namespace crab_llvm {

  using namespace crab::analyzer;
  using namespace crab::checker;
  using namespace crab::cg;

  char crab_llvm::CrabLlvm::ID = 0;

  static bool isRelationalDomain(CrabDomain dom) {
    return (dom == ZONES_SPLIT_DBM || dom == OPT_OCT_APRON ||
	    dom == PK_APRON        || dom == TERMS_ZONES);
  }

  #define ANALYZE(DOM,CFG,F,LIVE) analyzeCfg<DOM> (CFG, F, LIVE); 

  // #ifdef FASTER_COMPILATION
  // #define INTER_ANALYZE(DOM,CG,M,LIVE)                                      \
  // switch (CrabSummDomain){                                                  \
  //   default:                                                                \
  //     if (CrabSummDomain != ZONES_SPLIT_DBM)                                \
  //       crab::outs() << "Warning: choosing zones to compute summaries\n";   \
  //     analyzeCg<split_dbm_domain_t, DOM> (CG, M, LIVE) ; }
  // #else
  #define INTER_ANALYZE(DOM,CG,M,LIVE)                        \
  switch (CrabSummDomain){                                    \
    case OPT_OCT_APRON:                                       \
      analyzeCg<opt_oct_apron_domain_t, DOM>(CG, M, LIVE) ;   \
      break;                                                  \
    case TERMS_ZONES:                                         \
      analyzeCg<num_domain_t, DOM> (CG, M, LIVE) ;	      \
      break;						      \
    default:                                                  \
      analyzeCg <split_dbm_domain_t, DOM> (CG, M, LIVE); }
  //#endif 
    
  // some global counters
  static unsigned num_invars; // some measure for the size of invariants
  static unsigned num_nontrivial_blocks;

  CrabLlvm::CrabLlvm ()
    : llvm::ModulePass (ID), 
       m_absdom (INTERVALS), 
       m_mem (boost::make_shared<DummyHeapAbstraction>()),
       m_checks_db (nullptr) { }

  void CrabLlvm::releaseMemory () {
    m_pre_map.clear(); 
    m_post_map.clear(); 
    m_cfg_map.clear ();
  }

  bool CrabLlvm::runOnModule (Module &M) {
    // -- initialize from cli options
    m_absdom = CrabLlvmDomain;

    #ifdef HAVE_DSA
    m_mem.reset
      (new LlvmDsaHeapAbstraction(M,&getAnalysis<SteensgaardDataStructures>()));
    #endif     

    CRAB_LOG("crabllvm",
             unsigned num_analyzed_funcs = 0;
             for (auto &F : M) {
               if (F.isDeclaration () || F.empty ()) continue;
               if (F.isVarArg ()) continue;
               num_analyzed_funcs++;
             }
             crab::outs() << "Total number of analyzed functions:" 
                          << num_analyzed_funcs << "\n";);

    if (CrabInter){

      std::vector<cfg_ref_t> cfgs;
      liveness_map_t live_map;
      unsigned max_live_per_blk = 0;
      for (auto &F : M) {
        // -- skip functions without a body
        if (F.isDeclaration () || F.empty ()) continue;
        // -- skip variadic functions
        if (F.isVarArg ()) continue;

        // -- build cfg
        CfgBuilder B (F, m_vfac, *m_mem, CrabTrackLev,
                      /*include function decls and callsites*/
                      true,  &getAnalysis<TargetLibraryInfo>());

        auto cfg_ptr = B.getCfg();
        m_cfg_map [&F] = cfg_ptr;
        cfgs.push_back (*cfg_ptr);

        // -- build liveness
        if (CrabLive || isRelationalDomain(m_absdom)) {
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


	  if (isRelationalDomain(m_absdom)) {
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
	      m_absdom = INTERVALS;
	      #else
	      m_absdom = DIS_INTERVALS;
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
      switch (m_absdom) {
        #ifndef FASTER_COMPILATION
        case INTERVALS_CONGRUENCES: 
          INTER_ANALYZE(ric_domain_t, *cg, M, live_map); 
          break;
        case TERMS_INTERVALS:
          INTER_ANALYZE(term_int_domain_t, *cg, M, live_map); 
          break;
        case DIS_INTERVALS:
          INTER_ANALYZE(dis_interval_domain_t, *cg, M, live_map); 
          break;
        #endif 
        case TERMS_DIS_INTERVALS:
          INTER_ANALYZE(term_dis_int_domain_t, *cg, M, live_map); 
          break;
        case ZONES_SPLIT_DBM: 
          INTER_ANALYZE(split_dbm_domain_t, *cg, M, live_map); 
          break;
        case OPT_OCT_APRON:
          INTER_ANALYZE(opt_oct_apron_domain_t, *cg, M, live_map); 
          break;
        case PK_APRON:
          INTER_ANALYZE(pk_apron_domain_t, *cg, M, live_map); 
          break;
        case TERMS_ZONES: 
          INTER_ANALYZE(num_domain_t, *cg, M, live_map); 
          break;
        default: 
          if (m_absdom != INTERVALS)
            crab::outs() << "Warning: either abstract domain not found or "
                         << "inter-procedural version not implemented.\n"
                         << "If FASTER_COMPILATION is enabled then "
	                 << "some domains are not available.\n"
                         << "Running intervals ...\n"; 
          INTER_ANALYZE (interval_domain_t, *cg, M, live_map);
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
    
    if (CrabAssertCheck) {
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


  bool CrabLlvm::runOnFunction (Function &F) {
    if (CrabInter) return false;
      
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;
    // -- skip variadic functions
    if (F.isVarArg ()) return false;

    // -- build cfg
    CfgBuilder B (F, m_vfac, *m_mem, CrabTrackLev,
                  /*include function decls and callsites*/
                  true,  &getAnalysis<TargetLibraryInfo>());

    auto cfg_ptr = B.getCfg ();
    m_cfg_map [&F] = cfg_ptr;

    // -- run liveness
    liveness_t* live = nullptr;
    if (CrabLive || isRelationalDomain(m_absdom)) {
      CRAB_LOG("crabllvm",
               auto fdecl = cfg_ptr->get_func_decl ();            
               assert (fdecl);
               crab::outs() << "Running liveness analysis for " 
                            << (*fdecl).get_func_name ()
                            << "  ... ";);
      liveness_t ls (*cfg_ptr);
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

      if (isRelationalDomain(m_absdom)) {
	CRAB_LOG("crabllvm", 
		 crab::outs() << "Max live per block: "
		              << max_live_per_blk << "\n"
                              << "Threshold: "
		              << CrabRelationalThreshold << "\n");
	if (max_live_per_blk > CrabRelationalThreshold) {
          #ifdef FASTER_COMPILATION
	  m_absdom = INTERVALS;
	  #else
	  m_absdom = DIS_INTERVALS;
	  #endif 
	}
      }

      if (CrabLive) live = &ls;
    }

    // -- run invariant generator
    switch (m_absdom) {
      #ifndef FASTER_COMPILATION
      case INTERVALS_CONGRUENCES: 
        ANALYZE(ric_domain_t, *cfg_ptr, F, *live);
        break;
      case DIS_INTERVALS:
        ANALYZE(dis_interval_domain_t, *cfg_ptr, F, *live);
        break;
      case TERMS_INTERVALS:
        ANALYZE(term_int_domain_t, *cfg_ptr, F, *live);
        break;
      #endif 
      case TERMS_DIS_INTERVALS:
        ANALYZE(term_dis_int_domain_t, *cfg_ptr, F, *live);
        break;
      case ZONES_SPLIT_DBM: 
        ANALYZE(split_dbm_domain_t, *cfg_ptr, F, *live);
        break;
      case BOXES:
        ANALYZE(boxes_domain_t, *cfg_ptr, F, *live);
        break;
      case OPT_OCT_APRON:
        ANALYZE(opt_oct_apron_domain_t, *cfg_ptr, F, *live);
        break;
      case PK_APRON:
        ANALYZE(pk_apron_domain_t, *cfg_ptr, F, *live);
        break;
      case TERMS_ZONES:
        ANALYZE(num_domain_t, *cfg_ptr, F, *live); 
        break;
      default: 
        if (m_absdom != INTERVALS) {
          crab::outs() << "Warning: abstract domain not found.\n"
		       << "If FASTER_COMPILATION is enabled then "
		       << "some domains are not available.\n"
                       << "Running intervals ...\n"; 
        }
        ANALYZE(interval_domain_t, *cfg_ptr, F, *live);
    }
    
    return false;
  }
  
  void CrabLlvm::getAnalysisUsage (AnalysisUsage &AU) const {
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
   * Run inter-procedural analysis on the whole call graph
   **/
  template<typename BUDom, typename TDDom>
  inline void CrabLlvm::analyzeCg (call_graph_ref_t cg, const Module &M,
                                   const liveness_map_t& live_map) {
    typedef inter_fwd_analyzer<call_graph_ref_t, BUDom, TDDom> 
      inter_analyzer_t;
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
    
    inter_analyzer_t analyzer(cg, (CrabLive ? &live_map : nullptr),
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
            m_pre_map.insert(std::make_pair(&B, mkGenericAbsDomWrapper(pre)));
            // --- invariants that hold at the exit of the blocks
            auto post = analyzer.get_post (cfg, &B);
            m_post_map.insert(std::make_pair(&B,mkGenericAbsDomWrapper(post)));

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
          // Summaries are not currently stored but it would be easy to do so.
          if (CrabPrintAns) 
            printInvariants(llvm::outs (), *F);
          
          if (CrabPrintSumm && analyzer.has_summary (cfg)) {
            auto summ = analyzer.get_summary (cfg);
            crab::outs() << "SUMMARY " << *summ << "\n";
          }
        }
      }
    }

    // --- checking assertions and collecting data
    if (CrabAssertCheck) {
      CRAB_LOG("crabllvm", crab::outs() << "Checking assertions ... \n"); 
      typename inter_checker_t::prop_checker_ptr
	prop(new assert_prop_t(CrabCheckVerbose));
      if (CrabAssertCheck == NULLITY)
        prop.reset (new null_prop_t(CrabCheckVerbose));      
      inter_checker_t checker (analyzer, {prop});
      checker.run ();
      CRAB_LOG("crabllvm", checker.show (crab::outs()));
	       
      m_checks_db = boost::make_shared<checks_db_t>();
      (*m_checks_db) += checker.get_all_checks();
      CRAB_LOG("crabllvm", crab::outs() << "DONE!\n"); 
    }

    return;
  }

  /**
   * Run intra-procedural analysis on a CFG
   **/
  template<typename Dom>
  inline void CrabLlvm::analyzeCfg (cfg_ref_t cfg, const Function&F,
				    const liveness_t& live) {

    typedef intra_forward_backward_analyzer<cfg_ref_t,Dom> intra_analyzer_t;
    //typedef intra_fwd_analyzer<cfg_ref_t,Dom> intra_analyzer_t;
    typedef intra_checker<intra_analyzer_t> intra_checker_t;
    typedef assert_property_checker<intra_analyzer_t> assert_prop_t;
    typedef null_property_checker<intra_analyzer_t> null_prop_t;

    
    CRAB_LOG("crabllvm",
             auto fdecl = cfg.get_func_decl ();            
             assert (fdecl);
             crab::outs() << "Running intra-procedural analysis with " 
                          << "\"" << Dom::getDomainName ()  << "\""
                          << " for "  << (*fdecl).get_func_name ()
                          << "  ... ";);

    // -- run intra-procedural analysis
    // This analysis ignores the liveness information.
    intra_analyzer_t analyzer (cfg);
    analyzer.run(Dom::top(), Dom::top(), !CrabBackward, 
		 CrabWideningDelay, CrabNarrowingIters, CrabWideningJumpSet);
    // intra_analyzer_t analyzer (cfg, Dom::top(), &live, 
    //                            CrabWideningDelay, CrabNarrowingIters, 
    //                            CrabWideningJumpSet);
    // analyzer.run();

    CRAB_LOG("crabllvm", crab::outs() << "DONE\n"); 

    // -- store invariants 
    for (auto const &B : F) {      
      // --- invariants that hold at the entry of the blocks
      auto pre = analyzer.get_pre (&B);
      m_pre_map.insert(std::make_pair(&B, mkGenericAbsDomWrapper(pre)));
      // --- invariants that hold at the exit of the blocks
      auto post = analyzer.get_post (&B);
      m_post_map.insert(std::make_pair(&B, mkGenericAbsDomWrapper(post)));
      
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

    if (CrabPrintAns) {
      printInvariants(llvm::outs(), F);
    }
    
    if (CrabPrintPreCond && CrabBackward) {
      if (!F.isDeclaration () && !F.empty () && !F.isVarArg ()) {
	llvm::outs() << "\nNecessary preconditions for " << F.getName () << "\n";
	for (auto &B : F) {
	  const llvm::BasicBlock * BB = &B;
	  llvm::outs() << "\t" << BB->getName () << ": ";
	  auto pre = analyzer.get_preconditions(BB);
	  crab::outs() << pre << "\n";
	}
	llvm::outs() <<  "\n";
      }
    }

    if (CrabAssertCheck) {
      // --- checking assertions and collecting data
      CRAB_LOG("crabllvm", crab::outs() << "Checking assertions ... \n"); 
      typename intra_checker_t::prop_checker_ptr
	prop (new assert_prop_t (CrabCheckVerbose));
      if (CrabAssertCheck == NULLITY)
        prop.reset (new null_prop_t(CrabCheckVerbose));
      intra_checker_t checker (analyzer, {prop});
      checker.run ();
      CRAB_LOG("crabllvm",
	       llvm::outs() << "Function " << F.getName () << "\n";
	       checker.show (crab::outs()));
      m_checks_db = boost::make_shared<checks_db_t>();
      (*m_checks_db) += checker.get_all_checks();
      CRAB_LOG("crabllvm", crab::outs() << "DONE!\n");      
    }

    return;
  }

  /**
   * For crab-llvm clients
   **/
  
  cfg_ptr_t CrabLlvm::getCfg (llvm::Function* F) {
    auto it = m_cfg_map.find (F);
    if (it != m_cfg_map.end ())
      return it->second;
    return nullptr;
  }
  
  // return invariants that hold at the entry of BB
  GenericAbsDomWrapperPtr 
  CrabLlvm::getPre (const llvm::BasicBlock *BB, bool keep_shadows) const {
    const_iterator it = m_pre_map.find (BB);
    assert (it != m_pre_map.end ());
    if (keep_shadows)
      return it->second;
    else {
      std::vector<varname_t> shadows (m_vfac.get_shadow_vars ().begin (),
				      m_vfac.get_shadow_vars ().end ());
      // make a copy before projecting shadow variables out
      GenericAbsDomWrapperPtr invs = it->second->clone();
      invs->forget (shadows); 
      return invs;
    }
  }   

  // return invariants that hold at the exit of BB
  GenericAbsDomWrapperPtr 
  CrabLlvm::getPost (const llvm::BasicBlock *BB, bool keep_shadows) const {
    const_iterator it = m_post_map.find (BB);
    assert (it != m_post_map.end ());
    if (keep_shadows)
      return it->second;
    else {
      std::vector<varname_t> shadows (m_vfac.get_shadow_vars ().begin (),
				      m_vfac.get_shadow_vars ().end ());
      // make a copy before projecting shadow variables out
      GenericAbsDomWrapperPtr invs = it->second->clone();
      invs->forget (shadows); 
      return invs;
    }
  }

  void CrabLlvm::printInvariants (llvm::raw_ostream& o, 
                                  const Function& F) {
    if (!F.isDeclaration () && !F.empty () && !F.isVarArg ()) {
      o << "\nInvariants for " << F.getName () << "\n";
      for (auto &B : F) {
        const llvm::BasicBlock * BB = &B;
        o << "\t" << BB->getName () << ": ";
        auto pre = getPre (BB, KeepShadows);
	#if 1
        auto post = getPost (BB, KeepShadows);
        o << pre << " ==> " << post << "\n";
	#else
	o << pre << "\n";
	#endif 
      }
      o <<  "\n";
    }
  }


  /**
   * For assertion checking
   **/
  
  unsigned CrabLlvm::get_total_checks() const {
    return get_total_safe_checks() +  
           get_total_error_checks() + 
           get_total_warning_checks();
  }

  unsigned CrabLlvm::get_total_safe_checks () const {
    return (m_checks_db ? m_checks_db->get_total_safe() : 0);
  }

  unsigned CrabLlvm::get_total_error_checks () const {
    return (m_checks_db ? m_checks_db->get_total_error() : 0);
  }

  unsigned CrabLlvm::get_total_warning_checks () const {
    return (m_checks_db ? m_checks_db->get_total_warning() : 0);
  }

  void CrabLlvm::print_checks (raw_ostream &o) const {
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
  
} // end namespace 

static RegisterPass<crab_llvm::CrabLlvm> 
X ("crab-llvm", "Infer invariants using Crab", false, false);
  
   


