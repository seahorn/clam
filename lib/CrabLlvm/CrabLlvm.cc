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
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "crab_llvm/config.h"
#include "crab_llvm/CrabLlvm.hh"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/SymEval.hh"
#include "crab_llvm/Support/NameValues.hh"

#include "crab/common/debug.hpp"
#include "crab/common/stats.hpp"
#include "crab/analysis/liveness.hpp"
#include "crab/analysis/fwd_analyzer.hpp"
#include "crab/analysis/inter_fwd_analyzer.hpp"
#include "crab/checkers/assertion.hpp"
#include "crab/checkers/null.hpp"
#include "crab/checkers/checker.hpp"
#include "crab/cg/cg.hpp"
#include "crab/cg/cg_bgl.hpp"

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

// XXX: Compile time can be very slow (due to template
//      instantiation). We enable by default this option to mitigate
//      this problem by disabling some abstract domains. Comment it
//      out for compiling all available domains.
#define SHORTER_COMPILE_TIME

using namespace llvm;
using namespace crab_llvm;

llvm::cl::opt<bool>
CrabPrintAns ("crab-print-invariants", 
              llvm::cl::desc ("Print Crab invariants"),
              llvm::cl::init (false));

llvm::cl::opt<bool>
CrabPrintSumm ("crab-print-summaries", 
               llvm::cl::desc ("Print Crab function summaries"),
               llvm::cl::init (false));

llvm::cl::opt<bool>
CrabStats ("crab-stats", 
           llvm::cl::desc ("Show Crab statistics"),
           llvm::cl::init (false));

llvm::cl::opt<unsigned int>
CrabWideningDelay("crab-widening-delay", 
   llvm::cl::desc("Max number of fixpoint iterations until widening is triggered"),
   llvm::cl::init (1));

llvm::cl::opt<unsigned int>
CrabNarrowingIters("crab-narrowing-iterations", 
                   llvm::cl::desc("Max number of narrowing iterations"),
                   llvm::cl::init (999999));

llvm::cl::opt<unsigned int>
CrabWideningJumpSet("crab-widening-jump-set", 
                    llvm::cl::desc("Size of the jump set used for widening"),
                    llvm::cl::init (0));

llvm::cl::opt<CrabDomain>
CrabLlvmDomain("crab-dom",
               llvm::cl::desc ("Crab abstract domain used to infer invariants"),
               llvm::cl::values 
               (clEnumValN (INTERVALS, "int",
                            "Classical interval domain (default)"),
                clEnumValN (INTERVALS_CONGRUENCES, "ric",
                            "Reduced product of intervals with congruences"),
                clEnumValN (TERMS_INTERVALS, "term-int",
                            "Intervals with uninterpreted functions."),
                clEnumValN (TERMS_DIS_INTERVALS, "term-dis-int",
                            "Disjunctive Intervals with uninterpreted functions."),
                clEnumValN (BOXES, "boxes",
                            "Disjunctive intervals based on ldds"),
                clEnumValN (DIS_INTERVALS, "dis-int",
                            "Disjunctive intervals based on disjunction completion"),
                clEnumValN (ZONES_SPARSE_DBM , "zones-sparse",
                            "Zones domain with Sparse Difference-Bounds Matrix (DBMs)"),
                clEnumValN (ZONES_SPLIT_DBM, "zones-split",
                            "Zones domain with Sparse DBMs in Split Normal Form"),
                clEnumValN (OPT_OCT_APRON, "opt-oct-apron",
                            "Optimized octagons domain using Elina"),
                clEnumValN (PK_APRON, "pk-apron",
                            "Polyhedra domain using Apron library"),
                clEnumValN (TERMS_ZONES, "rtz",
                            "Reduced product term-dis-int with zones-split."),
                clEnumValN (ADAPT_TERMS_ZONES, "adapt-rtz",
                            "Choose dynamically between int and rtz."),
                clEnumValEnd),
               llvm::cl::init (INTERVALS));

// If domain is num
llvm::cl::opt<unsigned>
CrabNumThreshold("crab-adapt-rtz-threshold", 
   llvm::cl::desc("Max number of live vars per block before switching domains"),
   llvm::cl::init (100),
   cl::Hidden);

llvm::cl::opt<bool>
CrabLive("crab-live", 
        llvm::cl::desc("Run Crab with live ranges"),
        llvm::cl::init (false));

llvm::cl::opt<bool>
CrabInter ("crab-inter",
           cl::desc ("Crab Inter-procedural analysis"), 
           cl::init (false));

// It does not make much sense to have non-relational domains here.
llvm::cl::opt<CrabDomain>
CrabSummDomain("crab-inter-sum-dom",
       llvm::cl::desc ("Crab relational abstract domain to generate function summaries"),
       llvm::cl::values 
       (clEnumValN (ZONES_SPARSE_DBM, "zones-sparse",
                    "Zones domain with sparse DBMs"),
        clEnumValN (ZONES_SPLIT_DBM, "zones-split",
                    "Zones domain with sparse DBMs in Split Normal Form"),
        clEnumValN (OPT_OCT_APRON, "opt-oct-apron",
                    "Optimized octagons using Elina"),
        clEnumValN (TERMS_INTERVALS, "term-int",
                    "Intervals with uninterpreted functions."),
        clEnumValN (TERMS_DIS_INTERVALS, "term-dis-int",
                    "Disjunctive Intervals with uninterpreted functions."),
        clEnumValN (TERMS_ZONES, "rtz",
                    "Reduced product of term-dis-int and zones-split"),
        clEnumValEnd),
       llvm::cl::init (ZONES_SPLIT_DBM));

llvm::cl::opt<enum tracked_precision>
CrabTrackLev("crab-track",
   llvm::cl::desc ("Track abstraction level of the Crab Cfg"),
   cl::values (clEnumValN (INT, "int", "Integer registers only"),
               clEnumValN (PTR, "ptr", "Integers + pointer offsets"),
               clEnumValN (ARR, "arr", "Integers + offsets + memory contents via array abstraction"),
               clEnumValEnd),
   cl::init (tracked_precision::INT));

typedef enum { NONE = 0, ASSERTION = 1, NULLITY = 2} assert_check_kind_t;
llvm::cl::opt<assert_check_kind_t>
CrabAssertCheck ("crab-check", 
                 llvm::cl::desc ("Check user assertions"),
                 cl::values(
                     clEnumValN (NONE      , "none"  , "None"),
                     clEnumValN (ASSERTION , "assert", "User assertions"),
                     clEnumValN (NULLITY   , "null"  , "Null dereference"),
                     clEnumValEnd),
                 cl::init (assert_check_kind_t::NONE));

llvm::cl::opt<unsigned int>
CrabCheckVerbose ("crab-check-verbose", 
                 llvm::cl::desc ("Print verbose information about checks"),
                 llvm::cl::init (0));

// Important to crab-llvm clients (e.g., SeaHorn):
// Shadow variables are variables that cannot be mapped back to a
// const Value*. These are created for instance for memory heaps.
llvm::cl::opt<bool>
KeepShadows ("crab-keep-shadows",
             cl::desc ("Preserve shadow variables in invariants and summaries"), 
             cl::init (false),
             cl::Hidden);

namespace crab_llvm {

  using namespace crab::analyzer;
  using namespace crab::checker;
  using namespace crab::cg;

  char crab_llvm::CrabLlvm::ID = 0;

  #define ANALYZE(BASE_DOM,ARR_DOM,CFG,F,LIVE,RES)                    \
  RES = (CrabTrackLev == ARR ? analyzeCfg <ARR_DOM> (CFG, F, LIVE) :  \
                               analyzeCfg <BASE_DOM> (CFG, F, LIVE));

  #ifdef SHORTER_COMPILE_TIME
  #define INTER_ANALYZE(BASE_DOM,ARR_DOM,CG,M,LIVE,RES)                    \
  switch (CrabSummDomain){                                                 \
    default:                                                               \
      if (CrabSummDomain != ZONES_SPLIT_DBM)                               \
        crab::outs() << "Warning: choosing zones-split to compute summaries\n"; \
      RES = (CrabTrackLev == ARR ?                                         \
             analyzeCg <arr_split_dbm_domain_t, ARR_DOM> (CG, M, LIVE) :   \
             analyzeCg <split_dbm_domain_t, BASE_DOM> (CG, M, LIVE)) ; }             
  #else
  #define INTER_ANALYZE(BASE_DOM,ARR_DOM,CG,M,LIVE,RES)                    \
  switch (CrabSummDomain){                                                 \
    case TERMS_INTERVALS:                                                  \
      RES = (CrabTrackLev == ARR ?                                         \
             analyzeCg <arr_term_int_domain_t, ARR_DOM> (CG, M, LIVE) :    \
             analyzeCg <term_int_domain_t, BASE_DOM> (CG, M, LIVE)) ;      \
      break;                                                               \
    case TERMS_DIS_INTERVALS:                                              \
      RES = (CrabTrackLev == ARR ?                                         \
             analyzeCg <arr_term_dis_int_domain_t, ARR_DOM> (CG, M, LIVE) : \
             analyzeCg <term_dis_int_domain_t, BASE_DOM> (CG, M, LIVE)) ;  \
      break;                                                               \
    case ZONES_SPARSE_DBM:                                                 \
      RES = (CrabTrackLev == ARR ?                                         \
             analyzeCg <arr_dbm_domain_t, ARR_DOM> (CG, M, LIVE) :         \
             analyzeCg <dbm_domain_t, BASE_DOM> (CG, M, LIVE)) ;           \
      break;                                                               \
    case OPT_OCT_APRON:                                                    \
      RES = (CrabTrackLev == ARR ?                                         \
             analyzeCg <arr_opt_oct_apron_domain_t, ARR_DOM> (CG, M, LIVE) : \
             analyzeCg <opt_oct_apron_domain_t, BASE_DOM> (CG, M, LIVE)) ; \
      break;                                                               \
    case TERMS_ZONES:                                                      \
      RES = (CrabTrackLev == ARR ?                                         \
             analyzeCg <arr_num_domain_t, ARR_DOM> (CG, M, LIVE) :         \
             analyzeCg <num_domain_t, BASE_DOM> (CG, M, LIVE)) ;           \
      break;                                                               \
    default:                                                               \
      if (CrabSummDomain != ZONES_SPLIT_DBM)                               \
        crab::outs() << "Warning: choosing zones-split to compute summaries\n"; \
      RES = (CrabTrackLev == ARR ?                                         \
             analyzeCg <arr_split_dbm_domain_t, ARR_DOM> (CG, M, LIVE) :   \
             analyzeCg <split_dbm_domain_t, BASE_DOM> (CG, M, LIVE)) ; }             
  #endif 

  bool CrabLlvm::runOnModule (llvm::Module &M) {
    // -- initialize from cli options
    m_absdom = CrabLlvmDomain;

    #ifdef HAVE_DSA
    m_mem.reset (new DSAMemAnalysis (M, &getAnalysis<SteensgaardDataStructures> ()));
    #endif     

    if (m_absdom == ADAPT_TERMS_ZONES) {
      // --- needs liveness information
      CrabLive = true;
    }

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

      vector<cfg_ref_t> cfgs;
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
        if (CrabLive) {
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
          crab::CrabStats::count_max ("Liveness.count.maxOutVars", max_live_per_blk);

          live_map.insert (make_pair (cfg_ref_t(*cfg_ptr), live));
        }
      }

      // -- build call graph

      boost::scoped_ptr<call_graph_t> cg(new call_graph_t(cfgs));
      // -- run the interprocedural analysis

      // FIXME: the selection of the final domain is fixed for the
      //        whole program. That is, if there is one function that
      //        exceeds the threshold then the cheaper domain will be
      //        used for all functions. We should be able to change
      //        from one function to another.
      CrabDomain absdom = m_absdom;
      if (absdom == ADAPT_TERMS_ZONES) {
        CRAB_LOG("crabllvm",
                 crab::outs() << "Max live per block: " << max_live_per_blk << "\n";
                 crab::outs() << "Threshold: " << CrabNumThreshold << "\n");
        if (max_live_per_blk > CrabNumThreshold)
          absdom = DIS_INTERVALS;
        else 
          absdom = TERMS_ZONES;
      }
            
      bool change = false; 
      switch (absdom) {
        #ifndef SHORTER_COMPILE_TIME
        case INTERVALS_CONGRUENCES: 
          INTER_ANALYZE (ric_domain_t, arr_ric_domain_t, *cg, M, live_map, change); 
          break;
        case ZONES_SPARSE_DBM: 
          INTER_ANALYZE (dbm_domain_t, arr_dbm_domain_t, *cg, M, live_map, change); 
          break;        
        case TERMS_INTERVALS:
          INTER_ANALYZE (term_int_domain_t, arr_term_int_domain_t, *cg, M, live_map, change); 
          break;
        case DIS_INTERVALS:
          INTER_ANALYZE (dis_interval_domain_t, arr_dis_interval_domain_t,
                         *cg, M, live_map, change); 
          break;
        #endif  /* !SHORTER_COMPILE_TIME*/
        case TERMS_DIS_INTERVALS:
          INTER_ANALYZE (term_dis_int_domain_t,arr_term_dis_int_domain_t, 
                         *cg,M,live_map,change); 
          break;
        case ZONES_SPLIT_DBM: 
          INTER_ANALYZE (split_dbm_domain_t, arr_split_dbm_domain_t,
                         *cg, M, live_map, change); 
          break;
        case OPT_OCT_APRON:
          INTER_ANALYZE (opt_oct_apron_domain_t,arr_opt_oct_apron_domain_t, 
                         *cg, M, live_map, change); 
          break;
        case PK_APRON:
          INTER_ANALYZE (pk_apron_domain_t,arr_pk_apron_domain_t, *cg,M,live_map,change); 
          break;
        case TERMS_ZONES: 
          INTER_ANALYZE (num_domain_t,arr_num_domain_t, *cg,M,live_map,change); 
          break;
        default: 
          if (absdom != INTERVALS)
            crab::outs() << "Warning: either abstract domain not found or "
                         << "inter-procedural version not implemented.\n"
                         << "If you think the domain should be found "
                         << "make sure pragma SHORTER_COMPILE_TIME is disabled.\n"
                         << "Running intervals ...\n"; 
          INTER_ANALYZE (interval_domain_t,arr_interval_domain_t,
                         *cg,M,live_map,change);
      }
      
      if (CrabStats) {
        crab::CrabStats::PrintBrunch (crab::outs());
      }

      if (CrabLive) {
        for (auto &p : live_map) {
          delete p.second;
        }
      }          
      
      return change;
    }
    else {
      // -- run intra-procedural analysis
      bool change=false;
      for (auto &f : M) {
        change |= runOnFunction (f); 
      }
      if (CrabStats) {
        crab::CrabStats::PrintBrunch (crab::outs());
      }

      return change;
    }
  }


  bool CrabLlvm::runOnFunction (llvm::Function &F)
  {

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
    unsigned max_live_per_blk = 0;
    if (CrabLive) {
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
      unsigned total_live, avg_live_per_blk;
      ls.get_stats (total_live, max_live_per_blk, avg_live_per_blk);
      CRAB_LOG("crabllvm", 
               crab::outs() << "-- Max number of out live vars per block=" 
                            << max_live_per_blk << "\n"
                            << "-- Avg number of out live vars per block=" 
                            << avg_live_per_blk << "\n";);
      crab::CrabStats::count_max ("Liveness.count.maxOutVars", max_live_per_blk);
      live = &ls;
    }

    CrabDomain absdom = m_absdom;
    if (absdom == ADAPT_TERMS_ZONES) {
      CRAB_LOG("crabllvm", 
               crab::outs() << "Max live per block: " << max_live_per_blk << "\n"
                            << "Threshold: " << CrabNumThreshold << "\n");
      if (max_live_per_blk > CrabNumThreshold)
        absdom = DIS_INTERVALS;
      else 
        absdom = TERMS_ZONES;
    }

    // -- run invariant generator
    bool change=false;
    switch (absdom) {
      #ifndef SHORTER_COMPILE_TIME
      case INTERVALS_CONGRUENCES: 
        ANALYZE(ric_domain_t, arr_ric_domain_t, *cfg_ptr, F, *live, change);
        break;
      case DIS_INTERVALS:
        ANALYZE(dis_interval_domain_t, arr_dis_interval_domain_t, *cfg_ptr, F, *live, change);
        break;
      case TERMS_INTERVALS:
        ANALYZE(term_int_domain_t, arr_term_int_domain_t, *cfg_ptr, F, *live, change);
        break;
      case ZONES_SPARSE_DBM: 
        ANALYZE(dbm_domain_t, arr_dbm_domain_t, *cfg_ptr, F, *live, change);
        break;
      #endif /* !SHORTER_COMPILE_TIME */
      case TERMS_DIS_INTERVALS:
        ANALYZE(term_dis_int_domain_t, arr_term_dis_int_domain_t, *cfg_ptr, F, *live, change);
        break;
      case ZONES_SPLIT_DBM: 
        ANALYZE(split_dbm_domain_t, arr_split_dbm_domain_t, *cfg_ptr, F, *live, change);
        break;
      case BOXES:
        ANALYZE(boxes_domain_t, arr_boxes_domain_t, *cfg_ptr, F, *live, change);
        break;
      case OPT_OCT_APRON:
        ANALYZE(opt_oct_apron_domain_t, arr_opt_oct_apron_domain_t, *cfg_ptr, F, *live, change);
        break;
      case PK_APRON:
        ANALYZE(pk_apron_domain_t, arr_pk_apron_domain_t, *cfg_ptr, F, *live, change);
        break;
      case TERMS_ZONES:
        ANALYZE(num_domain_t, arr_num_domain_t, *cfg_ptr, F, *live, change);
        break;
      default: 
        if (absdom != INTERVALS) {
          crab::outs() << "Warning: abstract domain not found.\n"
                       << "If you think the domain should be found "
                       << "make sure SHORTER_COMPILE_TIME is disabled.\n"
                       << "Running intervals ...\n"; 
        }
        ANALYZE(interval_domain_t, arr_interval_domain_t, *cfg_ptr, F, *live, change);
    }
    
    return change;
  }

  template<typename BUDom, typename TDDom>
  inline bool CrabLlvm::analyzeCg (call_graph_ref_t cg, const llvm::Module &M,
                                   const liveness_map_t& live_map)
  {
    // -- run inter-procedural analysis on the whole call graph
    typedef inter_fwd_analyzer<call_graph_ref_t, VariableFactory,  BUDom, TDDom> 
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
    
    inter_analyzer_t analyzer(cg, m_vfac, (CrabLive ? &live_map : nullptr),
                              CrabWideningDelay, 
                              CrabNarrowingIters, 
                              CrabWideningJumpSet);
    analyzer.Run (TDDom::top ());

    CRAB_LOG("crabllvm", crab::outs() << "DONE\n");

    // -- store invariants     
    for (auto &n: boost::make_iterator_range (vertices (cg))) {
      cfg_ref_t cfg = n.get_cfg ();
      boost::optional<const llvm::Value *> v = n.name ().get ();
      if (v) {
        if (const llvm::Function *F = dyn_cast<llvm::Function> (*v)) {
          for (auto &B : *F) {
            // --- invariants that hold at the entry of the blocks
            auto pre = analyzer.get_pre (cfg, &B);
            m_pre_map.insert (make_pair (&B, mkGenericAbsDomWrapper (pre)));
            // --- invariants that hold at the exit of the blocks
            auto post = analyzer.get_post (cfg, &B);
            m_post_map.insert (make_pair (&B, mkGenericAbsDomWrapper (post)));
          }

          // --- print invariants and summaries
          // Summaries are not currently stored but it would be easy to do so.
          if (CrabPrintAns) 
            writeInvariants (llvm::outs (), *F);
          
          if (CrabPrintSumm && analyzer.has_summary (cfg)) {
            auto summ = analyzer.get_summary (cfg);
            crab::outs () << "SUMMARY " << F->getName () << ": " << summ << "\n";
          }
        }
      }
    }

    // --- checking assertions
    if (CrabAssertCheck) {
      CRAB_LOG("crabllvm", crab::outs() << "Checking assertions ... \n"); 
      typename inter_checker_t::prop_checker_ptr prop(new assert_prop_t(CrabCheckVerbose));
      if (CrabAssertCheck == NULLITY)
        prop.reset (new null_prop_t(CrabCheckVerbose));      
      inter_checker_t checker (analyzer, {prop});
      checker.run ();
      checker.show (crab::outs());
      m_checks_db = boost::make_shared<checks_db_t>();
      (*m_checks_db) += checker.get_all_checks();
      // if (is_unsafe()) {
      //   crab::outs () << "\nBRUNCH_STAT Result FALSE\n";
      // } else {
      //   crab::outs () << "\nBRUNCH_STAT Result TRUE\n";
      // } 
      CRAB_LOG("crabllvm", crab::outs() << "DONE!\n"); 
    }

    return false;
  }

  template<typename Dom>
  inline bool CrabLlvm::analyzeCfg (cfg_ref_t cfg, const Function&F, const liveness_t& live) {

    typedef typename num_fwd_analyzer<cfg_ref_t,Dom,VariableFactory>::type intra_analyzer_t;
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
    intra_analyzer_t analyzer (cfg, m_vfac, &live, 
                               CrabWideningDelay, CrabNarrowingIters, 
                               CrabWideningJumpSet);
    
    analyzer.Run (Dom::top());

    CRAB_LOG("crabllvm", crab::outs() << "DONE\n"); 

    // -- store invariants 
    for (auto const &B : F) {
      // --- invariants that hold at the entry of the blocks
      auto pre = analyzer.get_pre (&B);
      m_pre_map.insert (make_pair (&B, mkGenericAbsDomWrapper (pre)));
      // --- invariants that hold at the exit of the blocks
      auto post = analyzer.get_post (&B);
      m_post_map.insert (make_pair (&B, mkGenericAbsDomWrapper (post)));
    }

    if (CrabPrintAns)
      writeInvariants (llvm::outs (), F);

    if (CrabAssertCheck) {
      // --- checking assertions
      CRAB_LOG("crabllvm", crab::outs() << "Checking assertions ... \n"); 
      typename intra_checker_t::prop_checker_ptr prop (new assert_prop_t (CrabCheckVerbose));
      if (CrabAssertCheck == NULLITY)
        prop.reset (new null_prop_t(CrabCheckVerbose));
      intra_checker_t checker (analyzer, {prop});
      checker.run ();
      checker.show (crab::outs());
      m_checks_db = boost::make_shared<checks_db_t>();
      (*m_checks_db) += checker.get_all_checks();
      // if (is_unsafe()) {
      //   crab::outs () << "\nBRUNCH_STAT Result FALSE\n";
      // } else {
      //   crab::outs () << "\nBRUNCH_STAT Result TRUE\n";
      // } 
      CRAB_LOG("crabllvm", crab::outs() << "DONE!\n"); 
    }

    return false;
  }


  // return invariants that hold at the entry of BB
  GenericAbsDomWrapperPtr 
  CrabLlvm::getPre (const llvm::BasicBlock *BB, bool keep_shadows) const {
    const_iterator it = m_pre_map.find (BB);
    assert (it != m_pre_map.end ());
    if (keep_shadows)
      return it->second;
    else {
      vector<varname_t> shadows (m_vfac.get_shadow_vars ().begin (),
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
      vector<varname_t> shadows (m_vfac.get_shadow_vars ().begin (),
                                 m_vfac.get_shadow_vars ().end ());
      // make a copy before projecting shadow variables out
      GenericAbsDomWrapperPtr invs = it->second->clone();
      invs->forget (shadows); 
      return invs;
    }
  }


  void CrabLlvm::writeInvariants (llvm::raw_ostream& o, 
                                  const llvm::Function& F) {

    if (!F.isDeclaration () && !F.empty () && !F.isVarArg ()) {
      o << "\nFunction " << F.getName () << "\n";
      for (auto &B : F) {
        const llvm::BasicBlock * BB = &B;
        o << "\t" << BB->getName () << ": ";
        auto pre = getPre (BB, KeepShadows);
        auto post = getPost (BB, KeepShadows);
        o << pre << " ==> " << post << "\n";
      }
      o <<  "\n";
    }
  }

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

  bool CrabLlvm::is_safe () const {
    return !is_unsafe();
  }

  bool CrabLlvm::is_unsafe () const {
    return (get_total_warning_checks() > 0 || get_total_error_checks() > 0);
  }

  void CrabLlvm::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();

    #ifdef HAVE_DSA
    AU.addRequiredTransitive<llvm::SteensgaardDataStructures> ();
    #endif 
    AU.addRequired<llvm::DataLayoutPass>();
    AU.addRequired<llvm::TargetLibraryInfo>();
    AU.addRequired<llvm::UnifyFunctionExitNodes> ();
    AU.addRequired<crab_llvm::NameValues>();
  } 

} // end namespace 

static llvm::RegisterPass<crab_llvm::CrabLlvm> 
X ("crab-llvm", "Infer invariants using Crab", false, false);
  
   


