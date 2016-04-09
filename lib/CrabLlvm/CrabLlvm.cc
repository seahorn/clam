#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "crab_llvm/config.h"
#include "crab_llvm/AbstractDomains.hh"
#include "crab_llvm/CrabLlvm.hh"
#include "crab_llvm/SymEval.hh"
#include "crab_llvm/Support/NameValues.hh"

#include "crab/common/debug.hpp"
#include "crab/common/stats.hpp"
#include "crab/analysis/FwdAnalyzer.hpp"
#include "crab/analysis/InterFwdAnalyzer.hpp"
#include "crab/cg/CgBgl.hpp"

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;
using namespace crab_llvm;

// FIXME: template instantiation takes really long time ... so we
// don't include all the domains by default
//#define INCLUDE_ALL_DOMAINS

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
                clEnumValN (ZONES_DENSE_DBM, "zones-dense",
                            "Zones domain with dense DBMs"),
                clEnumValN (ZONES_DENSE_PACK_DBM, "zones-dense-pack",
                            "Zones domain with dense DBMs and dynamic variable packing"),
                clEnumValN (INTV_APRON, "int-apron",
                            "Intervals domain using Apron library"),
                clEnumValN (OCT_APRON, "oct-apron",
                            "Octagons domain using Apron library"),
                clEnumValN (OPT_OCT_APRON, "opt-oct-apron",
                            "Optimized octagons domain using Elina"),
                clEnumValN (PK_APRON, "pk-apron",
                            "Polyhedra domain using Apron library"),
                clEnumValN (NUM, "num",
                            "Choose between int and reduced product of term-dis-int with zones-split."),
                clEnumValEnd),
               llvm::cl::init (INTERVALS));

// If domain is num
llvm::cl::opt<unsigned>
CrabNumThreshold("crab-dom-num-max-live", 
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
        clEnumValN (ZONES_DENSE_DBM, "zones-dense",
                    "Zones domain with dense DBMs"),
        clEnumValN (ZONES_DENSE_PACK_DBM, "zones-dense-pack",
                    "Zones domain with dense DBMs and dynamic variable packing"),
        clEnumValN (OPT_OCT_APRON, "opt-oct-apron",
                    "Optimized octagons using Elina"),
        clEnumValN (TERMS_INTERVALS, "term-int",
                    "Intervals with uninterpreted functions."),
        clEnumValN (TERMS_DIS_INTERVALS, "term-dis-int",
                    "Disjunctive Intervals with uninterpreted functions."),
        clEnumValN (NUM, "num",
                    "Reduced product of term-dis-int and zones-split"),
        clEnumValEnd),
       llvm::cl::init (ZONES_SPLIT_DBM));

llvm::cl::opt<enum TrackedPrecision>
CrabTrackLev("crab-track",
   llvm::cl::desc ("Track abstraction level of the Crab Cfg"),
   cl::values (clEnumValN (INT, "int", "Integer registers only"),
               clEnumValN (PTR, "ptr", "Integers + pointer offsets"),
               clEnumValN (ARR, "arr", "Integers + offsets + memory contents via array abstraction"),
               clEnumValEnd),
   cl::init (TrackedPrecision::INT));

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
  using namespace crab::cg;

  char crab_llvm::CrabLlvm::ID = 0;

  #define ANALYZE(TRACK,BASE_DOM,ARR_DOM,CFG,F,LIVE,RES)         \
  RES = (TRACK == ARR ? analyzeCfg <ARR_DOM> (CFG, F, LIVE) :    \
                        analyzeCfg <BASE_DOM> (CFG, F, LIVE));

  #ifdef INCLUDE_ALL_DOMAINS
  #define INTER_ANALYZE(SUMM_DOM,BASE_DOM,ARR_DOM,TRACK,CG,M,LIVE,RES)     \
  switch (SUMM_DOM){                                                       \
    case TERMS_INTERVALS:                                                  \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_term_int_domain_t, ARR_DOM> (CG, LIVE, M) :      \
           analyzeCg <term_int_domain_t, BASE_DOM> (CG, LIVE, M)) ;        \
      break;                                                               \
    case TERMS_DIS_INTERVALS:                                              \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_term_dis_int_domain_t, ARR_DOM> (CG, LIVE, M) :  \
           analyzeCg <term_dis_int_domain_t, BASE_DOM> (CG, LIVE, M)) ;    \
      break;                                                               \
    case ZONES_DENSE_PACK_DBM:                                             \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_dp_dbm_domain_t, ARR_DOM> (CG, LIVE, M) :        \
           analyzeCg <dp_dbm_domain_t, BASE_DOM> (CG, LIVE, M)) ;          \
      break;                                                               \
    case ZONES_DENSE_DBM:                                                  \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_d_dbm_domain_t, ARR_DOM> (CG, LIVE, M) :         \
           analyzeCg <d_dbm_domain_t, BASE_DOM> (CG, LIVE, M)) ;           \
      break;                                                               \
    case ZONES_SPARSE_DBM:                                                 \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_dbm_domain_t, ARR_DOM> (CG, LIVE, M) :           \
           analyzeCg <dbm_domain_t, BASE_DOM> (CG, LIVE, M)) ;             \
      break;                                                               \
    case OPT_OCT_APRON:                                                    \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_opt_oct_apron_domain_t, ARR_DOM> (CG, LIVE, M) : \
           analyzeCg <opt_oct_apron_domain_t, BASE_DOM> (CG, LIVE, M)) ;   \
      break;                                                               \
    case NUM:                                                              \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_num_domain_t, ARR_DOM> (CG, LIVE, M) :           \
           analyzeCg <num_domain_t, BASE_DOM> (CG, LIVE, M)) ;             \
      break;                                                               \
    default:                                                               \
      if (SUMM_DOM != ZONES_SPLIT_DBM)                                     \
        cout << "Warning: choosing zones-split to compute summaries\n";    \
      RES = (TRACK == ARR ?                                                \
           analyzeCg <arr_split_dbm_domain_t, ARR_DOM> (CG, LIVE, M) :     \
           analyzeCg <split_dbm_domain_t, BASE_DOM> (CG, LIVE, M)) ; }             
  #else
  // Here we only allow to use opt-oct, zones-split or num
  #define INTER_ANALYZE(SUMM_DOM,BASE_DOM,ARR_DOM,TRACK,CG,M,LIVE,RES)       \
  switch (SUMM_DOM){                                                         \
    case OPT_OCT_APRON:                                                      \
      RES = (TRACK == ARR ?                                                  \
           analyzeCg <arr_opt_oct_apron_domain_t, ARR_DOM> (CG, LIVE, M) :   \
           analyzeCg <opt_oct_apron_domain_t, BASE_DOM> (CG, LIVE, M)) ;     \
      break;                                                                 \
    case NUM:                                                                \
      RES = (TRACK == ARR ?                                                  \
           analyzeCg <arr_num_domain_t, ARR_DOM> (CG, LIVE, M) :             \
           analyzeCg <num_domain_t, BASE_DOM> (CG, LIVE, M)) ;               \
      break;                                                                 \
    default:                                                                 \
      if (SUMM_DOM != ZONES_SPLIT_DBM)                                       \
        cout << "Warning: choosing zones-split to compute summaries\n";      \
      RES = (TRACK == ARR ?                                                  \
           analyzeCg <arr_split_dbm_domain_t, ARR_DOM> (CG, LIVE, M) :       \
           analyzeCg <split_dbm_domain_t, BASE_DOM> (CG, LIVE, M)) ; }
  #endif  


  bool CrabLlvm::runOnModule (llvm::Module &M) {
    // -- initialize from cli options
    m_absdom = CrabLlvmDomain;

    #ifdef HAVE_DSA
    m_mem.reset (new DSAMemAnalysis (M, &getAnalysis<SteensgaardDataStructures> (),
                                     CrabTrackLev));
    #endif     

    if (m_absdom == NUM) {
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
             cout << "Total number of analyzed functions:" 
                   << num_analyzed_funcs << "\n";);

    if (CrabInter){

      vector<cfg_t> cfgs;
      liveness_map_t live_map;
      unsigned max_live_per_blk = 0;
      for (auto &F : M) {
        // -- skip functions without a body
        if (F.isDeclaration () || F.empty ()) continue;
        // -- skip variadic functions
        if (F.isVarArg ()) continue;

        // -- build cfg
        CfgBuilderPtr builder (new CfgBuilder (F, m_vfac, *m_mem, 
                                               /*include function decls and callsites*/
                                               true));
        m_builder_map [&F] = builder;
        cfg_t& cfg = builder->getCfg ();
        cfgs.push_back (cfg);

        // -- build liveness
        if (CrabLive) {
          CRAB_LOG("crabllvm",
                   auto fdecl = cfg.get_func_decl ();            
                   assert (fdecl);
                   cout << "Running liveness analysis for " 
                        << (*fdecl).get_func_name () << "  ... ";);

          liveness_t* live = new liveness_t (cfg);
          live->exec ();
          CRAB_LOG("crabllvm", cout << "DONE!\n";);
          // some stats
          unsigned total_live, max_live_per_blk_, avg_live_per_blk;
          live->get_stats (total_live, max_live_per_blk_, avg_live_per_blk);
          max_live_per_blk = std::max (max_live_per_blk, max_live_per_blk_);
          CRAB_LOG("crabllvm",
                   cout << "-- Max number of out live vars per block=" 
                        << max_live_per_blk_ << "\n";
                   cout << "-- Avg number of out live vars per block=" 
                        << avg_live_per_blk << "\n";);
          crab::CrabStats::count_max ("Liveness.count.maxOutVars", max_live_per_blk);

          live_map.insert (make_pair (cfg, live));
        }
      }

      // -- build call graph

      CallGraph<cfg_t> cg (cfgs);

      // -- run the interprocedural analysis

      // FIXME: the selection of the final domain between NUM and a
      //        cheaper domain is fixed for the whole program. That
      //        is, if there is one function that exceeds the
      //        threshold then the cheaper domain will be used for all
      //        functions. We should be able to change from one
      //        function to another.
      CrabDomain absdom = m_absdom;
      if (absdom == NUM) {
        CRAB_LOG("crabllvm",
                 cout << "Max live per block: " << max_live_per_blk << endl;
                 cout << "Threshold: " << CrabNumThreshold << endl);
        if (max_live_per_blk > CrabNumThreshold) {
          absdom = DIS_INTERVALS;
        }
      }
            
      bool change = false;
      switch (absdom) {
        case INTERVALS_CONGRUENCES: 
          INTER_ANALYZE (CrabSummDomain,ric_domain_t,arr_ric_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case DIS_INTERVALS:
          INTER_ANALYZE (CrabSummDomain,dis_interval_domain_t,arr_dis_interval_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case ZONES_SPLIT_DBM: 
          INTER_ANALYZE (CrabSummDomain,split_dbm_domain_t,arr_split_dbm_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        #ifdef INCLUDE_ALL_DOMAINS
        case ZONES_SPARSE_DBM: 
          INTER_ANALYZE (CrabSummDomain,dbm_domain_t,arr_dbm_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case ZONES_DENSE_DBM: 
          INTER_ANALYZE (CrabSummDomain,d_dbm_domain_t,arr_d_dbm_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case ZONES_DENSE_PACK_DBM: 
          INTER_ANALYZE (CrabSummDomain,dp_dbm_domain_t,arr_dp_dbm_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        #endif 
        case TERMS_INTERVALS:
          INTER_ANALYZE (CrabSummDomain,term_int_domain_t,arr_term_int_domain_t, 
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case TERMS_DIS_INTERVALS:
          INTER_ANALYZE (CrabSummDomain,term_dis_int_domain_t,arr_term_dis_int_domain_t, 
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        #ifdef INCLUDE_ALL_DOMAINS
        case OCT_APRON:
          INTER_ANALYZE (CrabSummDomain,oct_apron_domain_t,arr_oct_apron_domain_t, 
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        #endif 
        case OPT_OCT_APRON:
          INTER_ANALYZE (CrabSummDomain,opt_oct_apron_domain_t,arr_opt_oct_apron_domain_t, 
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case PK_APRON:
          INTER_ANALYZE (CrabSummDomain,pk_apron_domain_t,arr_pk_apron_domain_t, 
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case NUM: 
          INTER_ANALYZE (CrabSummDomain,num_domain_t,arr_num_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        default: 
          if (absdom != INTERVALS)
            cout << "Warning: either abstract domain not found or "
                 << "inter-procedural version not implemented. "
                 << "Running intervals ...\n"; 
          INTER_ANALYZE (CrabSummDomain,interval_domain_t,arr_interval_domain_t,
                         CrabTrackLev,cg,M,live_map,change);
      }

      if (CrabStats) {
        std::ostringstream oss;
        crab::CrabStats::PrintBrunch (oss);
        outs () << oss.str();
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
        std::ostringstream oss;
        crab::CrabStats::PrintBrunch (oss);
        outs () << oss.str();
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
    CfgBuilderPtr builder (new CfgBuilder (F, m_vfac, *m_mem, 
                                           /*include function decls and callsites*/
                                           true));
    m_builder_map [&F] = builder;
    cfg_t& cfg = builder->getCfg ();

    // -- run liveness
    liveness_t* live = nullptr;
    unsigned max_live_per_blk = 0;
    if (CrabLive) {
      CRAB_LOG("crabllvm",
               auto fdecl = cfg.get_func_decl ();            
               assert (fdecl);
               cout << "Running liveness analysis for " 
                    << (*fdecl).get_func_name ()
                    << "  ... ";);
      liveness_t ls (cfg);
      ls.exec ();
      CRAB_LOG("crabllvm", cout << "DONE!\n");
      // some stats
      unsigned total_live, avg_live_per_blk;
      ls.get_stats (total_live, max_live_per_blk, avg_live_per_blk);
      CRAB_LOG("crabllvm", 
               cout << "-- Max number of out live vars per block=" 
                    << max_live_per_blk << "\n"
                    << "-- Avg number of out live vars per block=" 
                    << avg_live_per_blk << "\n";);
      crab::CrabStats::count_max ("Liveness.count.maxOutVars", max_live_per_blk);
      live = &ls;
    }

    CrabDomain absdom = m_absdom;
    if (absdom == NUM) {
      CRAB_LOG("crabllvm", 
               cout << "Max live per block: " << max_live_per_blk << "\n"
                    << "Threshold: " << CrabNumThreshold << "\n");
      if (max_live_per_blk > CrabNumThreshold) {
        absdom = DIS_INTERVALS;
      }
    }
    
    // -- run invariant generator
    bool change=false;
    switch (absdom) {
      case INTERVALS_CONGRUENCES: 
        ANALYZE(CrabTrackLev, ric_domain_t, arr_ric_domain_t, cfg, F, *live, change);
        break;
      case TERMS_INTERVALS:
        ANALYZE(CrabTrackLev,term_int_domain_t,arr_term_int_domain_t,cfg,F,*live,change);
        break;
      case TERMS_DIS_INTERVALS:
        ANALYZE(CrabTrackLev,term_dis_int_domain_t,arr_term_dis_int_domain_t,cfg,F,*live,change);
        break;
      case ZONES_SPLIT_DBM: 
        ANALYZE(CrabTrackLev,split_dbm_domain_t,arr_split_dbm_domain_t,cfg,F,*live,change);
        break;
      #ifdef INCLUDE_ALL_DOMAINS
      case ZONES_SPARSE_DBM: 
        ANALYZE(CrabTrackLev, dbm_domain_t, arr_dbm_domain_t, cfg, F, *live, change);
        break;
      case ZONES_DENSE_DBM: 
        ANALYZE(CrabTrackLev, d_dbm_domain_t, arr_d_dbm_domain_t, cfg, F, *live, change);
        break;
      case ZONES_DENSE_PACK_DBM: 
        ANALYZE(CrabTrackLev, dp_dbm_domain_t, arr_dp_dbm_domain_t, cfg, F, *live, change);
        break;
      #endif 
      case BOXES:
        ANALYZE(CrabTrackLev,boxes_domain_t,arr_boxes_domain_t,cfg,F,*live,change);
        break;
      case DIS_INTERVALS:
        ANALYZE(CrabTrackLev,dis_interval_domain_t,arr_dis_interval_domain_t,cfg,F,*live,change);
        break;
      #ifdef INCLUDE_ALL_DOMAINS
      case INTV_APRON:
        ANALYZE(CrabTrackLev, box_apron_domain_t, arr_box_apron_domain_t, cfg, F, *live, change);
        break;
      case OCT_APRON:
        ANALYZE(CrabTrackLev, oct_apron_domain_t, arr_oct_apron_domain_t, cfg, F, *live, change);
        break;
      #endif 
      case OPT_OCT_APRON:
        ANALYZE(CrabTrackLev,opt_oct_apron_domain_t,arr_opt_oct_apron_domain_t,cfg,F,*live,change);
        break;
      case PK_APRON:
        ANALYZE(CrabTrackLev,pk_apron_domain_t,arr_pk_apron_domain_t,cfg,F,*live,change);
        break;
      case NUM:
        ANALYZE(CrabTrackLev,num_domain_t,arr_num_domain_t,cfg,F,*live,change);
        break;
      default: 
        if (absdom != INTERVALS)
          cout << "Warning: abstract domain not found. Running intervals ...\n"; 
        ANALYZE(CrabTrackLev,interval_domain_t,arr_interval_domain_t,cfg,F,*live,change);
    }
    
    if (CrabPrintAns) {
      writeInvariants (outs (), F);
    }

    return change;
  }

  template<typename BUAbsDomain, typename TDAbsDomain>
  inline bool CrabLlvm::analyzeCg (const CallGraph<cfg_t>& cg, 
            const liveness_map_t& live_map,
           const llvm::Module &M)
  {
    // -- run inter-procedural analysis on the whole call graph
    typedef InterFwdAnalyzer< CallGraph<cfg_t>, VariableFactory,
                              BUAbsDomain, TDAbsDomain> analyzer_t;

    CRAB_LOG("crabllvm", 
              cout << "Running inter-procedural analysis with " 
                   << "forward domain:" 
                   << "\"" << TDAbsDomain::getDomainName () << "\"";
              cout << " and bottom-up domain:" 
                   << "\"" << BUAbsDomain::getDomainName () << "\"" 
                   << "  ... ";);
    
    analyzer_t analyzer(cg, m_vfac, (CrabLive ? &live_map : nullptr),
                        CrabWideningDelay, CrabNarrowingIters, 
                        CrabWideningJumpSet);
    analyzer.Run (TDAbsDomain::top ());

    CRAB_LOG("crabllvm", cout << "DONE\n");

    // -- store invariants     
    for (auto &n: boost::make_iterator_range (vertices (cg))) {
      const cfg_t& cfg = n.getCfg ();
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

          // -- print invariants and summaries
          // Summaries are not currently stored but it would be easy to do so.
          if (CrabPrintAns)
            writeInvariants (outs (), *F);

          if (CrabPrintSumm && analyzer.has_summary (cfg)) {
            auto summ = analyzer.get_summary (cfg);
            outs () << "SUMMARY " << F->getName () << ": " << summ << "\n";
          }
        }
      }
    }
    return false;
  }

  template<typename AbsDomain>
  inline bool CrabLlvm::analyzeCfg (cfg_t& cfg, 
                                    const Function&F, 
                                    const liveness_t& live) {

    typedef typename NumFwdAnalyzer <cfg_t, 
                                     AbsDomain, 
                                     VariableFactory>::type analyzer_t;
    CRAB_LOG("crabllvm",
             auto fdecl = cfg.get_func_decl ();            
             assert (fdecl);
             cout << "Running intra-procedural analysis with " 
                  << "\"" << AbsDomain::getDomainName ()  << "\""
                  << " for "  << (*fdecl).get_func_name ()
                  << "  ... ";);

    // -- run intra-procedural analysis
    analyzer_t analyzer (cfg, m_vfac, &live, 
                         CrabWideningDelay, CrabNarrowingIters, 
                         CrabWideningJumpSet);

    analyzer.Run (AbsDomain::top());

    CRAB_LOG("crabllvm", cout << "DONE\n"); 

    // -- store invariants 
    for (auto const &B : F) {
      // --- invariants that hold at the entry of the blocks
      auto pre = analyzer.get_pre (&B);
      m_pre_map.insert (make_pair (&B, mkGenericAbsDomWrapper (pre)));
      // --- invariants that hold at the exit of the blocks
      auto post = analyzer.get_post (&B);
      m_post_map.insert (make_pair (&B, mkGenericAbsDomWrapper (post)));
    }
    
    return false;
  }


  // return invariants that hold at the entry of BB
  GenericAbsDomWrapperPtr 
  CrabLlvm::getPre (const llvm::BasicBlock *BB, bool KeepShadows) const {
    const_iterator it = m_pre_map.find (BB);
    assert (it != m_pre_map.end ());
    if (KeepShadows)
      return it->second;
    else {
      vector<varname_t> shadows (m_vfac.get_shadow_vars ().begin (),
                                 m_vfac.get_shadow_vars ().end ());
      it->second->forget (shadows); 
      return it->second;
    }
  }   

  // return invariants that hold at the exit of BB
  GenericAbsDomWrapperPtr 
  CrabLlvm::getPost (const llvm::BasicBlock *BB, bool KeepShadows) const {
    const_iterator it = m_post_map.find (BB);
    assert (it != m_post_map.end ());
    if (KeepShadows)
      return it->second;
    else {
      vector<varname_t> shadows (m_vfac.get_shadow_vars ().begin (),
                                 m_vfac.get_shadow_vars ().end ());
      it->second->forget (shadows); 
      return it->second;
    }
  }


  void CrabLlvm::writeInvariants (llvm::raw_ostream& o, 
                                  const llvm::Function& F) {

    if (!F.isDeclaration () && !F.empty () && !F.isVarArg ()) {
      o << "\nFunction " << F.getName () << "\n";
      for (auto &B : F) {
        const llvm::BasicBlock * BB = &B;
        o << "\t" << BB->getName () << ": ";
        auto inv = getPost (BB, KeepShadows);
        o << inv << "\n";
      }
      o <<  "\n";
    }
  }

  void CrabLlvm::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();

    #ifdef HAVE_DSA
    AU.addRequiredTransitive<llvm::SteensgaardDataStructures> ();
    #endif 
    AU.addRequired<llvm::DataLayoutPass>();
    AU.addRequired<llvm::UnifyFunctionExitNodes> ();
    AU.addRequired<crab_llvm::NameValues>();
  } 

} // end namespace 

static llvm::RegisterPass<crab_llvm::CrabLlvm> 
X ("crab-llvm",
   "Infer invariants using Crab", 
   false, 
   false);
   


