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

#include "crab/domains/domain_traits.hpp"
#include "crab/analysis/FwdAnalyzer.hpp"
#include "crab/analysis/InterFwdAnalyzer.hpp"
#include "crab/cg/CgBgl.hpp"

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;
using namespace crab_llvm;

// for debugging
#define CRABLLVM_DEBUG
// for stats
#define CRABLLVM_STATS

llvm::cl::opt<bool>
CrabPrintAns ("crab-print-invariants", 
              llvm::cl::desc ("Print Crab invariants"),
              llvm::cl::init (false));

llvm::cl::opt<bool>
CrabPrintSumm ("crab-print-summaries", 
               llvm::cl::desc ("Print Crab function summaries"),
               llvm::cl::init (false));

llvm::cl::opt<unsigned int>
CrabWideningThreshold("crab-widening-threshold", 
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
                clEnumValN (TERMS, "term",
                            "Intervals with uninterpreted functions."),
                clEnumValN (BOXES, "boxes",
                            "Disjunctive intervals based on ldds"),
                clEnumValN (DIS_INTERVALS, "dis-int",
                            "Disjunctive intervals based on disjunction completion"),
                clEnumValN (NUM, "num",
                            "Choose automatically the numerical abstract domain."),
                clEnumValN (ZONES , "zones",
                            "Sparse Difference-Bounds Matrix (or Zones) domain"),
                clEnumValN (SZONES, "szones",
                            "Split Difference-Bounds Matrix domain"),
                clEnumValN (DZONES, "dzones",
                            "Dense Difference-Bounds Matrix"),
                clEnumValN (VZONES, "vzones",
                            "Dense Difference-Bounds Matrix with variable packing"),
                clEnumValN (INTV_APRON, "int-apron",
                            "Intervals using Apron library"),
                clEnumValN (OCT_APRON, "oct-apron",
                            "Octagons using Apron library"),
                clEnumValN (OPT_OCT_APRON, "opt-oct-apron",
                            "Optimized octagons using Elina"),
                clEnumValN (PK_APRON, "pk-apron",
                            "New polka using Apron library"),
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

// if crab-inter enabled. It does not make much sense to have
// non-relational domains.
llvm::cl::opt<CrabDomain>
CrabSummDomain("crab-inter-sum-dom",
       llvm::cl::desc ("Crab abstract domain used to generate function summaries"),
       llvm::cl::values 
       (clEnumValN (ZONES, "zones",
                    "Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (TERMS, "term",
                    "Intervals with uninterpreted functions."),
        clEnumValN (ZONES , "zones",
                    "Sparse Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (SZONES, "szones",
                    "Split Zones domain"),
        clEnumValN (VZONES, "vzones",
                    "Dense Zones with variable packing"),
        clEnumValN (OPT_OCT_APRON, "opt-oct-apron",
                    "Optimized octagons using Elina"),
        clEnumValEnd),
       llvm::cl::init (ZONES));

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

    #ifdef CRABLLVM_DEBUG
    unsigned num_analyzed_funcs = 0;
    for (auto &F : M) {
      if (F.isDeclaration () || F.empty ()) continue;
      if (F.isVarArg ()) continue;
      num_analyzed_funcs++;
    }
    std::cout << "Total number of analyzed functions:" 
              << num_analyzed_funcs << "\n";
    std::cout.flush ();
    #endif 

    if (CrabInter){

      std::vector<cfg_t> cfgs;
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
          #ifdef CRABLLVM_DEBUG
          auto fdecl = cfg.get_func_decl ();            
          assert (fdecl);
          std::cout << "Running liveness analysis for " 
                    << (*fdecl).get_func_name ()
                    << "  ... ";
          std::cout.flush ();
          #endif 
          liveness_t* live = new liveness_t (cfg);
          live->exec ();
          #ifdef CRABLLVM_DEBUG
          std::cout << "DONE!\n";
          std::cout.flush ();
          #endif 
          // some stats
          unsigned total_live, max_live_per_blk_, avg_live_per_blk;
          live->get_stats (total_live, max_live_per_blk_, avg_live_per_blk);
          max_live_per_blk = std::max (max_live_per_blk, max_live_per_blk_);
          #ifdef CRABLLVM_DEBUG
          std::cout << "-- Max number of out live vars per block=" 
                    << max_live_per_blk_ << "\n";
          std::cout << "-- Avg number of out live vars per block=" 
                    << avg_live_per_blk << "\n";
          std::cout.flush ();
          #endif 
          #ifdef CRABLLVM_STATS
          std::cerr << "BRUNCH_STAT maxlive " << max_live_per_blk << "\n";
          #endif 

          live_map.insert (make_pair (cfg, live));
        }
      }

      // -- build call graph

      CallGraph<cfg_t> cg (cfgs);

      // -- run the interprocedural analysis
      
      if (m_absdom == NUM) {
        #ifdef CRABLLVM_DEBUG
        cout << "Max live per block: " << max_live_per_blk << endl;
        cout << "Threshold: " << CrabNumThreshold << endl;
        #endif               
        if (max_live_per_blk < CrabNumThreshold) {
          #ifdef CRABLLVM_DEBUG
          std::cout << "Choosen automatically zones. \n";
          #endif 
          m_absdom = ZONES;
        }
        else {
          #ifdef CRABLLVM_DEBUG
          std::cout << "Choosen automatically intervals. \n";
          #endif 
          m_absdom = INTERVALS;
        }
      }
            
      bool change = false;
      switch (m_absdom) {
        case INTERVALS_CONGRUENCES: 
          switch (CrabSummDomain) {
            case TERMS:
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_term_domain_t, arr_ric_domain_t> (cg, live_map, M) : 
                        runOnCg <term_domain_t, ric_domain_t> (cg, live_map, M)) ; 
              break;
            case VZONES: // temporary
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_vdbm_domain_t, arr_ric_domain_t> (cg, live_map, M) : 
                        runOnCg <vdbm_domain_t, ric_domain_t> (cg, live_map, M)) ; 
              break;
            case ZONES: // temporary
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_dbm_domain_t, arr_ric_domain_t> (cg, live_map, M) : 
                        runOnCg <dbm_domain_t, ric_domain_t> (cg, live_map, M)) ; 
              break;
            case OPT_OCT_APRON: 
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_opt_oct_apron_domain_t, arr_ric_domain_t> (cg, live_map, M) : 
                        runOnCg <opt_oct_apron_domain_t, ric_domain_t> (cg, live_map, M)) ; 
              break;
            default:    // szones  
              if (CrabSummDomain != SZONES)            
                std::cerr << "Warning: choosing split zones to compute summaries\n";
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_sdbm_domain_t, arr_ric_domain_t> (cg, live_map, M) : 
                        runOnCg <sdbm_domain_t, ric_domain_t> (cg, live_map, M)) ; 
          }          
          break;
        case ZONES: 
          if (CrabSummDomain != ZONES)
            std::cerr << "Warning: choosing zones to compute summaries\n";
          change = (CrabTrackLev == ARR ? 
                    runOnCg <arr_dbm_domain_t, arr_dbm_domain_t> (cg, live_map, M) :  
                    runOnCg <dbm_domain_t, dbm_domain_t> (cg, live_map, M)) ; 
          break;
        case SZONES: 
          if (CrabSummDomain != SZONES)
            std::cerr << "Warning: choosing szones to compute summaries\n";
          change = (CrabTrackLev == ARR ? 
                    runOnCg <arr_sdbm_domain_t, arr_sdbm_domain_t> (cg, live_map, M) :  
                    runOnCg <sdbm_domain_t, sdbm_domain_t> (cg, live_map, M)) ; 
          break;
        case DZONES: 
          if (CrabSummDomain != DZONES)
            std::cerr << "Warning: choosing dzones to compute summaries\n";
          change = (CrabTrackLev == ARR ? 
                    runOnCg <arr_ddbm_domain_t, arr_ddbm_domain_t> (cg, live_map, M) :  
                    runOnCg <ddbm_domain_t, ddbm_domain_t> (cg, live_map, M)) ; 
          break;
        case VZONES: 
          if (CrabSummDomain != VZONES)
            std::cerr << "Warning: choosing vzones to compute summaries\n";
          change = (CrabTrackLev == ARR ? 
                    runOnCg <arr_vdbm_domain_t, arr_vdbm_domain_t> (cg, live_map, M) :  
                    runOnCg <vdbm_domain_t, vdbm_domain_t> (cg, live_map, M)) ; 
          break;
        case TERMS:
          switch (CrabSummDomain){
            case TERMS:
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_term_domain_t, arr_term_domain_t> (cg, live_map, M) : 
                        runOnCg <term_domain_t, term_domain_t> (cg, live_map, M)) ; 
              break;
            case VZONES: // temporary
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_vdbm_domain_t, arr_term_domain_t> (cg, live_map, M) : 
                        runOnCg <vdbm_domain_t, term_domain_t> (cg, live_map, M)) ; 
              break;
            case ZONES: // temporary
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_dbm_domain_t, arr_term_domain_t> (cg, live_map, M) : 
                        runOnCg <dbm_domain_t, term_domain_t> (cg, live_map, M)) ; 
              break;
            case OPT_OCT_APRON: 
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_opt_oct_apron_domain_t, arr_term_domain_t> (cg, live_map, M) : 
                        runOnCg <opt_oct_apron_domain_t, term_domain_t> (cg, live_map, M)) ; 
              break;
            default:    // szones  
              if (CrabSummDomain != SZONES)                        
                std::cerr << "Warning: choosing split zones to compute summaries\n";
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_sdbm_domain_t, arr_term_domain_t> (cg, live_map, M) : 
                        runOnCg <sdbm_domain_t, term_domain_t> (cg, live_map, M)) ; 
          }
          break;
        case OCT_APRON:
          if (CrabSummDomain != OCT_APRON)
            std::cerr << "Warning: choosing oct-apron to compute summaries\n";
          change = (CrabTrackLev == ARR ? 
                    runOnCg <arr_oct_apron_domain_t, arr_oct_apron_domain_t> (cg, live_map, M) : 
                    runOnCg <oct_apron_domain_t, oct_apron_domain_t> (cg, live_map, M)) ; 
          break;
        case OPT_OCT_APRON:
          if (CrabSummDomain != OPT_OCT_APRON)
            std::cerr << "Warning: choosing opt-oct-apron to compute summaries\n";
          change = (CrabTrackLev == ARR ? 
                    runOnCg <arr_opt_oct_apron_domain_t, arr_opt_oct_apron_domain_t> (cg, live_map, M) : 
                    runOnCg <opt_oct_apron_domain_t, opt_oct_apron_domain_t> (cg, live_map, M)) ; 
          break;
        case PK_APRON:
          if (CrabSummDomain != PK_APRON)
            std::cerr << "Warning: choosing pk-apron to compute summaries\n";
          change = (CrabTrackLev == ARR ? 
                    runOnCg <arr_pk_apron_domain_t, arr_pk_apron_domain_t> (cg, live_map, M) : 
                    runOnCg <pk_apron_domain_t, pk_apron_domain_t> (cg, live_map, M)) ; 
          break;
        case DIS_INTERVALS:
          switch (CrabSummDomain){
            case TERMS:
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_term_domain_t, arr_dis_interval_domain_t> (cg, live_map, M) : 
                        runOnCg <term_domain_t, dis_interval_domain_t> (cg, live_map, M)) ; 
              break;
            default:
              if (CrabSummDomain != SZONES)            
                std::cerr << "Warning: choosing split zones to compute summaries\n";
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_sdbm_domain_t, arr_dis_interval_domain_t> (cg, live_map, M) : 
                        runOnCg <sdbm_domain_t, dis_interval_domain_t> (cg, live_map, M)) ; 
          }
          break;
        case INTERVALS:  
        default: 
          if (m_absdom != INTERVALS)
            std::cerr << "Warning: either abstract domain not found or "
                      << "inter-procedural version not implemented. "
                      << "Running intervals inter-procedurally ...\n"; 
          
          switch (CrabSummDomain){
            case TERMS:
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_term_domain_t, arr_interval_domain_t> (cg, live_map, M) : 
                        runOnCg <term_domain_t, interval_domain_t> (cg, live_map, M)) ; 
              break;
            case VZONES: // temporary
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_vdbm_domain_t, arr_interval_domain_t> (cg, live_map, M) : 
                        runOnCg <vdbm_domain_t, interval_domain_t> (cg, live_map, M)) ; 
              break;
            case ZONES: // temporary
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_dbm_domain_t, arr_interval_domain_t> (cg, live_map, M) : 
                        runOnCg <dbm_domain_t, interval_domain_t> (cg, live_map, M)) ; 
              break;
            case OPT_OCT_APRON: 
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_opt_oct_apron_domain_t, arr_interval_domain_t> (cg, live_map, M) : 
                        runOnCg <opt_oct_apron_domain_t, interval_domain_t> (cg, live_map, M)) ; 
              break;
            default:    // szones  
              if (CrabSummDomain != SZONES)                        
                std::cerr << "Warning: choosing split zones to compute summaries\n";
              change = (CrabTrackLev == ARR ? 
                        runOnCg <arr_sdbm_domain_t, arr_interval_domain_t> (cg, live_map, M) : 
                        runOnCg <sdbm_domain_t, interval_domain_t> (cg, live_map, M)) ; 
          }
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
      #ifdef CRABLLVM_DEBUG
      auto fdecl = cfg.get_func_decl ();            
      assert (fdecl);
      std::cout << "Running liveness analysis for " 
                << (*fdecl).get_func_name ()
                << "  ... ";
      std::cout.flush ();
      #endif 
      liveness_t ls (cfg);
      ls.exec ();
      #ifdef CRABLLVM_DEBUG
      std::cout << "DONE!\n";
      std::cout.flush ();
      #endif 
      // some stats
      unsigned total_live, avg_live_per_blk;
      ls.get_stats (total_live, max_live_per_blk, avg_live_per_blk);
      #ifdef CRABLLVM_DEBUG
      std::cout << "-- Max number of out live vars per block=" 
                << max_live_per_blk << "\n";
      std::cout << "-- Avg number of out live vars per block=" 
                << avg_live_per_blk << "\n";
      std::cout.flush ();
      #endif 
      #ifdef CRABLLVM_STATS
      std::cerr << "BRUNCH_STAT maxlive " << max_live_per_blk << "\n";
      #endif 
      live = &ls;
    }

    if (m_absdom == NUM) {
      #ifdef CRABLLVM_DEBUG
      cout << "Max live per block: " << max_live_per_blk << endl;
      cout << "Threshold: " << CrabNumThreshold << endl;
      #endif               
      if (max_live_per_blk < CrabNumThreshold) {
        #ifdef CRABLLVM_DEBUG
        std::cout << "Choosen automatically zones. \n";
        #endif 
        m_absdom = ZONES;
      }
      else {
        #ifdef CRABLLVM_DEBUG
        std::cout << "Choosen automatically intervals. \n";
        #endif
        m_absdom = INTERVALS;
      }
    }
    
    // -- run invariant generator
    bool change=false;
    switch (m_absdom)
    {
      case INTERVALS_CONGRUENCES: 
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_ric_domain_t> (cfg, F, *live) : 
                  runOnCfg <ric_domain_t> (cfg, F, *live)) ; 
        break;
      case ZONES: 
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_dbm_domain_t> (cfg, F, *live) :  
                  runOnCfg <dbm_domain_t> (cfg, F, *live)) ; 
        break;
      case SZONES: 
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_sdbm_domain_t> (cfg, F, *live) :  
                  runOnCfg <sdbm_domain_t> (cfg, F, *live)) ; 
        break;
      case DZONES: 
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_ddbm_domain_t> (cfg, F, *live) :  
                  runOnCfg <ddbm_domain_t> (cfg, F, *live)) ; 
        break;
      case VZONES: 
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_vdbm_domain_t> (cfg, F, *live) :  
                  runOnCfg <vdbm_domain_t> (cfg, F, *live)) ; 
        break;
      case TERMS:
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_term_domain_t> (cfg, F, *live) : 
                  runOnCfg <term_domain_t> (cfg, F, *live)) ; 
        break;
      case BOXES:
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_boxes_domain_t> (cfg, F, *live) : 
                  runOnCfg <boxes_domain_t> (cfg, F, *live)) ; 
        break;
      case DIS_INTERVALS:
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_dis_interval_domain_t> (cfg, F, *live) : 
                  runOnCfg <dis_interval_domain_t> (cfg, F, *live)) ; 
        break;
      case INTV_APRON:
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_box_apron_domain_t> (cfg, F, *live) : 
                  runOnCfg <box_apron_domain_t> (cfg, F, *live)) ; 
        break;
      case OCT_APRON:
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_oct_apron_domain_t> (cfg, F, *live) : 
                  runOnCfg <oct_apron_domain_t> (cfg, F, *live)) ; 
        break;
      case OPT_OCT_APRON:
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_opt_oct_apron_domain_t> (cfg, F, *live) : 
                  runOnCfg <opt_oct_apron_domain_t> (cfg, F, *live)) ; 
        break;
      case PK_APRON:
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_pk_apron_domain_t> (cfg, F, *live) : 
                  runOnCfg <pk_apron_domain_t> (cfg, F, *live)) ; 
        break;
      case INTERVALS:  
      default: 
        if (m_absdom != INTERVALS)
          std::cerr << "Warning: abstract domain not found."
                    << "Running intervals ...\n"; 
        change = (CrabTrackLev == ARR ? 
                  runOnCfg <arr_interval_domain_t> (cfg, F, *live) : 
                  runOnCfg <interval_domain_t> (cfg, F, *live)) ; 
    }
    
    if (CrabPrintAns) {
      writeInvariants (outs (), F);
    }

    return change;
  }

  template<typename BUAbsDomain, typename TDAbsDomain>
  bool CrabLlvm::runOnCg (const CallGraph<cfg_t>& cg, 
                          const liveness_map_t& live_map,
                          const llvm::Module &M)
  {
    // -- run inter-procedural analysis on the whole call graph
    typedef InterFwdAnalyzer< CallGraph<cfg_t>, VariableFactory,
                              BUAbsDomain, TDAbsDomain> analyzer_t;

    #ifdef CRABLLVM_DEBUG
    std::cout << "Running inter-procedural analysis with " 
              << "forward domain:" 
              << "\"" << TDAbsDomain::getDomainName () << "\""
              << " and bottom-up domain:" 
              << "\"" << BUAbsDomain::getDomainName () << "\"" 
              << "  ... ";
    std::cout.flush ();
    #endif 

    analyzer_t analyzer(cg, m_vfac, (CrabLive ? &live_map : nullptr),
                        CrabWideningThreshold, CrabNarrowingIters, 
                        CrabWideningJumpSet);
    analyzer.Run (TDAbsDomain::top ());

    #ifdef CRABLLVM_DEBUG
    std::cout << "DONE\n";
    std::cout.flush ();
    #endif 

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
  bool CrabLlvm::runOnCfg (cfg_t& cfg, const Function&F, const liveness_t& live) {
    typedef typename NumFwdAnalyzer <cfg_t, 
                                     AbsDomain, 
                                     VariableFactory>::type analyzer_t;

    #ifdef CRABLLVM_DEBUG
    auto fdecl = cfg.get_func_decl ();            
    assert (fdecl);
    std::cout << "Running intra-procedural analysis with " 
              << "\"" << AbsDomain::getDomainName ()  << "\""
              << " for "  << (*fdecl).get_func_name ()
              << "  ... ";
    std::cout.flush ();
    #endif 

    // -- run intra-procedural analysis
    analyzer_t analyzer (cfg, m_vfac, &live, 
                         CrabWideningThreshold, CrabNarrowingIters, CrabWideningJumpSet);

    analyzer.Run (AbsDomain::top());

    #ifdef CRABLLVM_DEBUG
    std::cout << "DONE\n"; 
    std::cout.flush ();
    #endif 

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
      auto shadows = m_vfac.get_shadow_vars ();
      return forget (it->second, shadows);
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
      auto shadows = m_vfac.get_shadow_vars ();
      return forget (it->second, shadows);
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
  } 

} // end namespace 

static llvm::RegisterPass<crab_llvm::CrabLlvm> 
X ("crab-llvm",
   "Infer invariants using Crab", 
   false, 
   false);
   


