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
        clEnumValN (ZONES , "zones",
                    "Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (BOXES, "boxes",
                    "Disjunctive intervals based on ldds"),
        clEnumValN (DIS_INTERVALS, "dis-int",
                    "Disjunctive intervals based on disjunction completion"),
        clEnumValN (TERMS_INTERVALS, "term-int",
                    "Intervals with uninterpreted functions."),
        clEnumValN (TERMS_DIS_INTERVALS, "term-dis-int",
                    "Disjunctive Intervals with uninterpreted functions."),
        clEnumValN (NUM, "num",
                    "Choose automatically the numerical abstract domain."),
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
        clEnumValN (TERMS_INTERVALS, "term-int",
                    "Intervals with uninterpreted functions."),
        clEnumValN (TERMS_DIS_INTERVALS, "term-dis-int",
                    "Disjunctive Intervals with uninterpreted functions."),
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

  #define ANALYZE(TRACK,BASE_DOM,ARR_DOM,CFG,F,LIVE,RES)        \
    RES = (TRACK == ARR ? runOnCfg <ARR_DOM> (CFG, F, LIVE) :   \
                          runOnCfg <BASE_DOM> (CFG, F, LIVE))

  #define INTER_ANALYZE(SUMM_DOM,BASE_DOM,ARR_DOM,TRACK,CG,M,LIVE,RES)     \
  switch (SUMM_DOM){                                                       \
    case TERMS_INTERVALS:                                                  \
      RES = (TRACK == ARR ?                                                \
           runOnCg <arr_term_int_domain_t, ARR_DOM> (CG, LIVE, M) :        \
           runOnCg <term_int_domain_t, BASE_DOM> (CG, LIVE, M)) ;          \
      break;                                                               \
    case TERMS_DIS_INTERVALS:                                              \
      RES = (TRACK == ARR ?                                                \
           runOnCg <arr_term_dis_int_domain_t, ARR_DOM> (CG, LIVE, M) :    \
           runOnCg <term_dis_int_domain_t, BASE_DOM> (CG, LIVE, M)) ;      \
      break;                                                               \
    default:                                                               \
      RES = (TRACK == ARR ?                                                \
           runOnCg <arr_dbm_domain_t, ARR_DOM> (CG, LIVE, M) :             \
           runOnCg <dbm_domain_t, BASE_DOM> (CG, LIVE, M)) ; }             
 
   
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
          INTER_ANALYZE (CrabSummDomain,ric_domain_t,arr_ric_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case ZONES: 
          INTER_ANALYZE (CrabSummDomain,dbm_domain_t,arr_dbm_domain_t,
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case TERMS_INTERVALS:
          INTER_ANALYZE (CrabSummDomain,term_int_domain_t,arr_term_int_domain_t, 
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case TERMS_DIS_INTERVALS:
          INTER_ANALYZE (CrabSummDomain,term_dis_int_domain_t,arr_term_dis_int_domain_t, 
                         CrabTrackLev,cg,M,live_map,change); 
          break;
        case DIS_INTERVALS:
          INTER_ANALYZE (CrabSummDomain,dis_interval_domain_t,arr_dis_interval_domain_t,
                         CrabTrackLev, cg,M,live_map,change); 
          break;
        case INTERVALS:  
        default: 
          if (m_absdom != INTERVALS)
            std::cerr << "Warning: either abstract domain not found or "
                      << "inter-procedural version not implemented. "
                      << "Running intervals inter-procedurally ...\n"; 
      
          INTER_ANALYZE (CrabSummDomain,interval_domain_t,arr_interval_domain_t,
                         CrabTrackLev,cg,M,live_map,change);
      }      
      
      if (CrabLive) {
        for (auto &p : live_map)
          delete p.second;
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
    switch (m_absdom) {
      case INTERVALS_CONGRUENCES: 
        ANALYZE(CrabTrackLev, ric_domain_t, arr_ric_domain_t, cfg, F, *live, change);
        break;
      case ZONES: 
        ANALYZE(CrabTrackLev, dbm_domain_t, arr_dbm_domain_t, cfg, F, *live, change);
        break;
      case TERMS_INTERVALS:
        ANALYZE(CrabTrackLev, term_int_domain_t, arr_term_int_domain_t, cfg, F, *live, change);
        break;
      case TERMS_DIS_INTERVALS:
        ANALYZE(CrabTrackLev, term_dis_int_domain_t, arr_term_dis_int_domain_t, cfg, F, *live, change);
        break;
      case BOXES:
        ANALYZE(CrabTrackLev, boxes_domain_t, arr_boxes_domain_t, cfg, F, *live, change);
        break;
      case DIS_INTERVALS:
        ANALYZE(CrabTrackLev, dis_interval_domain_t, arr_dis_interval_domain_t, cfg, F, *live, change);
        break;
      case INTERVALS:  
      default: 
        if (m_absdom != INTERVALS)
          std::cerr << "Warning: abstract domain not found. Running intervals ...\n"; 
        ANALYZE(CrabTrackLev, interval_domain_t, arr_interval_domain_t, cfg, F, *live, change);
    }
    
    if (CrabPrintAns)
      writeInvariants (outs (), F);

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
              << " and backward domain:" 
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
    AU.addRequired<crab_llvm::NameValues>();
  } 

} // end namespace 

static llvm::RegisterPass<crab_llvm::CrabLlvm> 
X ("crab-llvm",
   "Infer invariants using Crab", 
   false, 
   false);
   


