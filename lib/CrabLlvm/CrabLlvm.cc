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
#include "crab_llvm/CrabLlvm.hh"
#include "crab_llvm/SymEval.hh"
#include "crab_llvm/AbstractDomainsImpl.hh"

#include "crab/domains/domain_traits.hpp"
#include "crab/analysis/FwdAnalyzer.hpp"
#include "crab/analysis/InterFwdAnalyzer.hpp"
#include "crab/cg/CgBgl.hpp"

#include <regex>

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;
using namespace crab_llvm;

// for debugging
#define CRABLLVM_DEBUG

llvm::cl::opt<bool>
LlvmCrabPrintAns ("crab-print-invariants", 
                  llvm::cl::desc ("Print Crab invariants"),
                  llvm::cl::init (false));

llvm::cl::opt<bool>
LlvmCrabPrintSumm ("crab-print-summaries", 
                   llvm::cl::desc ("Print Crab function summaries"),
                   llvm::cl::init (false));

llvm::cl::opt<unsigned int>
LlvmCrabWideningThreshold("crab-widening-threshold", 
   llvm::cl::desc("Max number of fixpoint iterations until widening is triggered"),
   llvm::cl::init (1),
   cl::Hidden);

llvm::cl::opt<unsigned int>
LlvmCrabNarrowingIters("crab-narrowing-iters", 
                       llvm::cl::desc("Max number of narrowing iterations"),
                       llvm::cl::init (999999),
                       cl::Hidden);

llvm::cl::opt<CrabDomain>
LlvmCrabDomain("crab-dom",
       llvm::cl::desc ("Crab abstract domain used to infer invariants"),
       llvm::cl::values 
       (clEnumValN (INTERVALS, "int",
                    "Classical interval domain (default)"),
        clEnumValN (INTERVALS_CONGRUENCES, "ric",
                    "Reduced product of intervals with congruences"),
        clEnumValN (ZONES , "zones",
                    "Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (SZONES, "szones",
                    "Split difference-Bounds Matrix domain"),
        clEnumValN (TERMS, "term",
                    "Intervals with uninterpreted functions."),
        clEnumValN (NUM, "num",
                    "Choose automatically the numerical abstract domain."),
        clEnumValN (BOXES, "boxes",
                    "Disjunctive intervals"),
        clEnumValEnd),
       llvm::cl::init (INTERVALS));

// If domain is num
llvm::cl::opt<unsigned>
LlvmCrabNumThreshold("crab-dom-num-max-live", 
   llvm::cl::desc("Max number of live vars per block before switching domains"),
   llvm::cl::init (100),
   cl::Hidden);

// If domain is boxes
llvm::cl::opt<string>
LlvmCrabBoxesTrackRegex("crab-boxes-track-names", 
                        llvm::cl::desc("Variables names (given as a regex) tracked by boxes"),
                        llvm::cl::init (""),
                        cl::Hidden);

llvm::cl::opt<bool>
LlvmCrabLive("crab-live", 
        llvm::cl::desc("Run Crab with live ranges"),
        llvm::cl::init (false));

llvm::cl::opt<bool>
LlvmCrabInter ("crab-inter",
               cl::desc ("Crab Inter-procedural analysis"), 
               cl::init (false));

llvm::cl::opt<enum TrackedPrecision>
LlvmCrabTrackLev("crab-track-lvl",
   llvm::cl::desc ("Track precision level of the Crab Cfg"),
   cl::values (clEnumValN (INT, "int", "Integer registers only"),
               clEnumValN (PTR, "ptr", "INT + pointer addresses"),
               clEnumValN (ARR, "arr", "PTR + memory content via array abstraction"),
               clEnumValEnd),
   cl::init (TrackedPrecision::INT));

// These two options refine crab-track-lvl=arr
llvm::cl::opt<bool>
LlvmCrabTrackOnlyGlobals ("crab-track-only-globals",
                          cl::desc ("Track only global arrays"), 
                          cl::init (false),
                          cl::Hidden);
llvm::cl::opt<bool>
LlvmCrabTrackOnlySingletons ("crab-track-only-singletons",
                          cl::desc ("Track only singleton cells"), 
                          cl::init (false),
                          cl::Hidden);

// Important to crab-llvm clients (e.g., SeaHorn):
// Shadow variables are variables that cannot be mapped back to a
// const Value*. These are created for instance for memory heaps.
llvm::cl::opt<bool>
LlvmKeepShadows ("crab-keep-shadows",
                 cl::desc ("Preserve shadow variables in invariants and summaries"), 
                 cl::init (false),
                 cl::Hidden);

namespace crab_llvm {

  // this namespace is temporary for the boxes domain
  namespace setup_if_boxes_dom {

    template<typename AbsDomain>
    void setGlobalParams (const cfg_t& cfg) { }

    template<typename AbsDomain>
    void resetGlobalParams () {}
  
    struct MatchRegex : public std::unary_function<cfg_t::varname_t,bool> {
      boost::optional <std::regex> m_re;
      MatchRegex (string s) { 
        if (s != "") {
          try  { m_re = std::regex (s); }
          catch (std::regex_error& e) {
            errs () << "Warning: syntax error in the regex: " << e.what () << "\n";
          }
        }
      }
      bool operator() (cfg_t::varname_t s)  {
        if (!m_re) return true; 
        else return std::regex_match (s.str (), *m_re);
      }
    };
  
    template<> void setGlobalParams <boxes_domain_t> (const cfg_t& cfg) {
      MatchRegex filter (LlvmCrabBoxesTrackRegex);
      boxes_domain_t::create_global (cfg.get_vars (), filter);
    }
 
    template<> void setGlobalParams <arr_boxes_domain_t> (const cfg_t& cfg) {
      MatchRegex filter (LlvmCrabBoxesTrackRegex);
      boxes_domain_t::create_global (cfg.get_vars (), filter);
    }
  
    template<> void resetGlobalParams <boxes_domain_t> () 
    { boxes_domain_t::destroy_global (); }

    template<> void resetGlobalParams <arr_boxes_domain_t> () 
    { boxes_domain_t::destroy_global (); }
    
  } // end namespace 
} // end namespace 

namespace crab_llvm {

  using namespace crab::analyzer;
  using namespace crab::cg;

  char crab_llvm::CrabLlvm::ID = 0;

  bool CrabLlvm::runOnModule (llvm::Module &M)
  {
    // -- initialize from cli options
    m_absdom = LlvmCrabDomain;

#ifdef HAVE_DSA
    m_mem = MemAnalysis (&getAnalysis<SteensgaardDataStructures> (),
                         LlvmCrabTrackLev, 
                         LlvmCrabTrackOnlyGlobals,
                         LlvmCrabTrackOnlySingletons);
#endif     

    if (m_absdom == NUM) {
      // --- needs liveness information
      LlvmCrabLive = true;
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

    if (LlvmCrabInter){

      std::vector<cfg_t> cfgs;
      liveness_map_t live_map;
      unsigned max_live_per_blk = 0;
      for (auto &F : M) {
        // -- skip functions without a body
        if (F.isDeclaration () || F.empty ()) continue;
        // -- skip variadic functions
        if (F.isVarArg ()) continue;

        // -- build cfg
        CfgBuilder builder (F, m_vfac, &m_mem, 
                            /*include function decls and callsites*/
                            true);
        cfg_t &cfg = builder.makeCfg ();
        cfgs.push_back (cfg);

        // -- build liveness
        if (LlvmCrabLive) {
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
        cout << "Threshold: " << LlvmCrabNumThreshold << endl;
#endif               
        if (max_live_per_blk < LlvmCrabNumThreshold) {
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
        // TODO: make an user option the abstract domain used
        // for the bottom-up phase of the interprocedural analysis
        case INTERVALS_CONGRUENCES: 
          change = (LlvmCrabTrackLev == ARR ? 
                    runOnCg <arr_dbm_domain_t, arr_ric_domain_t> (cg, live_map, M) : 
                    runOnCg <dbm_domain_t, ric_domain_t> (cg, live_map, M)) ; 
          break;
        case ZONES: 
          change = (LlvmCrabTrackLev == ARR ? 
                    runOnCg <arr_dbm_domain_t, arr_dbm_domain_t> (cg, live_map, M) :  
                    runOnCg <dbm_domain_t, dbm_domain_t> (cg, live_map, M)) ; 
          break;
        case SZONES: 
          change = (LlvmCrabTrackLev == ARR ? 
                    runOnCg <arr_sdbm_domain_t, arr_sdbm_domain_t> (cg, live_map, M) :  
                    runOnCg <sdbm_domain_t, sdbm_domain_t> (cg, live_map, M)) ; 
          break;
        case TERMS:
          change = (LlvmCrabTrackLev == ARR ? 
                    runOnCg <arr_dbm_domain_t, arr_term_domain_t> (cg, live_map, M) : 
                    runOnCg <dbm_domain_t, term_domain_t> (cg, live_map, M)) ; 
          break;
        // case BOXES:
        //   change = (LlvmCrabTrackLev == ARR ? 
        //             runOnCg <arr_boxes_domain_t, arr_boxes_domain_t> (cg, live_map, M) : 
        //             runOnCg <boxes_domain_t, boxes_domain_t> (cg, live_map, M)) ; 
        //   break;
        case INTERVALS:  
        default: 
          if (m_absdom != INTERVALS)
            std::cerr << "Warning: either abstract domain not found or "
                      << "inter-procedural version not implemented. "
                      << "Running intervals inter-procedurally ...\n"; 
          
          change = (LlvmCrabTrackLev == ARR ? 
                    runOnCg <arr_dbm_domain_t, arr_interval_domain_t> (cg, live_map, M) : 
                    runOnCg <dbm_domain_t, interval_domain_t> (cg, live_map, M)) ; 
      }      
      
      if (LlvmCrabLive) {
        for (auto &p : live_map)
          delete p.second;
      }          
        
      return change;
    }
    else {
      // -- run intra-procedural analysis
      bool change=false;
      for (auto &f : M) 
        change |= runOnFunction (f); 
      return change;
    }
  }

  bool CrabLlvm::runOnFunction (llvm::Function &F)
  {

    if (LlvmCrabInter) return false;
      
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;
    // -- skip variadic functions
    if (F.isVarArg ()) return false;

    // -- build cfg
    CfgBuilder builder (F, m_vfac, &m_mem, 
                        /*include function decls and callsites*/
                        true);
    cfg_t &cfg = builder.makeCfg ();

    // -- run liveness
    liveness_t* live = nullptr;
    unsigned max_live_per_blk = 0;
    if (LlvmCrabLive) {
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
      cout << "Threshold: " << LlvmCrabNumThreshold << endl;
#endif               
      if (max_live_per_blk < LlvmCrabNumThreshold) {
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
        change = (LlvmCrabTrackLev == ARR ? 
                  runOnCfg <arr_ric_domain_t> (cfg, *live, F) : 
                  runOnCfg <ric_domain_t> (cfg, *live, F)) ; 
        break;
      case ZONES: 
        change = (LlvmCrabTrackLev == ARR ? 
                  runOnCfg <arr_dbm_domain_t> (cfg, *live, F) :  
                  runOnCfg <dbm_domain_t> (cfg, *live, F)) ; 
        break;
      case SZONES: 
        change = (LlvmCrabTrackLev == ARR ? 
                  runOnCfg <arr_sdbm_domain_t> (cfg, *live, F) :  
                  runOnCfg <sdbm_domain_t> (cfg, *live, F)) ; 
        break;
      case TERMS:
        change = (LlvmCrabTrackLev == ARR ? 
                  runOnCfg <arr_term_domain_t> (cfg, *live, F) : 
                  runOnCfg <term_domain_t> (cfg, *live, F)) ; 
        break;
      case BOXES:
        change = (LlvmCrabTrackLev == ARR ? 
                  runOnCfg <arr_boxes_domain_t> (cfg, *live, F) : 
                  runOnCfg <boxes_domain_t> (cfg, *live, F)) ; 
        break;
      case INTERVALS:  
      default: 
        if (m_absdom != INTERVALS)
          std::cerr << "Warning: abstract domain not found."
                    << "Running intervals ...\n"; 
        
        change = (LlvmCrabTrackLev == ARR ? 
          runOnCfg <arr_interval_domain_t> (cfg, *live, F) : 
          runOnCfg <interval_domain_t> (cfg, *live, F)) ; 
    }
    
    if (LlvmCrabPrintAns)
      write (outs (), F);

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
    analyzer_t analyzer(cg, m_vfac, (LlvmCrabLive ? &live_map : nullptr),
                        LlvmCrabWideningThreshold, LlvmCrabNarrowingIters);
    analyzer.Run (TDAbsDomain::top ());

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
          if (LlvmCrabPrintAns)
            write (outs (), *F);

          if (LlvmCrabPrintSumm && analyzer.has_summary (cfg)) {
            auto summ = analyzer.get_summary (cfg);
            outs () << "SUMMARY " << F->getName () << ": " << summ << "\n";
          }
        }
      }
    }
    return false;
  }

  template<typename AbsDomain>
  bool CrabLlvm::runOnCfg (const cfg_t& cfg, 
                           const liveness_t& live, 
                           const llvm::Function &F) {

    typedef typename NumFwdAnalyzer <cfg_t, 
                                     AbsDomain, 
                                     VariableFactory>::type analyzer_t;
                                     
#ifdef CRABLLVM_DEBUG
    auto fdecl = cfg.get_func_decl ();            
    assert (fdecl);
    AbsDomain tmp;
    std::cout << "Running " << tmp.getDomainName () << " analysis for " 
              << (*fdecl).get_func_name ()
              << "  ... ";
    std::cout.flush ();
#endif 

    // -- run intra-procedural analysis
    analyzer_t analyzer (cfg, m_vfac, &live, 
                         LlvmCrabWideningThreshold, LlvmCrabNarrowingIters);

    setup_if_boxes_dom::setGlobalParams<AbsDomain> (cfg);
    AbsDomain inv = AbsDomain::top();
    analyzer.Run (inv);
    setup_if_boxes_dom::resetGlobalParams<AbsDomain> ();

#ifdef CRABLLVM_DEBUG
    std::cout << "DONE\n";
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


  void CrabLlvm::write (llvm::raw_ostream& o, const llvm::Function& F) {
    if (!F.isDeclaration () && !F.empty () && !F.isVarArg ()) {
      o << "\nFunction " << F.getName () << "\n";
      for (auto &B : F) {
        const llvm::BasicBlock * BB = &B;
        o << "\t" << BB->getName () << ": ";
        auto inv = getPost (BB, LlvmKeepShadows);
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
   false, false);
   


