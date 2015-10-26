#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
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

#include "crab/analysis/FwdAnalyzer.hpp"
#include "crab/analysis/InterFwdAnalyzer.hpp"
#include "crab/cg/CgBgl.hpp"


#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;
using namespace crab_llvm;

// for debugging
// #define CRABLLVM_DEBUG

llvm::cl::opt<bool>
LlvmCrabPrintAns ("crab-print-invariants", 
                  llvm::cl::desc ("Print Crab invariants"),
                  llvm::cl::init (false));

llvm::cl::opt<bool>
LlvmCrabPrintSumm ("crab-print-summaries", 
                   llvm::cl::desc ("Print Crab function summaries"),
                   llvm::cl::init (false));

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
        clEnumValN (TERMS, "term",
                    "Intervals with uninterpreted functions."),
        clEnumValEnd),
       llvm::cl::init (INTERVALS));

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

namespace crab_llvm
{

  using namespace crab::analyzer;
  using namespace crab::cg;
  using namespace domain_impl;

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
          // some stats
          unsigned total_live, max_live_per_blk, avg_live_per_blk;
          live->get_stats (total_live, max_live_per_blk, avg_live_per_blk);
          std::cout << "-- Max number of out live vars per block=" 
                    << max_live_per_blk << "\n";
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
        case TERMS:
          change = (LlvmCrabTrackLev == ARR ? 
                    runOnCg <arr_dbm_domain_t, arr_term_domain_t> (cg, live_map, M) : 
                    runOnCg <dbm_domain_t, term_domain_t> (cg, live_map, M)) ; 
          break;
        case INTERVALS:  
        default: 
          if (m_absdom != INTERVALS)
            std::cout << "Warning: abstract domain not found."
                      << "Running intervals ...\n"; 
          
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
      // some stats
      unsigned total_live, max_live_per_blk, avg_live_per_blk;
      ls.get_stats (total_live, max_live_per_blk, avg_live_per_blk);
      std::cout << "-- Max number of out live vars per block=" 
                << max_live_per_blk << "\n";
      std::cout << "-- Avg number of out live vars per block=" 
                << avg_live_per_blk << "\n";
      std::cout.flush ();
#endif 
      live = &ls;
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
      case TERMS:
        change = (LlvmCrabTrackLev == ARR ? 
                  runOnCfg <arr_term_domain_t> (cfg, *live, F) : 
                  runOnCfg <term_domain_t> (cfg, *live, F)) ; 
        break;
      case INTERVALS:  
      default: 
        if (m_absdom != INTERVALS)
          std::cout << "Warning: abstract domain not found."
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
                              BUAbsDomain, TDAbsDomain, inv_tbl_val_t> analyzer_t;
    analyzer_t analyzer(cg, m_vfac, (LlvmCrabLive ? &live_map : nullptr));
    analyzer.Run (TDAbsDomain::top ());

    // -- store invariants     
    for (auto &n: boost::make_iterator_range (/*boost::vertices (cg)*/cg.nodes ())) {
      const cfg_t& cfg = n.getCfg ();
      boost::optional<const llvm::Value *> v = n.name ().get ();
      if (v) {
        if (const llvm::Function *F = dyn_cast<llvm::Function> (*v)) {
          for (auto &B : *F) {
            // --- invariants that hold at the entry of the blocks
            m_pre_map.insert (make_pair (&B, analyzer.get_pre (cfg, &B)));
            // --- invariants that hold at the exit of the blocks
            m_post_map.insert (make_pair (&B, analyzer.get_post (cfg, &B)));
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

    typedef typename NumFwdAnalyzer <cfg_t, AbsDomain, 
                                     VariableFactory, 
                                     inv_tbl_val_t>::type analyzer_t;

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
    analyzer_t analyzer (cfg, m_vfac, &live);
    analyzer.Run (AbsDomain::top());

#ifdef CRABLLVM_DEBUG
    std::cout << "DONE\n";
#endif 

    // -- store invariants 
    for (auto const &B : F) {
      // --- invariants that hold at the entry of the blocks
      m_pre_map.insert (make_pair (&B, analyzer.get_pre (&B)));
      // --- invariants that hold at the exit of the blocks
      m_post_map.insert (make_pair (&B, analyzer.get_post (&B)));
    }
    
    return false;
  }

  template<typename T1, typename T2, typename Range>
  inline T2 forget (T2 inv, Range vs) {
    if (vs.begin () == vs.end ()) 
      return inv;
    T1 abs_dom_inv = inv_tbl_traits<T1, T2>::unmarshall (inv);
    if (abs_dom_inv.is_top () || abs_dom_inv.is_bottom ()) 
      return inv;
    crab::domain_traits::forget (abs_dom_inv, vs.begin(), vs.end());
    return inv_tbl_traits<T1, T2>::marshall(abs_dom_inv);
  }

  // It is expensive because it needs to translate from inv_tbl_val_t
  // to an abstract domain, performs abstract forget operation and
  // translates back to inv_tbl_val_t.
  template<typename Range>
  CrabLlvm::inv_tbl_val_t forget (CrabLlvm::inv_tbl_val_t inv, 
                                  CrabDomain absdom, Range vs) {
    switch (absdom) {
      case INTERVALS:  
        if (LlvmCrabTrackLev == ARR)
          return forget<arr_interval_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
        else
          return forget<interval_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
      case INTERVALS_CONGRUENCES: 
        if (LlvmCrabTrackLev == ARR)
          return forget<arr_ric_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
        else
          return forget<ric_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
      case ZONES: 
        if (LlvmCrabTrackLev == ARR)
          return forget<arr_dbm_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
        else
          return forget<dbm_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
      case TERMS:
        if (LlvmCrabTrackLev == ARR)
          return forget<arr_term_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
        else
          return forget<term_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
      default: assert(false && "unreachable");
    }
  }

  // return invariants that hold at the entry of BB
  CrabLlvm::inv_tbl_val_t CrabLlvm::getPre (const llvm::BasicBlock *BB, 
                                            bool KeepShadows) const {
    const_iterator it = m_pre_map.find (BB);
    assert (it != m_pre_map.end ());
    if (KeepShadows)
      return it->second;
    else {
      auto shadows = m_vfac.get_shadow_vars ();
      return forget (it->second, m_absdom, shadows);
    }
  }   

  // return invariants that hold at the exit of BB
  CrabLlvm::inv_tbl_val_t CrabLlvm::getPost (const llvm::BasicBlock *BB, 
                                             bool KeepShadows) const {
    const_iterator it = m_post_map.find (BB);
    assert (it != m_post_map.end ());
    if (KeepShadows)
      return it->second;
    else {
      auto shadows = m_vfac.get_shadow_vars ();
      return forget (it->second, m_absdom, shadows);
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
   


