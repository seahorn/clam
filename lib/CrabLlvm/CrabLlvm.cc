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
// #define CRABLLVM_DEBUG

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

  namespace setup_abs_dom {

    // Some domains can be more effective if the tracked variables are
    // known in advance.
    void collectVars(const llvm::Function &f, 
                     VariableFactory& vfac, MemAnalysis& mem, 
                     set<varname_t>& vars) {
      
      SymEval<VariableFactory, z_lin_exp_t> s (vfac, &mem);
      for (const_inst_iterator I = inst_begin(f), E = inst_end(f); I != E; ++I)
      {
        const Instruction *instr = &*I;
        
        //compare
        if(const CmpInst *ci = dyn_cast<CmpInst>(instr)) 
        {
          // -- track all compare instructions except for those
          // -- that are only used in the terminator of the basic
          // -- block they are defined in
          
          // XXX This includes compare instructions that are only
          // used in 'select' or 'or/and' statements inside BBs
          // they are defined in. We can probably get rid of such
          // compares as well.
          const llvm::BasicBlock *ciBB = ci->getParent ();
          const TerminatorInst *term = ciBB->getTerminator ();
          
          for (Value::const_use_iterator ut = ci->use_begin (),
                   ue = ci->use_end (); ut != ue; ++ut)
          {
            const User *u = ut->getUser ();
            const Instruction* uinst = cast<Instruction> (u);
            // -- use outside of a terminator
            if (uinst != term)
            {
              vars.insert (s.symVar (*ci));
              break;
            }
            // --  use in another BB
            if (uinst->getParent () != ciBB) 
            {
              vars.insert (s.symVar (*ci));
              break;
            }
          }
          
        }
        // -- boolean binary ops
        else if (isa<BinaryOperator> (*instr) && 
                 (instr->getType ()->isIntegerTy (1)))
        {
          const llvm::BasicBlock *pBB = instr->getParent ();
          
          for (Value::const_use_iterator ut = instr->use_begin (),
                   ue = instr->use_end (); ut != ue; ++ut)
          {
            const User *u = ut->getUser ();
            const Instruction* uinst = cast<Instruction> (u);
            // -- use outside of a terminator
            // --  use in another BB
            if (uinst->getParent () != pBB) 
            {
              vars.insert (s.symVar (*instr));
              break;		    
            }
          }
        }
        //PHI node
        else if(isa<PHINode>(*instr)) vars.insert (s.symVar (*instr));
        //binary operation
        else if(isa<BinaryOperator>(*instr))
        { 
          if (!(instr->getType ()->isIntegerTy (1))) vars.insert (s.symVar (*instr));
        }
        //return
        else if(const ReturnInst *ri = dyn_cast<ReturnInst>(instr)) 
        {
          if(ri->getNumOperands() && !(ri->getOperand(0)->getType ()->isIntegerTy ())) 
            vars.insert (s.symVar (*(ri->getOperand(0))));
        }
        //call
        else if (const CallInst *ci = dyn_cast<CallInst>(instr)) 
        {
          Function *fp = ci->getCalledFunction();
          // -- track non-void functions, and functions for which there 
          // -- is no llvm::Function object
          if(!fp || 
             (fp->getReturnType() != Type::getVoidTy(fp->getContext ())))
            vars.insert (s.symVar (*instr));
        }
        //cast
        else if(isa<CastInst>(*instr)) vars.insert (s.symVar (*instr));
        //select
        else if(isa<SelectInst>(*instr)) { vars.insert (s.symVar (*instr)); }
        //load
        else if (isa<LoadInst>(*instr)) {
          if (instr->getType()->isIntegerTy () && mem.getTrackLevel () == ARR) {
            vars.insert (s.symVar (*instr));
          }
        }
      }
    }

    // This method assumes that inv has some global state that can be
    // shared by all instances
    template<typename AbsDomain>
    void setGlobalParams (const llvm::Function &F, 
                          VariableFactory& vfac, MemAnalysis& mem, 
                          AbsDomain& inv) {}

    // This method assumes that inv has some global state that can be
    // shared by all instances
    template<typename AbsDomain>
    void resetGlobalParams (AbsDomain& inv) {}

    bool matchRegex (string s, string re_s){
      try {
        std::regex re (re_s);
        return std::regex_match (s, re);
     } 
      catch (std::regex_error& e) {
        errs () << "Syntax error in the regex: " << e.what () << "\n";
        return false;
      }
    }
  
    template<>
    void setGlobalParams (const llvm::Function &F, 
                          VariableFactory& vfac, MemAnalysis& mem, 
                          boxes_domain_t& inv) {
      set<varname_t> tracked_vars;
      collectVars (F, vfac, mem, tracked_vars);
      for (auto v: tracked_vars) {
        if (LlvmCrabBoxesTrackRegex != "" &&
            !matchRegex(v.str (), LlvmCrabBoxesTrackRegex))
          continue;
        inv.addTrackVar (v);
#ifdef CRABLLVM_DEBUG
        errs () << "\n-- Boxes will track " << v.str ();
#endif 
      }
    }
    
    template<>
    void setGlobalParams (const llvm::Function &F, 
                          VariableFactory& vfac, MemAnalysis& mem, 
                          arr_boxes_domain_t& inv) {
      set<varname_t> tracked_vars;
      collectVars (F, vfac, mem, tracked_vars);
      for (auto v: tracked_vars) {
        if (LlvmCrabBoxesTrackRegex != "" &&
            !matchRegex(v.str (), LlvmCrabBoxesTrackRegex))
          continue;
        inv.get_base_domain().addTrackVar (v);
#ifdef CRABLLVM_DEBUG
        errs () << "-- Boxes will track " << v.str () << "\n";
#endif 
      }
    }
  
    template<>
    void resetGlobalParams (boxes_domain_t& inv){
      inv.resetTrackVars ();
    }
    template<>
    void resetGlobalParams (arr_boxes_domain_t& inv){
      inv.get_base_domain().resetTrackVars ();
    }
  
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
    AbsDomain inv = AbsDomain::top();
    setup_abs_dom::setGlobalParams (F, m_vfac, m_mem, inv);
    analyzer.Run (inv);
    setup_abs_dom::resetGlobalParams (inv);
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

<<<<<<< HEAD
  template<typename T1, typename T2, typename Range>
  inline T2 forget (T2 inv, Range vs) {
    if (vs.begin () == vs.end ()) 
      return inv;
    T1 abs_dom_inv = crab::domain_traits::absdom_to_formula<T1, T2>::unmarshall (inv);
    if (abs_dom_inv.is_top () || abs_dom_inv.is_bottom ()) 
      return inv;
    crab::domain_traits::forget (abs_dom_inv, vs.begin(), vs.end());
    return crab::domain_traits::absdom_to_formula<T1, T2>::marshall(abs_dom_inv);
  }

  // It is expensive because it needs to translate from inv_tbl_val_t
  // to an abstract domain, performs abstract forget operation and
  // translates back to inv_tbl_val_t.
  template<typename Range>
  CrabLlvm::inv_tbl_val_t forget (CrabLlvm::inv_tbl_val_t inv, 
                                  CrabDomain absdom, Range vs) {
    switch (absdom) {
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
      case SZONES: 
        if (LlvmCrabTrackLev == ARR)
          return forget<arr_sdbm_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
        else
          return forget<sdbm_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
      case TERMS:
        if (LlvmCrabTrackLev == ARR)
          return forget<arr_term_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
        else
          return forget<term_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
      case INTERVALS:  
      default:  
        if (LlvmCrabTrackLev == ARR)
          return forget<arr_interval_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
        else
          return forget<interval_domain_t, CrabLlvm::inv_tbl_val_t, Range> (inv, vs);
    }
  }

=======
>>>>>>> origin/master
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
   


