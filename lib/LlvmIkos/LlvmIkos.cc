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

#include "ikos_llvm/config.h"
#include "ikos_llvm/LlvmIkos.hh"
#include "ikos_llvm/SymEval.hh"
#include "ikos_llvm/AbstractDomainsImpl.hh"
#include "ikos/analysis/FwdAnalyzer.hpp"

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;
using namespace llvm_ikos;

llvm::cl::opt<bool>
LlvmIkosPrintAns ("ikos-answer", llvm::cl::desc ("Print Ikos invariants"),
             llvm::cl::init (false));

llvm::cl::opt<IkosDomain>
LlvmIkosDomain("ikos-dom",
       llvm::cl::desc ("Ikos abstract domain used to infer invariants"),
       llvm::cl::values 
       (clEnumValN (INTERVALS, "int",
                    "Classical interval domain (default)"),
#if IKOS_MINOR_VERSION >= 2
        clEnumValN (INTERVALS_CONGRUENCES, "cong",
                    "Reduced product of intervals with congruences"),
        clEnumValN (ZONES , "zones",
                    "Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (OCTAGONS, "oct",
                   "Octagon domain"),
        clEnumValN (TERMS, "term",
                    "Term-enriched interval domain."),
#endif 
        clEnumValEnd),
       llvm::cl::init (INTERVALS));

llvm::cl::opt<bool>
LlvmIkosLive("ikos-live", 
        llvm::cl::desc("Run Ikos with live ranges"),
        llvm::cl::init (false));

llvm::cl::opt<enum TrackedPrecision>
LlvmIkosTrackLev("ikos-track-lvl",
   llvm::cl::desc ("Track level for Cfg and abstract domains"),
   cl::values (clEnumValN (REG, "reg", "Primitive registers only"),
               clEnumValN (PTR, "ptr", "REG + pointers"),
               clEnumValN (MEM, "mem", "PTR + memory content"),
               clEnumValEnd),
   cl::init (TrackedPrecision::REG));

llvm::cl::opt<bool>
LlvmIkosInterProc ("ikos-cfg-interproc",
             cl::desc ("Build inter-procedural Cfg"), 
             cl::init (false));


namespace llvm_ikos
{

  using namespace analyzer;
  using namespace domain_impl;

  char llvm_ikos::LlvmIkos::ID = 0;

  bool LlvmIkos::runOnModule (llvm::Module &M)
  {
    // -- initialize from cli options
    m_absdom = LlvmIkosDomain;
    m_runlive = LlvmIkosLive;

#ifdef HAVE_DSA
    m_mem = MemAnalysis (&getAnalysis<SteensgaardDataStructures> (),
                         LlvmIkosTrackLev);
#endif     

    bool change=false;
    for (auto &f : M) 
      change |= runOnFunction (f); 
    
    if (LlvmIkosPrintAns) dump (M);
    return change;
  }

  bool LlvmIkos::runOnFunction (llvm::Function &F)
  {
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    cfg_t cfg = CfgBuilder (F, m_vfac, &m_mem, LlvmIkosInterProc)();

    bool change=false;
    switch (m_absdom)
    {
      case INTERVALS:  
#if IKOS_MINOR_VERSION >= 2
          change = (LlvmIkosTrackLev >= MEM ? 
                    runOnCfg <arr_interval_domain_t> (cfg, F) : 
                    runOnCfg <interval_domain_t> (cfg, F)) ; 
#else
          change = runOnCfg <interval_domain_t> (cfg, F); 
#endif 
        break;
#if IKOS_MINOR_VERSION >= 2
      case INTERVALS_CONGRUENCES: 
         change = (LlvmIkosTrackLev >= MEM ? 
                   runOnCfg <arr_ric_domain_t> (cfg, F) : 
                   runOnCfg <ric_domain_t> (cfg, F)) ; 
        break;
      case ZONES: 
         change = (LlvmIkosTrackLev >= MEM ? 
                   runOnCfg <arr_dbm_domain_t> (cfg, F) :  
                   runOnCfg <dbm_domain_t> (cfg, F)) ; 
        break;
      case OCTAGONS: 
        change = (LlvmIkosTrackLev >= MEM ? 
                  runOnCfg <arr_octagon_domain_t> (cfg, F) : 
                  runOnCfg <octagon_domain_t> (cfg, F)) ; 
        break;
      case TERMS:
        change = (LlvmIkosTrackLev >= MEM ? 
                  runOnCfg <arr_term_domain_t> (cfg, F) : 
                  runOnCfg <term_domain_t> (cfg, F)) ; 
        break;
#endif
      default: assert(false && "Unsupported abstract domain");
    }
    return change;
  }

  template<typename AbsDomain>
  bool LlvmIkos::runOnCfg (cfg_t& cfg, llvm::Function &F)
  {
    typedef typename NumFwdAnalyzer <cfg_t,AbsDomain,VariableFactory>::type analyzer_t;

    analyzer_t analyzer (cfg, m_vfac, m_runlive);
    analyzer.Run (AbsDomain::top());

    // --- Translate internal abstract representations to integer
    //     linear constraints.
    for (auto &B : F)
    {
      // --- invariants that hold at the entry of the blocks
      const llvm::BasicBlock *BB = &B;
      AbsDomain pre = analyzer.get_pre (&B);
      if (pre.is_bottom ())
        m_pre_map.insert (make_pair (BB, mkFALSE ()));
      else if (pre.is_top ())
        m_pre_map.insert (make_pair (BB, mkTRUE ()));        
      else
          m_pre_map.insert (make_pair (BB, toLinCst (pre)));

      // --- invariants that hold at the exit of the blocks
      AbsDomain post = analyzer.get_post (&B);
      if (post.is_bottom ())
        m_post_map.insert (make_pair (BB, mkFALSE ()));
      else if (post.is_top ())
        m_post_map.insert (make_pair (BB, mkTRUE ()));        
      else
        m_post_map.insert (make_pair (BB, toLinCst (post)));
    }
    
    return false;
  }


  // Write to standard output the invariants 
  void LlvmIkos::dump (llvm::Module &M) const
  {
    for (auto &F : M)
    {
      if (F.isDeclaration () || F.empty ()) continue;
      
        errs () << "\nFunction " << F.getName () << "\n";
        for (auto &B : F)
        {
          const llvm::BasicBlock * BB = &B;
          errs () << "\t" << BB->getName () << ": ";
          auto inv = getPost (BB);
          errs () << inv << "\n";
        }
        errs () <<  "\n";
    }
  }

  void LlvmIkos::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();
#ifdef HAVE_DSA
    AU.addRequiredTransitive<llvm::SteensgaardDataStructures> ();
    AU.addRequired<llvm::DataLayoutPass>();
    AU.addRequired<llvm::UnifyFunctionExitNodes> ();
#endif 
  } 

} // end namespace llvm_ikos

static llvm::RegisterPass<llvm_ikos::LlvmIkos> 
X ("llvm-ikos",
   "Infer invariants using Ikos", 
   false, false);
   


