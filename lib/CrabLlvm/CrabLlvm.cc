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

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;
using namespace crab_llvm;

llvm::cl::opt<bool>
LlvmCrabPrintAns ("crab-answer", llvm::cl::desc ("Print Crab invariants"),
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
                    "Term-enriched interval domain."),
        clEnumValN (NAIVE_DBM , "naive-dbm",
                    "Naive Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (CGS_DBM , "cgs-dbm",
                    "Naive DBM + dynamic variable packing"),
        clEnumValEnd),
       llvm::cl::init (INTERVALS));

llvm::cl::opt<bool>
LlvmCrabLive("crab-live", 
        llvm::cl::desc("Run Crab with live ranges"),
        llvm::cl::init (false));

llvm::cl::opt<enum TrackedPrecision>
LlvmCrabTrackLev("crab-track-lvl",
   llvm::cl::desc ("Track level for Cfg and abstract domains"),
   cl::values (clEnumValN (REG, "reg", "Primitive registers only"),
               clEnumValN (PTR, "ptr", "REG + pointers"),
               clEnumValN (MEM, "mem", "PTR + memory content"),
               clEnumValEnd),
   cl::init (TrackedPrecision::REG));

llvm::cl::opt<bool>
LlvmCrabInterProc ("crab-cfg-interproc",
             cl::desc ("Build inter-procedural Cfg"), 
             cl::init (false));


namespace crab_llvm
{

  using namespace crab::analyzer;
  using namespace domain_impl;

  char crab_llvm::CrabLlvm::ID = 0;

  bool CrabLlvm::runOnModule (llvm::Module &M)
  {
    // -- initialize from cli options
    m_absdom = LlvmCrabDomain;
    m_runlive = LlvmCrabLive;

#ifdef HAVE_DSA
    m_mem = MemAnalysis (&getAnalysis<SteensgaardDataStructures> (),
                         LlvmCrabTrackLev);
#endif     

    bool change=false;
    for (auto &f : M) 
      change |= runOnFunction (f); 
    
    if (LlvmCrabPrintAns) dump (M);
    return change;
  }

  bool CrabLlvm::runOnFunction (llvm::Function &F)
  {
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    CfgBuilder builder (F, m_vfac, &m_mem, LlvmCrabInterProc);
    cfg_t &cfg = builder.makeCfg ();

    bool change=false;
    switch (m_absdom)
    {
      case INTERVALS:  
        change = (LlvmCrabTrackLev >= MEM ? 
                  runOnCfg <arr_interval_domain_t> (cfg, F) : 
                  runOnCfg <interval_domain_t> (cfg, F)) ; 
        break;
      case INTERVALS_CONGRUENCES: 
        change = (LlvmCrabTrackLev >= MEM ? 
                  runOnCfg <arr_ric_domain_t> (cfg, F) : 
                  runOnCfg <ric_domain_t> (cfg, F)) ; 
        break;
      case ZONES: 
        change = (LlvmCrabTrackLev >= MEM ? 
                  runOnCfg <arr_dbm_domain_t> (cfg, F) :  
                  runOnCfg <dbm_domain_t> (cfg, F)) ; 
        break;
      case TERMS:
        change = (LlvmCrabTrackLev >= MEM ? 
                  runOnCfg <arr_term_domain_t> (cfg, F) : 
                  runOnCfg <term_domain_t> (cfg, F)) ; 
        break;
      case NAIVE_DBM:
        change = runOnCfg <naive_dbm_domain_t> (cfg, F); 
        break;
      case CGS_DBM:
        change = runOnCfg <cgs_dbm_domain_t> (cfg, F); 
        break;
      default: assert(false && "Unsupported abstract domain");
    }
    return change;
  }

  template<typename AbsDomain>
  bool CrabLlvm::runOnCfg (cfg_t& cfg, llvm::Function &F)
  {
    typedef typename NumFwdAnalyzer <cfg_t, AbsDomain, 
                                     VariableFactory, 
                                     inv_tbl_val_t>::type analyzer_t;

    analyzer_t analyzer (cfg, m_vfac, m_runlive);
    analyzer.Run (AbsDomain::top());

    // Store invariants to survive across functions
    for (auto &B : F)
    {
      const llvm::BasicBlock *BB = &B;
      // --- invariants that hold at the entry of the blocks
      m_pre_map.insert (make_pair (BB, analyzer.get_pre (&B)));
      // --- invariants that hold at the exit of the blocks
      m_post_map.insert (make_pair (BB, analyzer.get_post (&B)));
    }
    
    return false;
  }


  // Write to standard output the invariants 
  void CrabLlvm::dump (llvm::Module &M) const
  {
    for (auto &F : M) {
      if (F.isDeclaration () || F.empty ()) continue;
      outs () << "\nFunction " << F.getName () << "\n";
      for (auto &B : F) {
        const llvm::BasicBlock * BB = &B;
        outs () << "\t" << BB->getName () << ": ";
        auto inv = getPost (BB);
        outs () << inv << "\n";
      }
      outs () <<  "\n";
    }
  }

  void CrabLlvm::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();
#ifdef HAVE_DSA
    AU.addRequiredTransitive<llvm::SteensgaardDataStructures> ();
    AU.addRequired<llvm::DataLayoutPass>();
    AU.addRequired<llvm::UnifyFunctionExitNodes> ();
#endif 
  } 

} // end namespace 

static llvm::RegisterPass<crab_llvm::CrabLlvm> 
X ("crab-llvm",
   "Infer invariants using Crab", 
   false, false);
   


