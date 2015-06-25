#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/IR/CFG.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "ikos_llvm/config.h"
#include "ikos_llvm/LlvmIkos.hh"
#include "ikos_llvm/Support/AbstractDomains.hh"

#include <ikos/analysis/FwdAnalyzer.hpp>
#include <ikos/intervals.hpp>                      
#if IKOS_MINOR_VERSION >= 2
#include <ikos/domains/intervals_congruences.hpp>                      
#include <ikos/domains/octagons.hpp>                      
#include <ikos/domains/dbm.hpp>
#include <ikos/domains/term/term_util.hpp>
#include <ikos/domains/term_equiv.hpp>
#include <ikos/domains/array_smashing.hpp>
#else 
#include <ikos/intervals_traits.hpp>
#endif 

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;

static llvm::cl::opt<bool>
PrintAnswer ("ikos-answer", llvm::cl::desc ("Print Ikos invariants"),
             llvm::cl::init (false));

using namespace llvm_ikos;
static llvm::cl::opt<IkosDomain>
Domain("ikos-dom",
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

static llvm::cl::opt<bool>
RunLive("ikos-live", 
        llvm::cl::desc("Run Ikos with live ranges"),
        llvm::cl::init (false));

static llvm::cl::opt<enum TrackedPrecision>
TrackedLevel("ikos-track-lvl",
   llvm::cl::desc ("Track level for Cfg and abstract domains"),
   cl::values (clEnumValN (REG, "reg", "Primitive registers only"),
               clEnumValN (PTR, "ptr", "REG + pointers"),
               clEnumValN (MEM, "mem", "PTR + memory content"),
               clEnumValEnd),
   cl::init (TrackedPrecision::REG));

static llvm::cl::opt<bool>
InterProc ("ikos-inter-proc",
             cl::desc ("Build inter-procedural Cfg"), 
             cl::init (false));

namespace domain_impl
{
  using namespace cfg_impl;
  using namespace ikos;

  // Numerical domains
  typedef interval_domain< z_number, varname_t >             interval_domain_t;
#if IKOS_MINOR_VERSION >= 2
  typedef interval_congruence_domain< z_number, varname_t > congruence_domain_t;
  typedef DBM< z_number, varname_t > dbm_domain_t;
  typedef octagon< z_number, varname_t > octagon_domain_t;
  //typedef ikos::term::TDomInfo<z_number, varname_t, interval_domain_t> idom_info;
  typedef interval_domain< z_number, ikos::term::StrVarAlloc_col::varname_t > str_interval_dom_t;
  typedef ikos::term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef anti_unif<idom_info>::anti_unif_t term_domain_t;  
#endif

  template<typename AbsDomain>
  ikos::linear_constraint_system<z_number, varname_t> toLinCst (AbsDomain inv)
  {
#if IKOS_MINOR_VERSION >= 2
    return inv.to_linear_constraint_system ();
#else
    return intervals_traits::to_linear_constraint_system (inv);
#endif       
  }

} // end namespace

namespace llvm_ikos
{

  using namespace analyzer;
  using namespace domain_impl;

  char llvm_ikos::LlvmIkos::ID = 0;

  bool LlvmIkos::runOnModule (llvm::Module &M)
  {
    // -- initialize from cli options
    m_absdom = Domain;
    m_runlive = RunLive;

#ifdef HAVE_DSA
    m_mem = MemAnalysis (&getAnalysis<SteensgaardDataStructures> (),
                         TrackedLevel);
#endif     

    bool change=false;

    for (auto &f : M) 
    {change |= runOnFunction (f); }

    if (PrintAnswer) dump (M);
    return change;
  }

  bool LlvmIkos::runOnFunction (llvm::Function &F)
  {
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    VariableFactory vfac; 

    //LOG ("ikos-cfg", errs () << "Cfg: \n");    
    cfg_t cfg = CfgBuilder (F, vfac, &m_mem, InterProc)();
    //LOG ("ikos-cfg", errs () << cfg << "\n");  
    //errs () << cfg << "\n";

    bool change=false;
    switch (m_absdom)
    {
      case INTERVALS:  
#if IKOS_MINOR_VERSION >= 2
          change = (TrackedLevel >= MEM ? 
                    runOnCfg <array_smashing<interval_domain_t,z_number,varname_t> > 
                    (cfg, F, vfac) : 
                    runOnCfg <interval_domain_t> (cfg, F, vfac)) ; 
#else
          change = runOnCfg <interval_domain_t> (cfg, F, vfac); 
#endif 
        break;
#if IKOS_MINOR_VERSION >= 2
      case INTERVALS_CONGRUENCES: 
         change = (TrackedLevel >= MEM ? 
                  runOnCfg <array_smashing<congruence_domain_t,z_number,varname_t> > 
                  (cfg, F, vfac) : 
                  runOnCfg <congruence_domain_t> (cfg, F, vfac)) ; 
        break;
      case ZONES: 
         change = (TrackedLevel >= MEM ? 
                  runOnCfg <array_smashing<dbm_domain_t,z_number,varname_t> > 
                  (cfg, F, vfac) : 
                  runOnCfg <dbm_domain_t> (cfg, F, vfac)) ; 
        break;
      case OCTAGONS: 
        change = (TrackedLevel >= MEM ? 
                  runOnCfg <array_smashing<octagon_domain_t,z_number,varname_t> > 
                  (cfg, F, vfac) : 
                  runOnCfg <octagon_domain_t> (cfg, F, vfac)) ; 
        break;
      case TERMS:
        change = (TrackedLevel >= MEM ? 
                  runOnCfg <array_smashing<term_domain_t,z_number,varname_t> > 
                  (cfg, F, vfac) : 
                  runOnCfg <term_domain_t> (cfg, F, vfac)) ; 
        break;
#endif
      default: assert(false && "Unsupported abstract domain");
    }
    // LOG ("ikos-verbose", errs () << "Ikos is done!\n");
    return change;
  }

  template<typename AbsDomain>
  bool LlvmIkos::runOnCfg (cfg_t& cfg, llvm::Function &F, VariableFactory &vfac)
  {
    FwdAnalyzer< basic_block_label_t, varname_t, cfg_t, VariableFactory, AbsDomain > 
        analyzer (cfg, vfac, m_runlive);

    analyzer.Run (AbsDomain::top());
    for (auto &B : F)
    {
      AbsDomain inv = analyzer [&B];
      const llvm::BasicBlock *BB = &B;
      if (inv.is_bottom ())
        m_inv_map.insert (make_pair (BB, mkFALSE ()));
      else if (inv.is_top ())
        m_inv_map.insert (make_pair (BB, mkTRUE ()));        
      else
        m_inv_map.insert (make_pair (BB, toLinCst (inv)));
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
          auto inv = this->operator[] (BB);
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
   "Infer invariants using Ikos", false, false);


