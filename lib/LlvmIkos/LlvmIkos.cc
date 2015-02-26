#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Support/CFG.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "ikos_llvm/CfgBuilder.hh"
#include "ikos_llvm/LlvmIkos.hh"
#include "ikos_llvm/Support/AbstractDomains.hh"

#include "boost/range.hpp"
#include "boost/scoped_ptr.hpp"

#include <ikos/analysis/FwdAnalyzer.hpp>
#include <ikos/domains/intervals.hpp>                      
#include <ikos/domains/intervals_congruences.hpp>                      
#include <ikos/domains/octagons.hpp>                      
#include <ikos/domains/dbm.hpp>                      

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
        clEnumValN (INTERVALS_CONGRUENCES, "cong",
                    "Reduced product of intervals with congruences"),
        clEnumValN (ZONES , "zones",
                    "Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (OCTAGONS, "oct",
                   "Octagon domain"),
        clEnumValEnd),
       llvm::cl::init (INTERVALS));

static llvm::cl::opt<bool>
RunLive("ikos-live", 
        llvm::cl::desc("Run Ikos with live ranges"),
        llvm::cl::init (false));

namespace domain_impl
{
  using namespace cfg_impl;
  using namespace ikos;

  // Numerical domains
  typedef interval_domain< z_number, varname_t >             interval_domain_t;
  typedef interval_congruence_domain< z_number, varname_t >  interval_congruence_domain_t;
  typedef DBM< z_number, varname_t >                         dbm_domain_t;
  typedef octagon< z_number, varname_t >                     octagon_domain_t;

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
    cfg_t cfg = CfgBuilder (F, vfac)();
    //errs () << cfg << "\n";
    //LOG ("ikos-cfg", errs () << cfg << "\n");    

    bool change=false;
    switch (m_absdom)
    {
      case INTERVALS: 
        change = runOnCfg <interval_domain_t> (cfg, F, vfac); 
        break;
      case INTERVALS_CONGRUENCES: 
        change = runOnCfg <interval_congruence_domain_t> (cfg, F, vfac); 
        break;
      case ZONES: 
        change = runOnCfg <dbm_domain_t> (cfg, F, vfac); 
        break;
      case OCTAGONS: 
        change = runOnCfg <octagon_domain_t> (cfg, F, vfac); 
        break;
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

    
    //LOG ("ikos-verbose", errs () << analyzer << "\n");
    for (auto &B : F)
    {
      AbsDomain inv = analyzer [&B];
      const llvm::BasicBlock *BB = &B;
      if (inv.is_bottom ())
        m_inv_map.insert (make_pair (BB, mkFALSE ()));
      else if (inv.is_top ())
        m_inv_map.insert (make_pair (BB, mkTRUE ()));        
      else
      {
        ZLinearConstraintSystem csts = inv.to_linear_constraint_system ();
        m_inv_map.insert (make_pair (BB, csts));
      }
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
} // end namespace llvm_ikos




static llvm::RegisterPass<llvm_ikos::LlvmIkos> 
X ("llvm-ikos",
   "Infer invariants using Ikos", false, false);


