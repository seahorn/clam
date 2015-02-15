#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Support/CFG.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "ikos/CfgBuilder.hh"
#include "ikos/LlvmIkos.hh"
#include "ikos/Support/AbstractDomains.hh"

#include "boost/range.hpp"
#include "boost/scoped_ptr.hpp"

#include <ikos_analysis/FwdAnalyzer.hpp>
#include <ikos_domains/intervals.hpp>                      
#include <ikos_domains/intervals_congruences.hpp>                      
#include <ikos_domains/octagons.hpp>                      
#include <ikos_domains/dbm.hpp>                      

using namespace llvm;

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
    //LOG ( "ikos-verbose" , errs () << "IKOS: Processing a module\n");

    bool change=false;

    for (auto &f : M) 
    {change |= runOnFunction (f); }

    return change;
  }

  bool LlvmIkos::runOnFunction (llvm::Function &F)
  {
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    VariableFactory vfac; 

    //LOG ("ikos-cfg", errs () << "Cfg: \n");    
    cfg_t cfg = CfgBuilder (F, vfac)();
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
      boost::optional<ZLinearConstraintSystem> csts = inv.to_linear_constraint_system ();
      const llvm::BasicBlock *BB = &B;
      if (csts)
        m_inv_map.insert (make_pair (BB, *csts));
      //else
      //  m_inv_map.insert (make_pair (BB, mkFALSE ()));
      else
        m_inv_map.insert (make_pair (BB, mkTRUE ()));
    }

    return false;
  }

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


