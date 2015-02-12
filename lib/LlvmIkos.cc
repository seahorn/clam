// #include "avy/AvyDebug.h"
// #include "seahorn/SymStore.hh"
// #include "seahorn/SymExec.hh"
// #include "seahorn/Support/CFG.hh"
// #include "seahorn/LiveSymbols.hh"
// #include "seahorn/Ikos/Ikos.hh"
// #include "seahorn/Ikos/Ikos_Common.hh"
// #include "seahorn/Ikos/Ikos_Analyzer.hh"
// #include "seahorn/Ikos/Ikos_Domains.hh"
// #include "seahorn/Ikos/Ikos_Transformations.hh"
// #include "seahorn/BoostLlvmGraphTraits.hh"

// #include "seahorn/HornifyFunction.hh"

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Support/CFG.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "include/CfgBuilder.hh"
#include "include/LlvmIkos.hh"

//#include "ufo/Passes/NameValues.hpp"

#include "boost/range.hpp"
#include "boost/scoped_ptr.hpp"

//#include "ufo/Stats.hh"

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

  char LlvmIkos::ID = 0;

  bool LlvmIkos::runOnModule (llvm::Module &M)
  {
    //LOG ( "ikos-verbose" , errs () << "IKOS: Processing a module\n");
    bool change=false;
    for (auto &f : M) { change |= runOnFunction (f); }
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
        change = runOnCfg<interval_domain_t> (cfg, F, vfac); break;
      case REDUCED_INTERVALS_CONGRUENCES: 
        change = runOnCfg<interval_congruence_domain_t> (cfg, F, vfac); break;
      case ZONES: 
        change = runOnCfg<dbm_domain_t> (cfg, F, vfac); break;
      case OCTAGONS: 
        change = runOnCfg<octagon_domain_t> (cfg, F, vfac); break;
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

}
