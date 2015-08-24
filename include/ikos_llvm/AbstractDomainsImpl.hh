#ifndef __ABSTRACT_DOMAINS_IMPL_HH__
#define __ABSTRACT_DOMAINS_IMPL_HH__

#include "ikos_llvm/config.h"
#include "ikos_llvm/CfgBuilder.hh"
#include "ikos_llvm/Support/AbstractDomains.hh"

/*

 Implementations of the abstract domains choosing a particular number
 and variable type.

*/

namespace domain_impl
{
  using namespace cfg_impl;
  using namespace ikos;

  // Numerical domains
  typedef interval_domain< z_number, varname_t > interval_domain_t;
#if IKOS_MINOR_VERSION >= 2
  // scalar versions
  typedef interval_congruence_domain< z_number, varname_t > ric_domain_t;
  typedef DBM< z_number, varname_t > dbm_domain_t;
  typedef octagon< z_number, varname_t > octagon_domain_t;
  //typedef ikos::term::TDomInfo<z_number, varname_t, interval_domain_t> idom_info;
  typedef interval_domain< z_number, cfg::var_factory_impl::StrVarAlloc_col::varname_t > str_interval_dom_t;
  typedef ikos::term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef anti_unif<idom_info>::anti_unif_t term_domain_t;  
  // array versions
  typedef array_smashing<interval_domain_t,z_number,varname_t> arr_interval_domain_t;
  typedef array_smashing<ric_domain_t,z_number,varname_t> arr_ric_domain_t;
  typedef array_smashing<dbm_domain_t,z_number,varname_t> arr_dbm_domain_t;
  typedef array_smashing<octagon_domain_t,z_number,varname_t> arr_octagon_domain_t;
  typedef array_smashing<term_domain_t,z_number,varname_t> arr_term_domain_t;
#endif
  typedef ikos::linear_expression<z_number, varname_t> z_lin_exp_t;
  typedef ikos::linear_constraint<z_number, varname_t> z_lin_cst_t;
  typedef ikos::linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;

  template<typename AbsDomain>
  z_lin_cst_sys_t toLinCst (AbsDomain inv)
  {
#if IKOS_MINOR_VERSION >= 2
    return inv.to_linear_constraint_system ();
#else
    return intervals_traits::to_linear_constraint_system (inv);
#endif       
  }

} // end namespace

#endif
