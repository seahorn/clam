#ifndef __ABSTRACT_DOMAINS_IMPL_HH__
#define __ABSTRACT_DOMAINS_IMPL_HH__

#include "crab_llvm/config.h"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/Support/AbstractDomains.hh"

/*

 Implementations of the abstract domains choosing a particular number
 and variable type.

*/

namespace domain_impl
{
  using namespace crab::cfg_impl;
  using namespace crab::domains;
  using namespace ikos;

  typedef ikos::linear_expression<z_number, varname_t> z_lin_exp_t;
  typedef ikos::linear_constraint<z_number, varname_t> z_lin_cst_t;
  typedef ikos::linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;

  typedef interval_domain< z_number, varname_t > interval_domain_t;
  typedef interval_congruence_domain< z_number, varname_t > ric_domain_t;
  typedef DBM< z_number, varname_t > dbm_domain_t;
  typedef naive_dbm <z_number, varname_t> naive_dbm_domain_t;
  typedef var_packing_naive_dbm <z_number, varname_t> cgs_dbm_domain_t;
  /// TERM variants 
  //typedef ikos::term::TDomInfo<z_number, varname_t, interval_domain_t> idom_info;
  typedef crab::cfg::var_factory_impl::StrVarAlloc_col::varname_t str_varname_t;

  typedef interval_domain< z_number, str_varname_t > str_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef anti_unif<idom_info>::anti_unif_t term_domain_t;  

  typedef var_packing_naive_dbm <z_number, str_varname_t> str_cgs_dbm_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_cgs_dbm_dom_t> cgs_dbm_dom_info;
  typedef anti_unif<cgs_dbm_dom_info>::anti_unif_t term_cgs_dbm_domain_t;  

  typedef DBM <z_number, str_varname_t> str_dbm_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_dbm_dom_t> dbm_dom_info;
  typedef anti_unif<dbm_dom_info>::anti_unif_t term_dbm_domain_t;  
  ///////////////////////

  typedef array_smashing<interval_domain_t,z_number,varname_t> arr_interval_domain_t;
  typedef array_smashing<ric_domain_t,z_number,varname_t> arr_ric_domain_t;
  typedef array_smashing<dbm_domain_t,z_number,varname_t> arr_dbm_domain_t;
  typedef array_smashing<term_domain_t,z_number,varname_t> arr_term_domain_t;

} // end namespace

#endif
