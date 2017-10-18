#ifndef __ABSTRACT_DOMAINS_HH__
#define __ABSTRACT_DOMAINS_HH__

#include "crab_llvm/crab_cfg.hh"

#include "crab/domains/linear_constraints.hpp"                     
#include "crab/domains/intervals.hpp"                      
#include "crab/domains/dis_intervals.hpp"                      
#include "crab/domains/sparse_dbm.hpp"
#include "crab/domains/split_dbm.hpp"
#include "crab/domains/boxes.hpp"
#include "crab/domains/apron_domains.hpp"
//#include "crab/domains/array_sparse_graph.hpp"
#include "crab/domains/array_smashing.hpp"
#include "crab/domains/term_equiv.hpp"
//#include "crab/domains/nullity.hpp"
#include "crab/domains/flat_boolean_domain.hpp"
#include "crab/domains/combined_domains.hpp"
//#include "crab/domains/diff_domain.hpp" /* only debugging */

/*
   Definition of the abstract domains (no instantiation done here)
*/

namespace crab_llvm {

  using namespace crab::domains;
  using namespace ikos;

  /// --- Types for linear constraints and expressions
  typedef ikos::linear_expression<z_number, varname_t> z_lin_exp_t;
  typedef ikos::linear_constraint<z_number, varname_t> z_lin_cst_t;
  typedef ikos::linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;

  /* BEGIN MACROS only for internal use */
  // The base numerical domain 
  #define BASE(DOM) base_ ## DOM
  // Array functor domain where the base domain is a reduced product
  // of a boolean domain with the numerical domain DOM.
  #define ARRAY_BOOL_NUM(DOM) \
    typedef array_smashing<flat_boolean_numerical_domain<BASE(DOM)> > DOM
  // Array functor domain where the base domain is DOM
  #define ARRAY_NUM(DOM) \
    typedef array_smashing<BASE(DOM)> DOM;
  /* END MACROS only for internal use */
  
  //////
  // Base domains
  //////
  
  /// -- Intervals
  #if 1
  typedef interval_domain<z_number, varname_t> BASE(interval_domain_t);
  #else
  // -- enable apron version for more precise backward operations
  typedef apron_domain<z_number, varname_t, apron_domain_id_t::APRON_INT>
  BASE(interval_domain_t);
  #endif 
  /// -- Zones using sparse DBMs (SAS'16)
  typedef SpDBM_impl::DefaultParams<z_number> SparseDBMGraph;
  typedef SparseDBM<z_number, varname_t, SparseDBMGraph> BASE(dbm_domain_t);
  /// -- Zones using sparse DBMs in split normal form (SAS'16)
  typedef SDBM_impl::DefaultParams<z_number> SplitDBMGraph;
  typedef SplitDBM<z_number, varname_t, SplitDBMGraph> BASE(split_dbm_domain_t);
  /// -- Boxes
  typedef boxes_domain<z_number, varname_t> BASE(boxes_domain_t);
  // typedef diff_domain<flat_boolean_numerical_domain<BASE(interval_domain_t)>,
  // 		         boxes_domain<z_number, varname_t> > BASE(boxes_domain_t);
  /// -- DisIntervals
  typedef dis_interval_domain <z_number, varname_t> BASE(dis_interval_domain_t);
  /// -- Elina/Apron domains
  typedef apron_domain<z_number, varname_t, apron_domain_id_t::APRON_OPT_OCT>
  BASE(opt_oct_apron_domain_t);
  typedef apron_domain<z_number, varname_t, apron_domain_id_t::APRON_PK>
  BASE(pk_apron_domain_t);
  /// -- Reduced product of intervals with congruences
  typedef numerical_congruence_domain<BASE(interval_domain_t)> BASE(ric_domain_t);
  /// -- Term functor domain with Intervals (VMCAI'16)
  typedef crab::cfg::var_factory_impl::str_var_alloc_col::varname_t str_varname_t;
  typedef interval_domain<z_number, str_varname_t> str_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef term_domain<idom_info> BASE(term_int_domain_t);
  /// -- Term functor domain with DisIntervals (VMCAI'16)
  typedef dis_interval_domain<z_number, str_varname_t> str_dis_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_dis_interval_dom_t> dis_idom_info;
  typedef term_domain<dis_idom_info> BASE(term_dis_int_domain_t);
  /// -- Reduced product of Term(DisIntervals) with split zones
  typedef reduced_numerical_domain_product2<BASE(term_dis_int_domain_t),
					    BASE(split_dbm_domain_t)> BASE(num_domain_t);

  ARRAY_BOOL_NUM(interval_domain_t);
  ARRAY_BOOL_NUM(dbm_domain_t);
  ARRAY_BOOL_NUM(split_dbm_domain_t);
  ARRAY_BOOL_NUM(dis_interval_domain_t);
  ARRAY_BOOL_NUM(opt_oct_apron_domain_t);
  ARRAY_BOOL_NUM(pk_apron_domain_t);
  ARRAY_BOOL_NUM(ric_domain_t);
  ARRAY_BOOL_NUM(term_int_domain_t);  
  ARRAY_BOOL_NUM(term_dis_int_domain_t);  
  ARRAY_BOOL_NUM(num_domain_t);
  // Boxes can reason natively about booleans so that's why we don't
  // combine it with a boolean domain.
  ARRAY_NUM(boxes_domain_t);
  
} // end namespace crab-llvm

#endif
