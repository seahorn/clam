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
#include "crab/domains/array_smashing.hpp"
#include "crab/domains/term_equiv.hpp"
#include "crab/domains/combined_domains.hpp"

/*
   Definition of the abstract domains
*/


namespace crab_llvm {

   ////
   // Base (non-array) numerical domains for user options
   ////

   enum CrabDomain
     {   INTERVALS
       , INTERVALS_CONGRUENCES
       , BOXES
       , DIS_INTERVALS
       , ZONES_SPARSE_DBM
       , ZONES_SPLIT_DBM
       , TERMS_INTERVALS
       , TERMS_DIS_INTERVALS
       , TERMS_ZONES // TERMS_INTERVALS x  ZONES_SPLIT_DBM
       , ADAPT_TERMS_ZONES // (#live vars < threshold ? TERMS_INTERVALSxZONES_SPLIT_DBM, INTERVALS)
       , OPT_OCT_APRON
       , PK_APRON
     };

  //////
  /// Definition of the abstract domains
  //////

  using namespace crab::domains;
  using namespace ikos;

  /// --- Types for linear constraints and expressions
  typedef ikos::linear_expression<z_number, varname_t> z_lin_exp_t;
  typedef ikos::linear_constraint<z_number, varname_t> z_lin_cst_t;
  typedef ikos::linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;

  //////
  //// Base domains
  //////

  /// -- Intervals
  typedef interval_domain< z_number, varname_t> interval_domain_t;
  /// -- Zones with sparse DBM
  typedef SpDBM_impl::DefaultParams<z_number> SparseDBMGraph;
  typedef SparseDBM<z_number, varname_t, SparseDBMGraph> dbm_domain_t;
  /// -- Zones with split DBM
  typedef SDBM_impl::DefaultParams<z_number> SplitDBMGraph;
  typedef SplitDBM<z_number, varname_t, SplitDBMGraph> split_dbm_domain_t;
  /// -- Boxes
  typedef boxes_domain<z_number, varname_t> boxes_domain_t;
  /// -- DisIntervals
  typedef dis_interval_domain <z_number, varname_t> dis_interval_domain_t;
  /// -- Apron domains
  typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_OPT_OCT > opt_oct_apron_domain_t;
  typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_PK > pk_apron_domain_t;

  //////
  /// Combination/functor of domains 
  //////

  /// -- Reduced product of intervals with congruences
  typedef numerical_congruence_domain<interval_domain_t> ric_domain_t;
  /// -- Term functor domain with Intervals
  typedef crab::cfg::var_factory_impl::str_var_alloc_col::varname_t str_varname_t;
  typedef interval_domain<z_number, str_varname_t> str_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef term_domain<idom_info> term_int_domain_t;  
  /// -- Term functor domain with DisIntervals
  typedef dis_interval_domain<z_number, str_varname_t> str_dis_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_dis_interval_dom_t> dis_idom_info;
  typedef term_domain<dis_idom_info> term_dis_int_domain_t;  
  /// -- Reduced product of Term(DisIntervals) with split zones
  typedef reduced_numerical_domain_product2<term_dis_int_domain_t, split_dbm_domain_t> num_domain_t; 
  /// -- Array smashing functor domain 
  typedef array_smashing<interval_domain_t> arr_interval_domain_t;
  typedef array_smashing<ric_domain_t> arr_ric_domain_t;
  typedef array_smashing<dbm_domain_t> arr_dbm_domain_t;
  typedef array_smashing<split_dbm_domain_t> arr_split_dbm_domain_t;
  typedef array_smashing<term_int_domain_t> arr_term_int_domain_t;
  typedef array_smashing<term_dis_int_domain_t> arr_term_dis_int_domain_t;
  typedef array_smashing<boxes_domain_t> arr_boxes_domain_t;
  typedef array_smashing<dis_interval_domain_t> arr_dis_interval_domain_t;
  typedef array_smashing<num_domain_t> arr_num_domain_t;
  typedef array_smashing<opt_oct_apron_domain_t> arr_opt_oct_apron_domain_t;
  typedef array_smashing<pk_apron_domain_t> arr_pk_apron_domain_t;

} // end namespace crab-llvm

#endif
