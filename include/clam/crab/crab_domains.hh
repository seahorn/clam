#pragma once

#include "clam/crab/crab_cfg.hh"
#include "crab/config.h"
#include "crab/domains/linear_constraints.hpp"                     
#include "crab/domains/intervals.hpp"                      
#include "crab/domains/dis_intervals.hpp"                      
#include "crab/domains/split_dbm.hpp"
#include "crab/domains/boxes.hpp"
#ifdef HAVE_APRON
#include "crab/domains/apron_domains.hpp"
#else 
#include "crab/domains/elina_domains.hpp"
#endif 
#include "crab/domains/array_smashing.hpp"
#include "crab/domains/array_adaptive.hpp"
#include "crab/domains/term_equiv.hpp"
#include "crab/domains/flat_boolean_domain.hpp"
#include "crab/domains/combined_domains.hpp"
#include "crab/domains/wrapped_interval_domain.hpp"

/*
   Definition of the abstract domains (no instantiation done here)
*/

namespace clam {

  using namespace crab::domains;
  using namespace ikos;

#ifdef HAVE_ARRAY_ADAPT
  class ArrayAdaptParams {
  public:
    /* options for array smashing */  
    enum { is_smashable = 1 };
    enum { smash_at_nonzero_offset = 1};
    enum { max_smashable_cells = 64};
    /* options for array expansion */  
    enum { max_array_size = 512 };
  };
  template<class Dom>
  using array_domain = array_adaptive_domain<Dom, ArrayAdaptParams>;
#else
  template<class Dom>
  using array_domain = array_smashing<Dom>;
#endif   
  
  /* BEGIN MACROS only for internal use */
  // The base numerical domain 
  #define BASE(DOM) base_ ## DOM
  // Array functor domain where the base domain is a reduced product
  // of a boolean domain with the numerical domain DOM.
  #define ARRAY_BOOL_NUM(DOM) \
    typedef array_domain<flat_boolean_numerical_domain<BASE(DOM)>> DOM
  // Array functor domain where the base domain is DOM
  #define ARRAY_NUM(DOM) \
    typedef array_domain<BASE(DOM)> DOM
  /* END MACROS only for internal use */
  
  //////
  // Base domains
  //////
  
  /// -- Intervals
  typedef interval_domain<number_t, varname_t> BASE(interval_domain_t);
  /// -- Wrapped interval domain (APLAS'12)
  typedef wrapped_interval_domain<number_t, varname_t> BASE(wrapped_interval_domain_t);
  /// To choose DBM parameters
  struct BigNumDBMParams{
    /* This version uses unlimited integers so no overflow */
    enum { chrome_dijkstra = 1 };
    enum { widen_restabilize = 1 };
    enum { special_assign = 1 };
    enum { close_bounds_inline = 0 };	 
    typedef ikos::z_number Wt;
    typedef crab::SparseWtGraph<Wt> graph_t;
  };
  struct SafeFastDBMParams{
    /* This version checks for overflow and raise error if detected*/
    enum { chrome_dijkstra = 1 };
    enum { widen_restabilize = 1 };
    enum { special_assign = 1 };
    enum { close_bounds_inline = 0 };	 
    typedef crab::safe_i64 Wt;
    typedef crab::AdaptGraph<Wt> graph_t;
  };
  struct FastDBMParams{
    /* This version does not check for overflow */
    enum { chrome_dijkstra = 1 };
    enum { widen_restabilize = 1 };
    enum { special_assign = 1 };
    enum { close_bounds_inline = 0 };	 
    typedef int64_t Wt;
    typedef crab::AdaptGraph<Wt> graph_t;
  };
  /// -- Zones using sparse DBMs in split normal form (SAS'16)
#ifdef USE_DBM_BIGNUM
  typedef SplitDBM<number_t, varname_t, BigNumDBMParams> BASE(split_dbm_domain_t);
#else
#ifdef USE_DBM_SAFEINT
  typedef SplitDBM<number_t, varname_t, SafeFastDBMParams> BASE(split_dbm_domain_t);
#else
  typedef SplitDBM<number_t, varname_t, FastDBMParams> BASE(split_dbm_domain_t);
#endif
#endif 
  /// -- Boxes
  typedef boxes_domain<number_t, varname_t> BASE(boxes_domain_t);
  /// -- DisIntervals
  typedef dis_interval_domain <number_t, varname_t> BASE(dis_interval_domain_t);
  #ifdef HAVE_APRON
  /// -- Apron domains
  typedef apron_domain<number_t, varname_t, apron_domain_id_t::APRON_OCT>
  BASE(oct_domain_t);
  typedef apron_domain<number_t, varname_t, apron_domain_id_t::APRON_PK>
  BASE(pk_domain_t);
  #else
  /// -- Elina domains
  typedef elina_domain<number_t, varname_t, elina_domain_id_t::ELINA_OCT>
  BASE(oct_domain_t);
  typedef elina_domain<number_t, varname_t, elina_domain_id_t::ELINA_PK>
  BASE(pk_domain_t);
  #endif 
  /// -- Reduced product of intervals with congruences
  typedef numerical_congruence_domain<BASE(interval_domain_t)> BASE(ric_domain_t);
  /// -- Term functor domain with Intervals (VMCAI'16)
  typedef crab::cfg::var_factory_impl::str_var_alloc_col::varname_t str_varname_t;
  typedef interval_domain<number_t, str_varname_t> str_interval_dom_t;
  typedef term::TDomInfo<number_t, varname_t, str_interval_dom_t> idom_info;
  typedef term_domain<idom_info> BASE(term_int_domain_t);
  /// -- Term functor domain with DisIntervals (VMCAI'16)
  typedef dis_interval_domain<number_t, str_varname_t> str_dis_interval_dom_t;
  typedef term::TDomInfo<number_t, varname_t, str_dis_interval_dom_t> dis_idom_info;
  typedef term_domain<dis_idom_info> BASE(term_dis_int_domain_t);
  /// -- Reduced product of Term(DisIntervals) with split zones
  typedef reduced_numerical_domain_product2<BASE(term_dis_int_domain_t),
					    BASE(split_dbm_domain_t),
					    reduced_product_impl::term_dbm_params> BASE(num_domain_t);

  ARRAY_BOOL_NUM(interval_domain_t);
  ARRAY_BOOL_NUM(split_dbm_domain_t);
  ARRAY_BOOL_NUM(dis_interval_domain_t);
  ARRAY_BOOL_NUM(oct_domain_t);
  ARRAY_BOOL_NUM(pk_domain_t);
  ARRAY_BOOL_NUM(ric_domain_t);
  ARRAY_BOOL_NUM(term_int_domain_t);  
  ARRAY_BOOL_NUM(term_dis_int_domain_t);  
  ARRAY_BOOL_NUM(num_domain_t);
  // Boxes can reason natively about booleans so that's why we don't
  // combine it with a boolean domain.
  ARRAY_NUM(boxes_domain_t);
  /* domains that preserve machine arithmetic semantics */
  ARRAY_BOOL_NUM(wrapped_interval_domain_t);

} // end namespace crab-llvm
