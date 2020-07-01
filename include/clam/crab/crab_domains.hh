#pragma once

// AG: Something is wrong. Formatting breaks compilation
// clang-format off

#include "crab/config.h"

#include <clam/crab/crab_lang.hh>

#include <crab/domains/apron_domains.hpp>
#include <crab/domains/array_adaptive.hpp>
#include <crab/domains/array_smashing.hpp>
#include <crab/domains/boxes.hpp>
#include <crab/domains/combined_domains.hpp>
#include <crab/domains/dis_intervals.hpp>
#include <crab/domains/elina_domains.hpp>
#include <crab/domains/flat_boolean_domain.hpp>
#include <crab/domains/intervals.hpp>
#include <crab/domains/reference_domain.hpp>
#include <crab/domains/split_dbm.hpp>
#include <crab/domains/term_equiv.hpp>
#include <crab/domains/wrapped_interval_domain.hpp>


/**
 * Definition of the abstract domains.
 *
 * No instantiation should happen here.
 * 
 * All clam domains consist of fixed combination of functor and
 * product domains where the only tunable parameter is the base
 * numerical domain. The names of the domains listed below reflect the
 * name of the base numerical domain:
 * 
 * - interval_domain_t
 * - wrapped_interval_domain_t
 * - ric_domain_t
 * - split_dbm_domain_t
 * - dis_interval_domain_t
 * - oct_domain_t
 * - pk_domain_t
 * - term_int_domain_t
 * - term_dis_int_domain_t
 * - num_domain_t
 * - boxes_domain_t
 * 
 **/

namespace clam {
using namespace crab::domains;
using namespace ikos;

/* ====================================================================== */  
/* BEGIN MACROS to create the hierarchy of domains. Only for internal use */
/* ====================================================================== */    
// The base numerical domain 
#define BASE(DOM) base_ ## DOM
// Term functor domain 
#define TERM_FUNCTOR(DOM) \
  term_domain<term::TDomInfo<number_t, dom_varname_t, DOM>>
// Reduced product of boolean domain with numerical domain
#define BOOL_NUM(DOM) flat_boolean_numerical_domain<DOM>
// Array functor domain where the parameter domain is DOM
#define ARRAY_FUNCTOR(DOM) array_adapt_domain<DOM>
// Reference functor domain -- the root of the hierarchy of domains.
#define REF_FUNCTOR(DOM) \
  reference_domain<reference_domain_impl::Params<z_number, varname_t, DOM>>
/* ====================================================================== */    
/* END MACROS to create the hierarchy of domains. Only for internal use */
/* ====================================================================== */    


/* BEGIN array domains */    
class ArrayAdaptParams {
public:
 /* options for array smashing */  
  enum { is_smashable = 1 };
  enum { smash_at_nonzero_offset = 1};
  enum { max_smashable_cells = 1024};
  /* options for array expansion */  
  enum { max_array_size = 1024 };
};
template<class Dom>
using array_adapt_domain = array_adaptive_domain<Dom, ArrayAdaptParams>;
  
template<class Dom>
using array_smashing_domain = array_smashing<Dom>;

/* END array domains */      
  
/* BEGIN base numerical domains */
using domvar_allocator = crab::var_factory_impl::str_var_alloc_col;
using dom_varname_t = domvar_allocator::varname_t;  
/// -- Intervals
using BASE(interval_domain_t) = interval_domain<number_t, dom_varname_t>;
/// -- Wrapped interval domain 
using BASE(wrapped_interval_domain_t) = wrapped_interval_domain<number_t, dom_varname_t>;
/// To choose DBM parameters
struct BigNumDBMParams{
  /* This version uses unlimited integers so no overflow */
  enum { chrome_dijkstra = 1 };
  enum { widen_restabilize = 1 };
  enum { special_assign = 1 };
  enum { close_bounds_inline = 0 };	 
  using Wt = ikos::z_number;
  using graph_t = crab::SparseWtGraph<Wt>;
};
struct SafeFastDBMParams{
  /* This version checks for overflow and raise error if detected*/
  enum { chrome_dijkstra = 1 };
  enum { widen_restabilize = 1 };
  enum { special_assign = 1 };
  enum { close_bounds_inline = 0 };	 
  using Wt = crab::safe_i64;
  using graph_t = crab::AdaptGraph<Wt>;
};
struct FastDBMParams{
  /* This version does not check for overflow */
  enum { chrome_dijkstra = 1 };
  enum { widen_restabilize = 1 };
  enum { special_assign = 1 };
  enum { close_bounds_inline = 0 };	 
  using Wt = int64_t;
  using graph_t = crab::AdaptGraph<Wt>;
};
  /// -- Zones 
#ifdef USE_DBM_BIGNUM
using BASE(split_dbm_domain_t) = split_dbm_domain<number_t, dom_varname_t, BigNumDBMParams>;
#else
#ifdef USE_DBM_SAFEINT
using BASE(split_dbm_domain_t) = split_dbm_domain<number_t, dom_varname_t, SafeFastDBMParams>;
#else
using BASE(split_dbm_domain_t) = split_dbm_domain<number_t, dom_varname_t, FastDBMParams>;
#endif
#endif 
/// -- Boxes
using BASE(boxes_domain_t) = boxes_domain<number_t, dom_varname_t>;
/// -- DisIntervals
using BASE(dis_interval_domain_t) = dis_interval_domain <number_t, dom_varname_t>;
#ifdef HAVE_APRON  
/// -- Apron domains
using BASE(oct_domain_t) = apron_domain<number_t, dom_varname_t, apron_domain_id_t::APRON_OCT>;
using BASE(pk_domain_t) = apron_domain<number_t, dom_varname_t, apron_domain_id_t::APRON_PK>;
#else   
/// -- Elina domains
using BASE(oct_domain_t) = elina_domain<number_t, dom_varname_t, elina_domain_id_t::ELINA_OCT>;
using BASE(pk_domain_t) = elina_domain<number_t, dom_varname_t, elina_domain_id_t::ELINA_PK>;
#endif   
/// -- Reduced product of intervals with congruences
using BASE(ric_domain_t) = numerical_congruence_domain<BASE(interval_domain_t)>;
/// -- Reduced product of term(dis_intervals) and zones
using BASE(num_domain_t) = reduced_numerical_domain_product2
  <TERM_FUNCTOR(BASE(dis_interval_domain_t)), BASE(split_dbm_domain_t),
   reduced_product_impl::term_dbm_params>;
/* END base numerical domains */


// ======== The actual domains ======== //
using interval_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(interval_domain_t))));
using split_dbm_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(split_dbm_domain_t))));
using dis_interval_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(dis_interval_domain_t))));
using oct_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(oct_domain_t))));
using pk_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(pk_domain_t))));
using ric_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(ric_domain_t))));
using term_int_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(TERM_FUNCTOR(BASE(interval_domain_t)))));  
using term_dis_int_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(TERM_FUNCTOR(BASE(dis_interval_domain_t)))));
using num_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(num_domain_t))));
// Boxes can reason natively about booleans so that's why we don't
// combine it with a boolean domain.
using boxes_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BASE(boxes_domain_t)));
using wrapped_interval_domain_t = REF_FUNCTOR(ARRAY_FUNCTOR(BOOL_NUM(BASE(wrapped_interval_domain_t))));
} // end namespace clam

// clang-format on
