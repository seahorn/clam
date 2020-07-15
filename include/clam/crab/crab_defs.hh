#pragma once

/**
 * Types and macros to define abstract domains.
 **/

// AG: Something is wrong. Formatting breaks compilation
// clang-format off

#include <crab/config.h>
#include <clam/crab/crab_lang.hh>

#include <crab/domains/array_adaptive.hpp>
#include <crab/domains/flat_boolean_domain.hpp>
#include <crab/domains/reference_domain.hpp>
#include <crab/domains/term_equiv.hpp>

namespace clam {
using namespace crab::domains;
using namespace ikos;

/* ====================================================================== */  
/* BEGIN MACROS to create the hierarchy of domains. Only for internal use */
/* ====================================================================== */    
// The base numerical domain 
#define BASE(DOM) base_ ## DOM
// Term functor domain 
#define TERM_FUN(DOM) \
  term_domain<term::TDomInfo<number_t, dom_varname_t, DOM>>
// Reduced product of boolean domain with numerical domain
#define BOOL_NUM(DOM) flat_boolean_numerical_domain<DOM>
// Array functor domain where the parameter domain is DOM
#define ARRAY_FUN(DOM) array_adapt_domain<DOM>
// Reference functor domain -- the root of the hierarchy of domains.
#define REF_FUN(DOM) \
  reference_domain<reference_domain_impl::Params<z_number, varname_t, DOM>>
/* ====================================================================== */    
/* END MACROS to create the hierarchy of domains. Only for internal use   */
/* ====================================================================== */    

/* ====================================================================== */      
/* BEGIN array adaptive domain                                            */
/* ====================================================================== */      
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
/* ====================================================================== */      
/* END array adaptive domain                                              */
/* ====================================================================== */      

/* ====================================================================== */        
/* BEGIN reference domain                                                 */
/* ====================================================================== */  
using domvar_allocator = crab::var_factory_impl::str_var_alloc_col;
using dom_varname_t = domvar_allocator::varname_t;
/* ====================================================================== */    
/* END reference domain                                                   */
/* ====================================================================== */    
} // end namespace clam
// clang-format on
