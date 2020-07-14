#pragma once

#include <clam/crab/domains/terms_dis_intervals.hh>
#include <clam/crab/domains/split_dbm.hh>

namespace clam {
/// -- Reduced product of term(dis_intervals) and zones
using BASE(num_domain_t) = reduced_numerical_domain_product2
  <TERM_FUN(BASE(dis_interval_domain_t)), BASE(split_dbm_domain_t),
   reduced_product_impl::term_dbm_params>;
using num_domain_t = REF_FUN(ARRAY_FUN(BOOL_NUM(BASE(num_domain_t))));  
} // end namespace clam

