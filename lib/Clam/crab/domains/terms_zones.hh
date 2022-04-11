#pragma once

#include <crab/domains/dis_intervals.hpp>
#include "split_dbm.hh"

namespace clam {
/// -- Reduced product of term(dis_intervals) and zones
using str_dis_interval_domain_t = crab::domains::dis_interval_domain<number_t, str_varname_t>;
using BASE(num_domain_t) = reduced_numerical_domain_product2<
  TERM_FUN(str_dis_interval_domain_t),
  BASE(split_dbm_domain_t),
  reduced_product_impl::term_dbm_params>;
using num_domain_t = RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(num_domain_t))));
} // end namespace clam
