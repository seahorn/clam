#pragma once

#include <crab/domains/wrapped_interval_domain.hpp>
#include "crab_defs.hh"

namespace clam {
using BASE(wrapped_interval_domain_t) =
  crab::domains::wrapped_interval_domain<number_t, region_subdom_varname_t>;
using wrapped_interval_domain_t =
  RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(wrapped_interval_domain_t))));
} // end namespace clam
