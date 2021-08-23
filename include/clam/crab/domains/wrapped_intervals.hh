#pragma once

#include <clam/crab/crab_defs.hh>
#include <crab/config.h>
#include <crab/domains/wrapped_interval_domain.hpp>

namespace clam {
using BASE(wrapped_interval_domain_t) =
  crab::domains::wrapped_interval_domain<number_t, region_dom_varname_t>;
using wrapped_interval_domain_t =
  RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(wrapped_interval_domain_t))));
} // end namespace clam
