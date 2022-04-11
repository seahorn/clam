#pragma once

#include <crab/domains/dis_intervals.hpp>
#include "crab_defs.hh"

namespace clam {
using BASE(dis_interval_domain_t) =
crab::domains::dis_interval_domain<number_t, region_subdom_varname_t>;
using dis_interval_domain_t =
    RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(dis_interval_domain_t))));
} // end namespace clam
