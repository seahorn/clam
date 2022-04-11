#pragma once

#include <crab/domains/intervals.hpp>
#include "crab_defs.hh"

namespace clam {
using BASE(interval_domain_t) = ikos::interval_domain<number_t, region_subdom_varname_t>;
using interval_domain_t = RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(interval_domain_t))));
} // end namespace clam
