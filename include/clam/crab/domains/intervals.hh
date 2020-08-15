#pragma once

#include <clam/crab/crab_defs.hh>
#include <crab/config.h>
#include <crab/domains/intervals.hpp>

namespace clam {
using namespace ikos;
using BASE(interval_domain_t) = interval_domain<number_t, dom_varname_t>;
using interval_domain_t = RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(interval_domain_t))));
} // end namespace clam
