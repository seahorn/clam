#pragma once

#include <clam/crab/crab_defs.hh>
#include <crab/config.h>
#include <crab/domains/dis_intervals.hpp>

namespace clam {
using namespace crab::domains;
using BASE(dis_interval_domain_t) = dis_interval_domain <number_t, dom_varname_t>;
using dis_interval_domain_t = REF_FUN(ARRAY_FUN(BOOL_NUM(BASE(dis_interval_domain_t))));  
} // end namespace clam

