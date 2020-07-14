#pragma once

#include <clam/crab/crab_defs.hh>
#include <crab/config.h>
#include <crab/domains/wrapped_interval_domain.hpp>

namespace clam {
using namespace crab::domains;
using BASE(wrapped_interval_domain_t) = wrapped_interval_domain <number_t, dom_varname_t>;
using wrapped_interval_domain_t = REF_FUN(ARRAY_FUN(BOOL_NUM(BASE(wrapped_interval_domain_t))));  
} // end namespace clam
