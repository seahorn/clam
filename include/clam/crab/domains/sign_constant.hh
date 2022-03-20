#pragma once

#include <clam/crab/crab_defs.hh>
#include <crab/config.h>
#include <crab/domains/sign_constant_domain.hpp>

namespace clam {
using BASE(sign_constant_domain_t) =
  crab::domains::sign_constant_domain<number_t, region_subdom_varname_t>;
using sign_constant_domain_t =
    RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(sign_constant_domain_t))));
} // end namespace clam
