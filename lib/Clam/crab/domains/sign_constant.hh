#pragma once

#include <crab/domains/sign_constant_domain.hpp>
#include "crab_defs.hh"

namespace clam {
using BASE(sign_constant_domain_t) =
  crab::domains::sign_constant_domain<number_t, region_subdom_varname_t>;
using sign_constant_domain_t =
  RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(sign_constant_domain_t))));
} // end namespace clam
