#pragma once

#include <crab/domains/apron_domains.hpp>
#include "crab_defs.hh"

namespace clam {
using BASE(pk_pplite_domain_t) =
  crab::domains::apron_domain<number_t, region_subdom_varname_t,
			      crab::domains::apron_domain_id_t::APRON_PPLITE_POLY>;
using pk_pplite_domain_t = RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(pk_pplite_domain_t))));
} // end namespace clam
