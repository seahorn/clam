#pragma once

#include <crab/domains/dis_intervals.hpp>
#include "crab_defs.hh"

namespace clam {
using str_dis_interval_domain_t =
  crab::domains::dis_interval_domain<number_t, str_varname_t>;
using term_dis_int_domain_t =
  RGN_FUN(ARRAY_FUN(BOOL_NUM(TERM_FUN(str_dis_interval_domain_t))));
} // end namespace clam
