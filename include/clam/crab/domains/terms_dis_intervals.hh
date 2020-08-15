#pragma once

#include <clam/crab/domains/dis_intervals.hh>
namespace clam {
using term_dis_int_domain_t =
    RGN_FUN(ARRAY_FUN(BOOL_NUM(TERM_FUN(BASE(dis_interval_domain_t)))));
} // end namespace clam
