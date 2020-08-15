#pragma once

#include <clam/crab/domains/intervals.hh>
namespace clam {
using term_int_domain_t =
    RGN_FUN(ARRAY_FUN(BOOL_NUM(TERM_FUN(BASE(interval_domain_t)))));
} // end namespace clam
