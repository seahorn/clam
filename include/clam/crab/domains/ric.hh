#pragma once

#include <clam/crab/domains/intervals.hh>
namespace clam {
using namespace crab::domains;
using namespace ikos;
using BASE(ric_domain_t) = numerical_congruence_domain<BASE(interval_domain_t)>;
using ric_domain_t = REF_FUN(ARRAY_FUN(BOOL_NUM(BASE(ric_domain_t))));  
} // end namespace clam

