#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include "terms_dis_intervals.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
REGISTER_DOMAIN(clam::CrabDomain::TERMS_DIS_INTERVALS, term_dis_int_domain)
#else
UNREGISTER_DOMAIN(term_dis_int_domain)
#endif
} // end namespace clam
  
