#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include "ric.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
REGISTER_DOMAIN(clam::CrabDomain::INTERVALS_CONGRUENCES, ric_domain)
#else
UNREGISTER_DOMAIN(ric_domain)
#endif
} // end namespace clam
