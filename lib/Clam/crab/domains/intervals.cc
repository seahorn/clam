#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include "intervals.hh"


namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
REGISTER_DOMAIN(clam::CrabDomain::INTERVALS, interval_domain)
#else
UNREGISTER_DOMAIN(interval_domain)
#endif  
} // end namespace clam
