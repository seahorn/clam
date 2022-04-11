#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include "dis_intervals.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
REGISTER_DOMAIN(clam::CrabDomain::DIS_INTERVALS, dis_interval_domain)
#else
UNREGISTER_DOMAIN(dis_interval_domain)
#endif
} // end namespace clam

