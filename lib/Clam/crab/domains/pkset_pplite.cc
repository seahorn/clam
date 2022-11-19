#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include <crab/config.h>
#include "pkset_pplite.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
#ifdef HAVE_PPLITE
REGISTER_DOMAIN(clam::CrabDomain::PKSET_PPLITE, pkset_pplite_domain)
#else
UNREGISTER_DOMAIN(pkset_pplite_domain)
#endif
#else
UNREGISTER_DOMAIN(pkset_pplite_domain)
#endif
} // end namespace clam

