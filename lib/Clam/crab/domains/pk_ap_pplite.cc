#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include <crab/config.h>
#include "pk_ap_pplite.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
#if defined(HAVE_APRON) && defined(HAVE_PPLITE)
REGISTER_DOMAIN(clam::CrabDomain::PK_AP_PPLITE, pk_ap_pplite_domain)
#else
UNREGISTER_DOMAIN(pk_ap_pplite_domain)
#endif
#else
UNREGISTER_DOMAIN(pk_ap_pplite_domain)
#endif
} // end namespace clam

