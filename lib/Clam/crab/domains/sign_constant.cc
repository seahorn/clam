#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include "sign_constant.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
REGISTER_DOMAIN(clam::CrabDomain::SIGN_CONSTANTS, sign_constant_domain)
#else
UNREGISTER_DOMAIN(sign_constant_domain)
#endif
} // end namespace clam

