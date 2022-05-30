#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include "split_oct.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
REGISTER_DOMAIN(clam::CrabDomain::OCT_SPLIT_DBM, split_oct_domain)
#else
UNREGISTER_DOMAIN(split_oct_domain)
#endif
} // end namespace clam

