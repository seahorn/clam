#include <clam/config.h>
#include <clam/CrabDomain.hh>
#include <clam/RegisterAnalysis.hh>
#include <crab/config.h>
#include "boxes.hh"

namespace clam {
#ifdef INCLUDE_ALL_DOMAINS
#ifdef HAVE_LDD
REGISTER_DOMAIN(clam::CrabDomain::BOXES, boxes_domain)
#else
UNREGISTER_DOMAIN(boxes_domain)
#endif
#else
UNREGISTER_DOMAIN(boxes_domain)
#endif
} // end namespace clam
