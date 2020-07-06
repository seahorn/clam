#include "clam/config.h"
#include "clam/RegisterAnalysis.hh"
#include "clam/crab/crab_domains.hh"
namespace clam {
#if defined(HAVE_APRON) || defined(HAVE_ELINA)
template<>						        
clam_abstract_domain DomainRegistry::makeTopDomain<oct_domain_t>() {	
   oct_domain_t dom_val;						        
   clam_abstract_domain res(std::move(dom_val));		
   return res;							
 }
#endif   
} //end namespace
