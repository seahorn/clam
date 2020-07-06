#include "clam/RegisterAnalysis.hh"
#include "clam/crab/crab_domains.hh"
namespace clam {
template<>						        
clam_abstract_domain DomainRegistry::makeTopDomain<term_int_domain_t>() {	
   term_int_domain_t dom_val;						        
   clam_abstract_domain res(std::move(dom_val));		
   return res;							
}
} //end namespace
