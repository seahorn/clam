#include "clam/RegisterAnalysis.hh"
#include "clam/crab/domains/dis_intervals.hh"
namespace clam {
template<>						        
clam_abstract_domain DomainRegistry::makeTopDomain<dis_interval_domain_t>() {	
   dis_interval_domain_t dom_val;						        
   clam_abstract_domain res(std::move(dom_val));		
   return res;							
 }  
} //end namespace
