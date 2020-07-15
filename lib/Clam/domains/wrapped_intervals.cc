#include "clam/crab/domains/wrapped_intervals.hh"
#include "clam/RegisterAnalysis.hh"
namespace clam {
template <>
clam_abstract_domain
DomainRegistry::makeTopDomain<wrapped_interval_domain_t>() {
  wrapped_interval_domain_t dom_val;
  clam_abstract_domain res(std::move(dom_val));
  return res;
}
} // namespace clam
