#include "clam/crab/domains/terms_zones.hh"
#include "clam/RegisterAnalysis.hh"
namespace clam {
template <> clam_abstract_domain DomainRegistry::makeTopDomain<num_domain_t>() {
  num_domain_t dom_val;
  clam_abstract_domain res(std::move(dom_val));
  return res;
}
} // namespace clam
