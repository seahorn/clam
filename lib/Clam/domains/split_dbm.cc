#include "clam/crab/domains/split_dbm.hh"
#include "clam/RegisterAnalysis.hh"
namespace clam {
template <>
clam_abstract_domain DomainRegistry::makeTopDomain<split_dbm_domain_t>() {
  split_dbm_domain_t dom_val;
  clam_abstract_domain res(std::move(dom_val));
  return res;
}
} // namespace clam
