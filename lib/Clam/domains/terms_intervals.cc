#include "clam/crab/domains/terms_intervals.hh"
#include "clam/RegisterAnalysis.hh"
namespace clam {
template <>
clam_abstract_domain DomainRegistry::makeTopDomain<term_int_domain_t>() {
  term_int_domain_t dom_val;
  clam_abstract_domain res(std::move(dom_val));
  return res;
}
} // namespace clam
