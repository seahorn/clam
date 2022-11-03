#include <clam/config.h>
#include <crab/config.h>
#include <clam/RegisterAnalysis.hh>
#include "crab/domains/register_domains.hh"

namespace clam {

void DomainRegistry::registerAllDomains() {
  register_interval_domain();
  register_ric_domain();
  register_wrapped_interval_domain();
  register_dis_interval_domain();
  register_boxes_domain();
  register_split_dbm_domain();
  register_split_oct_domain();  
  register_term_int_domain();
  register_term_dis_int_domain();
  register_num_domain();
  register_oct_domain();
  register_pk_domain();
  register_pk_pplite_domain();  
  register_sign_constant_domain();  
}

bool DomainRegistry::count(CrabDomain::Type dom_ty) {
  auto &map = getFactoryMap();
  return map.find(dom_ty) != map.end();
}

clam_abstract_domain DomainRegistry::at(CrabDomain::Type dom_ty) {
  auto &map = getFactoryMap();
  return map.at(dom_ty);
}

DomainRegistry::FactoryMap &DomainRegistry::getFactoryMap() {
  static FactoryMap map;
  return map;
}
} // end namespace clam
