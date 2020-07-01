#pragma once

#include "clam/ClamAnalysisParams.hh"
#include "clam/Clam.hh"

#include <map>

namespace clam {
class DomainRegistry {
public:
  typedef std::map<CrabDomain::Type, clam_abstract_domain> FactoryMap;

  template<typename AbsDom>
  static bool add(CrabDomain::Type dom_ty) {
    auto &map = getFactoryMap();
    auto dom = DomainRegistry::makeTopDomain<AbsDom>();
    return map.insert({dom_ty, dom}).second;
  }
  
  static bool count(CrabDomain::Type dom_ty) {
    auto &map = getFactoryMap();
    return map.find(dom_ty) != map.end();
  }
  
  static clam_abstract_domain at(CrabDomain::Type dom_ty) {
    auto &map = getFactoryMap();
    return map.at(dom_ty);
  }
  
private:
  
  static FactoryMap s_map;
  
  static FactoryMap& getFactoryMap() {
    return s_map;
  }

  template<typename AbsDom>
  static clam_abstract_domain makeTopDomain();  
};      

#define MAKE_TOP_DOMAIN(DOM)			                \
template<>						        \
clam_abstract_domain DomainRegistry::makeTopDomain<DOM>() {	\
  DOM dom_val;						        \
  clam_abstract_domain res(dom_val);				\
  return res;							\
}
  
#define REGISTER_DOMAIN(domain_enum_val, domain_decl)			\
  bool domain_decl ## _entry = DomainRegistry::add<domain_decl>(domain_enum_val); \
  MAKE_TOP_DOMAIN(domain_decl)
  
} //end namespace clam
