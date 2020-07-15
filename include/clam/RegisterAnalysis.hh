#pragma once

#include "clam/Clam.hh"
#include "clam/ClamAnalysisParams.hh"
#include "crab/support/stats.hpp"

#include <map>

namespace clam {

class DomainRegistry {
public:
  typedef std::map<CrabDomain::Type, clam_abstract_domain> FactoryMap;

  template <typename AbsDom> static bool add(CrabDomain::Type dom_ty) {
    auto &map = getFactoryMap();
    auto dom = DomainRegistry::makeTopDomain<AbsDom>();
    bool res = map.insert({dom_ty, dom}).second;
    crab::CrabStats::reset();
    return res;
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
  static FactoryMap &getFactoryMap() {
    static FactoryMap map;
    return map;
  }

  /* The domains instantiations happen in lib/Clam/domains */
  template <typename AbsDom> static clam_abstract_domain makeTopDomain();
};

#define REGISTER_DOMAIN(domain_enum_val, domain_decl)                          \
  bool domain_decl##_entry = DomainRegistry::add<domain_decl>(domain_enum_val);

} // end namespace clam
