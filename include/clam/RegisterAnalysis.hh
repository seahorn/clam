#pragma once

#include <clam/Clam.hh>
#include <map>

namespace clam {
class DomainRegistry {
public:
  using FactoryMap = std::map<CrabDomain::Type, clam_abstract_domain>;
  static FactoryMap &getFactoryMap();

  static void registerAllDomains();
  static bool count(CrabDomain::Type dom_ty);
  static clam_abstract_domain at(CrabDomain::Type dom_ty);
};

} // end namespace clam
