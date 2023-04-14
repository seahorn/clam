#include "clam/CrabDomainParser.hh"

namespace clam {
CrabDomainParser::CrabDomainParser(llvm::cl::Option &O)
    : llvm::cl::parser<CrabDomain::Type>(O) {}

void CrabDomainParser::addLiteralOption(llvm::StringRef Name, const unsigned &V,
                                        llvm::StringRef HelpStr) {
  CrabDomain::Type TV;
  bool found = false;
  for (auto t : CrabDomain::List) {
    if (t.value() == V) {
      TV = t;
      found = true;
      break;
    }
  }
  if (found) {
    OptionInfo X(Name, TV, HelpStr);
    Values.push_back(X);
    AddLiteralOption(Owner, Name);
  } else {
    // TODO: ERROR
  }
}

// parse - Return true on error.
bool CrabDomainParser::parse(llvm::cl::Option &O, llvm::StringRef ArgName,
                             llvm::StringRef ArgValue,
                             CrabDomain::Type &Val) {
  bool found = false;
  for (auto t : CrabDomain::List) {
    if (t.name() == ArgValue) {
      Val = t;
      found = true;
      break;
    }
  }
  return !found;
}
} // end namespace clam
