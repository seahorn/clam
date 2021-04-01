#pragma once

#include "clam/CrabDomain.hh"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"

#include <string>

namespace clam {
struct CrabDomainParser : public llvm::cl::parser<CrabDomain::Type> {
  CrabDomainParser(llvm::cl::Option &O);

  void addLiteralOption(llvm::StringRef Name, const unsigned &V,
                        llvm::StringRef HelpStr);
  // parse - Return true on error.
  bool parse(llvm::cl::Option &O, llvm::StringRef ArgName,
             llvm::StringRef ArgValue, CrabDomain::Type &Val);
};
} // end namespace clam
