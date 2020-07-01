#pragma once

#include "llvm/Support/raw_ostream.h"
#include <crab/support/debug.hpp> // to access to CrabWarningFlag

namespace clam {

#define CLAM_ERROR(...)                                                        \
  do {                                                                         \
    llvm::errs() << "CLAM ERROR: ";                                            \
    llvm::errs() << __VA_ARGS__;                                               \
    llvm::errs() << " at " << __FILE__ << ":" << __LINE__ << "\n";             \
    std::exit(EXIT_FAILURE);                                                   \
  } while (0)

#define CLAM_WARNING(...)                                                      \
  do {                                                                         \
    if (::crab::CrabWarningFlag) {                                             \
      llvm::errs() << "CLAM WARNING: ";                                        \
      llvm::errs() << __VA_ARGS__;                                             \
      llvm::errs() << "\n";                                                    \
    }                                                                          \
  } while (0)

} // end namespace clam
