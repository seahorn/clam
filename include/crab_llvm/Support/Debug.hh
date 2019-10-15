# pragma once

#include <crab/common/debug.hpp> // to access to CrabWarningFlag
#include "llvm/Support/raw_ostream.h"

namespace crabllvm {

#define CRABLLVM_ERROR(...)					\
do {								\
  llvm::errs() << "CRABLLVM ERROR: ";				\
  llvm::errs() << __VA_ARGS__ ;					\
  llvm::errs() << " at " << __FILE__ << ":" << __LINE__ << "\n";\
  std::exit (EXIT_FAILURE);					\
  } while (0)


#define CRABLLVM_WARNING(...)			\
do {						\
   if (::crab::CrabWarningFlag) {		\
     llvm::errs() << "CRABLLVM WARNING: ";	\
     llvm::errs() << __VA_ARGS__ ;		\
     llvm::errs() << "\n";			\
   }						\
 } while(0)

} // end namespace crabllvm

