#pragma once

/**
 * Utilities for memory-based property checkers.
 **/

#include "llvm/ADT/PointerIntPair.h"

namespace llvm {
class Value;
} // end namespace llvm

namespace clam {
namespace memory_check_utils {
// The Pointer is the base (global, alloca or function parameter) or
// null if it cannot identified. The Integer is either 0 or 1,
// indicating if the base is dereferenceable.
using DerefPointer = llvm::PointerIntPair<llvm::Value *, 1>;
DerefPointer getBasePtr(llvm::Value *V);
} // end namespace memory_check_utils
} // end namespace clam
