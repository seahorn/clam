#pragma once
#include "clam/HeapAbstraction.hh"

namespace llvm {
class DataLayout;
class Value;
} // namespace llvm

namespace seadsa {
class Cell;
class Node;
} // namespace seadsa

namespace clam {

// Translate a sea-dsa **cell** to a Clam memory region.
RegionInfo SeaDsaToRegion(const seadsa::Cell &c, const llvm::DataLayout &dl,
                          // flags to allow unsound disambiguation
                          bool disambiguate_unknown, bool disambiguate_ptr_cast,
                          bool disambiguate_external);

} // end namespace clam
