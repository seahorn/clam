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

// Decide if a sea-dsa **cell** can be safely translated to a Crab
// array. Succeeds if returned value != UNTYPED_REGION
// 
// Note that we translate cells to Crab arrays. This implies extra
// checks to make sure that all cells originated from the same node
// are disjoint. By construction all sea-dsa nodes are
// dijoint. However, not all sea-dsa cells are disjoint.
RegionInfo DsaToRegion(const seadsa::Cell &c, const llvm::DataLayout &dl,
		       // flags to allow unsound disambiguation
                       bool disambiguate_unknown, bool disambiguate_ptr_cast,
		       bool disambiguate_external);

} // end namespace clam
