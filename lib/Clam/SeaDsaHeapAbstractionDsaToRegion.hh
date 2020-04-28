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

// Decide if a sea-dsa cell can be safely translated to a Crab
// array. Succeeds if returned value != UNTYPED_REGION
RegionInfo DsaToRegion(const seadsa::Cell &c,
		       const llvm::DataLayout &dl,
		       // field-sensitivity
		       bool split_dsa_nodes,
		       // abstract-domain dependent flag
		       bool disambiguate_for_array_smashing,		       
		       // flags to allow unsound disambiguation
		       bool disambiguate_unknown,
		       bool disambiguate_ptr_cast,
		       bool disambiguate_external);

} // end namespace clam
