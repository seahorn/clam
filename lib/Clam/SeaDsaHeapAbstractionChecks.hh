#pragma once 
#include "clam/config.h"

#ifdef HAVE_SEA_DSA
#include "clam/HeapAbstraction.hh"

namespace llvm {
  class DataLayout;
  class Value;
}

namespace sea_dsa {
  class Cell;
  class Node;
}

namespace clam {
  
// Disambiguation means that the cell can be safely translated to a
// Crab array. Succeeds if returned value != UNTYPED_REGION
region_info canBeDisambiguated(const sea_dsa::Cell& c, const llvm::DataLayout& dl,
			       // flags to allow unsound disambiguation
			       bool disambiguate_unknown,
			       bool disambiguate_ptr_cast,
			       bool disambiguate_external);

} // end namespace clam
#endif 
