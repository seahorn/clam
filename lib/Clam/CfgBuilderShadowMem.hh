#pragma once

#include <utility>
#include "llvm/ADT/Optional.h"

namespace llvm {
class Value;
class StoreInst;
class LoadInst;
class PHINode;
}

namespace sea_dsa {
class ShadowMem;
class Cell;
}

namespace clam {

// Return the Value *defined* and *used* by the shadow mem store
// associated to I
std::pair<llvm::Value*, llvm::Value*>
getShadowMemDefAndUse(llvm::StoreInst &I, const sea_dsa::ShadowMem& sm);

// Return the Value *used* by the the shadow mem load associated to I.
llvm::Value& getShadowMemUse(llvm::LoadInst &I, const sea_dsa::ShadowMem& sm);

// Return the unique scalar (if any) from the shadow mem store
// associated to I.
const llvm::Value* getShadowMemUniqueScalar(llvm::StoreInst &I,
					    const sea_dsa::ShadowMem &sm);

// Return the unique scalar (if any) from the shadow mem load
// associated to I.
const llvm::Value* getShadowMemUniqueScalar(llvm::LoadInst &I,
					    const sea_dsa::ShadowMem &sm);

// Return true if the PHI node is a shadow memory PHI node
bool isShadowMemPHINode(llvm::PHINode &PN);

// Return the cell associated with the shadow mem PHI node
llvm::Optional<sea_dsa::Cell> getShadowMemCell(const llvm::PHINode &ShadowMemPHI,
					       const sea_dsa::ShadowMem &sm);
}
