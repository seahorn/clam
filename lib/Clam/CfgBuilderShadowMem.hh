#pragma once

#include <utility>
#include "llvm/ADT/Optional.h"
#include "clam/HeapAbstraction.hh"

namespace llvm {
class Value;
class StoreInst;
class LoadInst;
class PHINode;
class DataLayout;
class Instruction;
}

namespace sea_dsa {
class ShadowMem;
class Cell;
}

namespace clam {

// Return the Value *defined* and *used* by the shadow mem store
// associated to I. Both pair elements should not be null but either
// can be.
std::pair<llvm::Value*, llvm::Value*>
getShadowMemDefAndUse(llvm::StoreInst &I, const sea_dsa::ShadowMem& sm);

// Return the Value *used* by the the shadow mem load associated to I.
llvm::Value& getShadowMemUse(llvm::LoadInst &I, const sea_dsa::ShadowMem &sm);

// Return the unique scalar (if any) from the shadow mem store
// associated to I. It can return null.
const llvm::Value* getShadowMemUniqueScalar(llvm::StoreInst &I,
					    const sea_dsa::ShadowMem &sm);

// Return the unique scalar (if any) from the shadow mem load
// associated to I. It can return null.
const llvm::Value* getShadowMemUniqueScalar(llvm::LoadInst &I,
					    const sea_dsa::ShadowMem &sm);

// Return the cell associated with the shadow mem PHI incoming value.
llvm::Optional<sea_dsa::Cell> getShadowMemCell(const llvm::PHINode &phi,
 					       const llvm::Value &incVal,
 					       const sea_dsa::ShadowMem &sm);

// Get region from a shadow mem instruction.
Region getShadowRegion(llvm::CallInst &shadowCI,
		       const llvm::DataLayout &dl,
		       const sea_dsa::ShadowMem &sm);

// Get region from a cell
Region getShadowRegion(const sea_dsa::Cell &c,
		       const llvm::DataLayout &dl,		       
		       const sea_dsa::ShadowMem &sm);
  
// Find the shadow instruction associated to the global initializer.
// It can return null.
llvm::CallInst* getShadowCIFromGvInitializer(const sea_dsa::ShadowMem &sm,
					     llvm::Instruction &gvInitInst,
					     llvm::Value &v);

// Find the shadow instruction associated to the global initializer
// and extract region from the corresponding shadow mem global.init or
// arg.init.
Region getShadowRegionFromGvInitializer(const sea_dsa::ShadowMem &sm,
					const llvm::DataLayout &dl,
					llvm::Instruction &gvInitInst,
					llvm::Value &v);

// Find the shadow instruction associated to store or load instruction
// and extract the region from there.
Region getShadowRegionFromLoadOrStore(const sea_dsa::ShadowMem &sm,
				      const llvm::DataLayout &dl,
				      llvm::Instruction &loadOrStore);
} // namespace clam
