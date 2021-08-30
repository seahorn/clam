#pragma once

#include "clam/HeapAbstraction.hh"
#include "clam/CfgBuilderParams.hh"
#include <set>

namespace llvm {
class Instruction;
class Value;
} // end namespace llvm

/**
 *  Convenient utilities to extract memory regions from LLVM
 *  instructions.
 **/
namespace clam {

using RegionVec = typename HeapAbstraction::RegionVec;
using RegionSet = std::set<Region>;

// Return the region associated to ptr
Region getRegion(HeapAbstraction &mem, RegionSet &Regions,
		 const CrabBuilderParams &params,
		 const llvm::Function &fun, const llvm::Value &ptr);

Region getRegion(HeapAbstraction &mem, RegionSet &Regions,
		 const CrabBuilderParams &params,
		 const llvm::Instruction &I, const llvm::Value &ptr);

// Return whether the region contains a singleton alias class.
const llvm::Value *getSingletonValue(Region r, bool enable_unique_scalars);

// v is either a llvm::Function or llvm::CallInst.
template<typename V>
RegionVec getInputRegions(HeapAbstraction &mem,
			  const CrabBuilderParams &params, V &v);

// v is either a llvm::Function or llvm::CallInst.
template<typename V>
RegionVec getInputOutputRegions(HeapAbstraction &mem,
				const CrabBuilderParams &params, V &v);

// v is either a llvm::Function or llvm::CallInst.
template<typename V>
RegionVec getOutputRegions(HeapAbstraction &mem,
			   const CrabBuilderParams &params, V &v);

} // end namespace clam
