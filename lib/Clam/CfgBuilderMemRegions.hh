#pragma once

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "clam/HeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "seadsa/Graph.hh"

#include "CfgBuilderUtils.hh"

#include <set>

/**
 *  Convenient utilities to extract memory regions from LLVM
 *  instructions.
 **/
namespace clam {

using RegionVec = typename HeapAbstraction::RegionVec;
using RegionSet = std::set<Region>;

static bool isTrackedRegion(Region rgn, const CrabBuilderParams &params) {
  if (params.precision_level == CrabBuilderPrecision::MEM) {
    return true;
  } else if (params.precision_level == CrabBuilderPrecision::SINGLETON_MEM) {
    return (rgn.getRegionInfo().containScalar() &&
            !rgn.getRegionInfo().isHeap());
  } else {
    return false;
  }
}

// Return the region associated to ptr
inline Region getRegion(HeapAbstraction &mem, RegionSet &Regions,
                        const CrabBuilderParams &params,
                        const llvm::Function &fun, const llvm::Value &ptr) {
  Region rgn = mem.getRegion(fun, ptr);
  if (isTrackedRegion(rgn, params)) {
    Regions.insert(rgn);
    return rgn;
  } else {
    return Region();
  }
}

inline Region getRegion(HeapAbstraction &mem, RegionSet &Regions,
                        const CrabBuilderParams &params,
                        const llvm::Instruction &I, const llvm::Value &ptr) {
  const llvm::Function &fun = *(I.getParent()->getParent());
  return getRegion(mem, Regions, params, fun, ptr);
}

// Return whether the region contains a singleton alias class.
inline const llvm::Value *getSingletonValue(Region r,
                                            bool enable_unique_scalars) {
  if (enable_unique_scalars) {
    if (r.isUnknown()) {
      return nullptr;
    }
    if (const llvm::Value *v = r.getSingleton()) {
      return v;
    }
  }
  return nullptr;
}

// v is either a llvm::Function or llvm::CallInst.
template <typename V>
inline RegionVec getInputRegions(HeapAbstraction &mem,
                                 const CrabBuilderParams &params, V &v) {
  auto regions = mem.getOnlyReadRegions(v);
  if (params.trackMemory()) {
    return regions;
  } else {
    RegionVec scalar_regions;
    std::copy_if(regions.begin(), regions.end(),
                 std::back_inserter(scalar_regions),
                 [&params](Region r) { return isTrackedRegion(r, params); });
    return scalar_regions;
  }
}

// v is either a llvm::Function or llvm::CallInst.
template <typename V>
inline RegionVec getInputOutputRegions(HeapAbstraction &mem,
                                       const CrabBuilderParams &params, V &v) {
  auto regions = mem.getModifiedRegions(v);
  if (params.trackMemory()) {
    return regions;
  } else {
    RegionVec scalar_regions;
    std::copy_if(regions.begin(), regions.end(),
                 std::back_inserter(scalar_regions),
                 [&params](Region r) { return isTrackedRegion(r, params); });
    return scalar_regions;
  }
}

// v is either a llvm::Function or llvm::CallInst.
template <typename V>
inline RegionVec getOutputRegions(HeapAbstraction &mem,
                                  const CrabBuilderParams &params, V &v) {
  auto regions = mem.getNewRegions(v);
  if (params.trackMemory()) {
    return regions;
  } else {
    RegionVec scalar_regions;
    std::copy_if(regions.begin(), regions.end(),
                 std::back_inserter(scalar_regions),
                 [&params](Region r) { return isTrackedRegion(r, params); });
    return scalar_regions;
  }
}

} // end namespace clam
