#pragma once

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "clam/HeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "seadsa/Graph.hh"

#include "CfgBuilderUtils.hh"

/**
 *  Convenient utilities to extract memory regions from LLVM
 *  instructions.
 **/
namespace clam {

typedef typename HeapAbstraction::RegionVec RegionVec;

// Return the region associated to ptr
inline Region getRegion(HeapAbstraction &mem, const llvm::DataLayout &dl /*unused*/,
			llvm::Instruction *user, llvm::Value *ptr) {
  // Use the Heap analysis (mem) to access to the cell pointed by the pointer.
  llvm::Function *fun = user->getParent()->getParent();
  Region res = mem.getRegion(*fun, user, ptr);
  if (res.getRegionInfo().getType() != UNTYPED_REGION) {
    return res;
  }
  return Region();
}

// Return whether the region contains a singleton alias class.
inline const llvm::Value *getSingletonValue(Region r, bool enable_unique_scalars) {
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
inline RegionVec getReadOnlyRegions(HeapAbstraction &mem, V &v) {
  RegionVec res;
  auto regions = mem.getOnlyReadRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
               [](Region r) {
                 return r.getRegionInfo().getType() != UNTYPED_REGION;
               });
  return res;
}

// v is either a llvm::Function or llvm::CallInst.
template <typename V>
inline RegionVec getModifiedRegions(HeapAbstraction &mem, V &v) {
  RegionVec res;
  auto regions = mem.getModifiedRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
               [](Region r) {
                 return r.getRegionInfo().getType() != UNTYPED_REGION;
               });
  return res;
}

// v is either a llvm::Function or llvm::CallInst.
template <typename V>
inline RegionVec getNewRegions(HeapAbstraction &mem, V &v) {
  RegionVec res;
  auto regions = mem.getNewRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
               [](Region r) {
                 return r.getRegionInfo().getType() != UNTYPED_REGION;
               });
  return res;
}

} // end namespace clam
