#pragma once

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "clam/HeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "seadsa/Graph.hh"
#include "seadsa/ShadowMem.hh"

#include "CfgBuilderShadowMem.hh"
#include "CfgBuilderUtils.hh"
#include "SeaDsaHeapAbstractionDsaToRegion.hh"

/**
 *  Convenient utilities to extract memory regions from LLVM
 *  instructions.
 **/
namespace clam {

typedef typename HeapAbstraction::RegionVec RegionVec;

// "Switch" function that uses either ShadowMem (mem) or
// HeapAbstraction (sm) to return the cell pointer a LLVM pointer.
inline Region getRegion(HeapAbstraction &mem, const seadsa::ShadowMem *sm,
                        const llvm::DataLayout &dl, llvm::Instruction *user,
                        llvm::Value *ptr) {
  if (sm) {
    // Use ShadowMem (sm) to access to the cell pointed by the pointer.
    llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(user);
    if (llvm::isa<llvm::LoadInst>(user) || llvm::isa<llvm::StoreInst>(user)) {
      return getShadowRegionFromLoadOrStore(*sm, dl, *user);
    } else if (CI && (isZeroInitializer(*CI) || isIntInitializer(*CI))) {
      return getShadowRegionFromGvInitializer(*sm, dl, *user, *ptr);
    } else {
      // To see which instructions we are skipping ...
      CLAM_WARNING("getRegion using ShadowMem skipped " << *user);
    }
  } else {
    // Use the Heap analysis (mem) to access to the cell pointed by the pointer.
    llvm::Function *fun = user->getParent()->getParent();
    Region res = mem.getRegion(*fun, user, ptr);
    if (res.getRegionInfo().getType() == INT_REGION ||
        res.getRegionInfo().getType() == BOOL_REGION) {
      return res;
    }
  }
  return Region();
}

// Return whether the region contains a singleton alias class.
inline const llvm::Value *getSingletonValue(Region r,
                                            bool enable_unique_scalars) {
  if (enable_unique_scalars) {
    if (r.isUnknown())
      return nullptr;
    if (r.getRegionInfo().getType() == INT_REGION ||
        r.getRegionInfo().getType() == BOOL_REGION) {
      if (const llvm::Value *v = r.getSingleton()) {
        return v;
      }
    }
  }
  return nullptr;
}

/**
 * HeapAbstraction functions
 **/

// v is either a llvm::Function or llvm::CallInst.
template <typename V>
inline RegionVec getReadOnlyRegions(HeapAbstraction &mem, V &v) {
  RegionVec res;
  auto regions = mem.getOnlyReadRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
               [](Region r) {
                 return r.getRegionInfo().getType() == INT_REGION ||
                        r.getRegionInfo().getType() == BOOL_REGION;
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
                 return r.getRegionInfo().getType() == INT_REGION ||
                        r.getRegionInfo().getType() == BOOL_REGION;
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
                 return r.getRegionInfo().getType() == INT_REGION ||
                        r.getRegionInfo().getType() == BOOL_REGION;
               });
  return res;
}

} // end namespace clam
