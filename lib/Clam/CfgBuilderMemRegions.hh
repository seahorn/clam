#pragma once

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "clam/HeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "sea_dsa/ShadowMem.hh"
#include "sea_dsa/Graph.hh"

#include "SeaDsaHeapAbstractionDsaToRegion.hh"

/**
 *  Convenient utilities for memory regions.
 *
 *  We don't add array statements for memory regions containing
 *  pointers. This means that if the load's lhs or store value
 *  operand is a pointer we only add the corresponding pointer
 *  statement (ptr_load/ptr_store) but not any extra array statement
 *  (array_load/array_store).
 *
 *  FIXME: If we would want to add array statements with elements of
 *  pointer type, we need to do some renaming. Otherwise, for
 *  instance, for the lhs of a load instruction, the same variable
 *  name would be used both for ptr_load and array_load with
 *  contradicting types.
 **/
namespace clam {

typedef typename HeapAbstraction::RegionVec RegionVec;

inline Region get_region(HeapAbstraction &mem,
			 const sea_dsa::ShadowMem* sm,
			 const llvm::DataLayout &dl,
			 llvm::Instruction *user, llvm::Value *ptr) {
  if (sm) {
    // Use ShadowMem to access to the cell pointed by the pointer.
    if (llvm::isa<llvm::LoadInst>(user) || llvm::isa<llvm::StoreInst>(user)) {
      auto it = user->getIterator();
      --it;
      if (llvm::CallInst *ci = llvm::dyn_cast<llvm::CallInst>(&*it)) {
    	sea_dsa::ShadowMemInstOp op = sm->getShadowMemOp(*ci);
    	switch (op) {
    	case sea_dsa::ShadowMemInstOp::LOAD:
    	case sea_dsa::ShadowMemInstOp::STORE:
	  {
	    auto cellOpt= sm->getShadowMemCell(*ci);
	    if (cellOpt.hasValue()) {
	      sea_dsa::Cell c = cellOpt.getValue();
	      auto id_opt = sm->getCellId(c);
	      if (!id_opt.hasValue())
		break;	    
	      auto ri = DsaToRegion(c, dl, false, false, false);
	      if (ri.get_type() == UNTYPED_REGION)
		break;
	      return Region(id_opt.getValue(), ri, c.getNode()->getUniqueScalar());
	    }
	  }
    	  break;
    	default:; 
	  CLAM_ERROR("unreachable");
    	}       
      }
    } else {
      // To see which instructions we are skipping ...
      CLAM_WARNING("get_region using ShadowMem skipped " << *user);
    }
  } else {
    // Use the Heap analysis to access to the cell pointed by the pointer.
    llvm::Function *fun = user->getParent()->getParent();
    Region res = mem.getRegion(*fun, user, ptr);
    if (res.getRegionInfo().get_type() == INT_REGION ||
	res.getRegionInfo().get_type() == BOOL_REGION) {
      return res;
    }
  }
  return Region();    
}

// Return whether the region contains a singleton alias class
inline const llvm::Value *
get_singleton_value(Region r, bool enable_unique_scalars) {
  if (enable_unique_scalars) {
    if (r.isUnknown())
      return nullptr;
    if (r.getRegionInfo().get_type() == INT_REGION ||
	r.getRegionInfo().get_type() == BOOL_REGION) {
      if (const llvm::Value *v = r.getSingleton()) {
        return v;
      }
    }
  }
  return nullptr;
}
// V is either a llvm::Function or llvm::CallInst
template <typename V>
inline RegionVec get_read_only_regions(HeapAbstraction &mem, V &v) {
  RegionVec res;
  auto regions = mem.getOnlyReadRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
               [](Region r) {
                 return r.getRegionInfo().get_type() == INT_REGION ||
                        r.getRegionInfo().get_type() == BOOL_REGION;
               });
  return res;
}

// V is either a llvm::Function or llvm::CallInst
template <typename V>
inline RegionVec get_modified_regions(HeapAbstraction &mem, V &v) {
  RegionVec res;
  auto regions = mem.getModifiedRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
               [](Region r) {
                 return r.getRegionInfo().get_type() == INT_REGION ||
                        r.getRegionInfo().get_type() == BOOL_REGION;
               });
  return res;
}

// V is either a llvm::Function or llvm::CallInst
template <typename V>
inline RegionVec get_new_regions(HeapAbstraction &mem, V &v) {
  RegionVec res;
  auto regions = mem.getNewRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
               [](Region r) {
                 return r.getRegionInfo().get_type() == INT_REGION ||
                        r.getRegionInfo().get_type() == BOOL_REGION;
               });
  return res;
}

} // end namespace clam
