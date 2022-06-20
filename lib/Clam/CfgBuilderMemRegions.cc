#include "CfgBuilderMemRegions.hh"
#include "CfgBuilderUtils.hh"

#include "clam/Support/Debug.hh"
#include "seadsa/Graph.hh"

using namespace llvm;

/**
 *  Convenient utilities to extract memory regions from LLVM
 *  instructions.
 **/
namespace clam {

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
Region getRegion(HeapAbstraction &mem, RegionSet &Regions,
		 const CrabBuilderParams &params,
		 const Function &fun, const Value &ptr) {
  Region rgn = mem.getRegion(fun, ptr);
  if (isTrackedRegion(rgn, params)) {
    Regions.insert(rgn);
    return rgn;
  } else {
    return Region();
  }
}

Region getRegion(HeapAbstraction &mem, RegionSet &Regions,
                        const CrabBuilderParams &params,
                        const Instruction &I, const Value &ptr) {
  const Function &fun = *(I.getParent()->getParent());
  return getRegion(mem, Regions, params, fun, ptr);
}

// Return whether the region contains a singleton alias class.
const Value *getSingletonValue(Region r,
				     bool enable_unique_scalars) {
  if (enable_unique_scalars) {
    if (r.isUnknown()) {
      return nullptr;
    }
    if (const Value *v = r.getSingleton()) {
      return v;
    }
  }
  return nullptr;
}

// v is either a Function or CallInst.
template <typename V>
RegionVec getInputRegions(HeapAbstraction &mem,
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

// v is either a Function or CallInst.
template <typename V>
RegionVec getInputOutputRegions(HeapAbstraction &mem,
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

// v is either a Function or CallInst.
template <typename V>
RegionVec getOutputRegions(HeapAbstraction &mem,
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

// instantiations
template RegionVec getInputRegions<CallInst>(HeapAbstraction &mem,
					     const CrabBuilderParams &params,
					     CallInst &v);
template RegionVec getInputRegions<Function>(HeapAbstraction &mem,
					     const CrabBuilderParams &params,
					     Function &v);


template RegionVec getInputOutputRegions<CallInst>(HeapAbstraction &mem,
						   const CrabBuilderParams &params,
						   CallInst &v);
template RegionVec getInputOutputRegions<Function>(HeapAbstraction &mem,
						   const CrabBuilderParams &params,
						   Function &v);

  
template  RegionVec getOutputRegions<CallInst>(HeapAbstraction &mem,
					       const CrabBuilderParams &params,
					       CallInst &v);
template  RegionVec getOutputRegions<Function>(HeapAbstraction &mem,
					       const CrabBuilderParams &params,
					       Function &v);
  
} // end namespace clam
