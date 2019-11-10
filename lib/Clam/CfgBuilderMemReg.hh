#pragma once

#include "clam/HeapAbstraction.hh"

/* 
   Helpers for memory regions.
   
   We don't add array statements for memory regions containing
   pointers. This means that if the load's lhs or store value
   operand is a pointer we only add the corresponding pointer
   statement (ptr_load/ptr_store) but not any extra array statement
   (array_load/array_store). 
   
   FIXME: If we would want to add array statements with elements of
   pointer type, we need to do some renaming. Otherwise, for
   instance, for the lhs of a load instruction, the same variable
   name would be used both for ptr_load and array_load with
   contradicting types.
*/

namespace clam {

typedef typename HeapAbstraction::region_t mem_region_t;  
typedef typename HeapAbstraction::region_vector_t mem_region_vector_t;
  
inline mem_region_t
get_region(HeapAbstraction &mem, llvm::Function &f, llvm::Value *v) {
  mem_region_t res = mem.getRegion(f, v);
  if (res.get_type() == INT_REGION || res.get_type() == BOOL_REGION) {
    return res;
  } else {
    return mem_region_t();
  }
}

// Return whether the region contains a singleton alias class
template<typename HeapAbstraction>
inline const llvm::Value* get_singleton_value(Region<HeapAbstraction> r,
					      bool enable_unique_scalars) {
  if (enable_unique_scalars) {
    if (r.isUnknown()) return nullptr;
    if (r.get_type() == INT_REGION || r.get_type() == BOOL_REGION) {
      if (const llvm::Value* v = r.getSingleton()) {
	return v;
      }
    }
  }
  return nullptr;
}
  
template<typename V>
inline mem_region_vector_t get_read_only_regions(HeapAbstraction &mem, V& v) {
  mem_region_vector_t res;
  auto regions = mem.getOnlyReadRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
	       [](mem_region_t r){
		 return r.get_type() == INT_REGION || r.get_type() == BOOL_REGION;
	       });
  return res;
}

template<typename V>
inline mem_region_vector_t get_modified_regions(HeapAbstraction &mem, V& v) {
  mem_region_vector_t res;
  auto regions = mem.getModifiedRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
	       [](mem_region_t r){
		 return r.get_type() == INT_REGION || r.get_type() == BOOL_REGION;
	       });
  return res;
}
  
template<typename V>
inline mem_region_vector_t get_new_regions(HeapAbstraction &mem, V& v) {
  mem_region_vector_t res;
  auto regions = mem.getNewRegions(v);
  std::copy_if(regions.begin(), regions.end(), std::back_inserter(res),
	       [](mem_region_t r){
		 return r.get_type() == INT_REGION || r.get_type() == BOOL_REGION;
	       });
  return res;
}

} // end namespace clam
