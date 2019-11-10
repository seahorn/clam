#pragma once

#include "clam/config.h"
#include "clam/HeapAbstraction.hh"
#include "llvm/ADT/StringRef.h"

namespace clam {
  
/* Dummy heap analysis if no pointer analysis is available */  
struct DummyHeapAbstraction: public HeapAbstraction {

  using typename HeapAbstraction::region_t;
  using typename HeapAbstraction::region_vector_t;
  using typename HeapAbstraction::region_id_t;  
  
  DummyHeapAbstraction(): HeapAbstraction() { }
  
  const llvm::Value* getSingleton(region_id_t) const {
    return nullptr;
  }
  
  region_t getRegion(const llvm::Function&, const llvm::Value*) {
    return region_t();
  }
  
  region_vector_t getAccessedRegions(const llvm::Function&) {
    return region_vector_t();
  }
  
  region_vector_t getOnlyReadRegions(const llvm::Function&) {
    return region_vector_t();
  }
  
  region_vector_t getModifiedRegions(const llvm::Function&) {
    return region_vector_t();
  }
  
  region_vector_t getNewRegions(const llvm::Function&) {
    return region_vector_t();
  }
  
  region_vector_t getAccessedRegions(const llvm::CallInst&) {
    return region_vector_t();
  }
  
  region_vector_t getOnlyReadRegions(const llvm::CallInst&) {
    return region_vector_t();
  }
  
  region_vector_t getModifiedRegions(const llvm::CallInst&) {
    return region_vector_t();
  }
  
  region_vector_t getNewRegions(const llvm::CallInst&) {
    return region_vector_t();
  }
  
  llvm::StringRef getName() const {
    return "DummyHeapAbstraction";
  }
}; 

} // end namespace

