#pragma once

#include "llvm/ADT/StringRef.h"

#include "crab_llvm/config.h"
#include "crab_llvm/HeapAbstraction.hh"

namespace crab_llvm {

/* Dummy heap analysis if no pointer analysis is available */  
struct DummyHeapAbstraction: public HeapAbstraction {

  using typename HeapAbstraction::region_t;
  using typename HeapAbstraction::region_set_t;
  
  DummyHeapAbstraction(): HeapAbstraction() { }
  
  const llvm::Value* getSingleton(int) const {
    return nullptr;
  }
  
  region_t getRegion(const llvm::Function&, llvm::Value*) {
    return region_t();
  }
  
  region_set_t getAccessedRegions(const llvm::Function&) {
    return region_set_t();
  }
  
  region_set_t getOnlyReadRegions(const llvm::Function&) {
    return region_set_t();
  }
  
  region_set_t getModifiedRegions(const llvm::Function&) {
    return region_set_t();
  }
  
  region_set_t getNewRegions(const llvm::Function&) {
    return region_set_t();
  }
  
  region_set_t getAccessedRegions(llvm::CallInst&) {
    return region_set_t();
  }
  
  region_set_t getOnlyReadRegions(llvm::CallInst&) {
    return region_set_t();
  }
  
  region_set_t getModifiedRegions(llvm::CallInst&) {
    return region_set_t();
  }
  
  region_set_t getNewRegions(llvm::CallInst&) {
    return region_set_t();
  }
  
  llvm::StringRef getName() const {
    return "DummyHeapAbstraction";
  }
}; 

} // end namespace

