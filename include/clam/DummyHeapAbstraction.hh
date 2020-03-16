#pragma once

#include "clam/config.h"
#include "clam/HeapAbstraction.hh"
#include "llvm/ADT/StringRef.h"

namespace clam {
  
/* Dummy heap analysis if no pointer analysis is available */  
struct DummyHeapAbstraction: public HeapAbstraction {

  using typename HeapAbstraction::RegionVec;
  using typename HeapAbstraction::RegionId;  
  
  DummyHeapAbstraction(): HeapAbstraction() { }

  HeapAbstraction::ClassId getClassId() const {
    return HeapAbstraction::ClassId::DUMMY;
  }

  bool isBasePtr(const llvm::Function &F, const llvm::Value *V) {
    return false;
  }
  
  Region getRegion(const llvm::Function&,
		   const llvm::Instruction*, const llvm::Value*) {
    return Region();
  }
  
  RegionVec getAccessedRegions(const llvm::Function&) {
    return RegionVec();
  }
  
  RegionVec getOnlyReadRegions(const llvm::Function&) {
    return RegionVec();
  }
  
  RegionVec getModifiedRegions(const llvm::Function&) {
    return RegionVec();
  }
  
  RegionVec getNewRegions(const llvm::Function&) {
    return RegionVec();
  }
  
  RegionVec getAccessedRegions(const llvm::CallInst&) {
    return RegionVec();
  }
  
  RegionVec getOnlyReadRegions(const llvm::CallInst&) {
    return RegionVec();
  }
  
  RegionVec getModifiedRegions(const llvm::CallInst&) {
    return RegionVec();
  }
  
  RegionVec getNewRegions(const llvm::CallInst&) {
    return RegionVec();
  }
  
  llvm::StringRef getName() const {
    return "DummyHeapAbstraction";
  }
}; 

} // end namespace

