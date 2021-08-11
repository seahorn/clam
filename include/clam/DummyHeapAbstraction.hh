#pragma once

#include "clam/HeapAbstraction.hh"
#include "clam/config.h"
#include "llvm/ADT/StringRef.h"

namespace clam {

/* Dummy heap analysis if no pointer analysis is available */
struct DummyHeapAbstraction : public HeapAbstraction {

  using typename HeapAbstraction::RegionId;
  using typename HeapAbstraction::RegionVec;

  DummyHeapAbstraction() : HeapAbstraction() {}

  HeapAbstraction::ClassId getClassId() const {
    return HeapAbstraction::ClassId::DUMMY;
  }

  Region getRegion(const llvm::Function &, const llvm::Value &) {
    return Region();
  }

  RegionVec getOnlyReadRegions(const llvm::Function &) const { return RegionVec(); }

  RegionVec getModifiedRegions(const llvm::Function &) const { return RegionVec(); }

  RegionVec getNewRegions(const llvm::Function &) const { return RegionVec(); }

  RegionVec getOnlyReadRegions(const llvm::CallInst &) const { return RegionVec(); }

  RegionVec getModifiedRegions(const llvm::CallInst &) const { return RegionVec(); }

  RegionVec getNewRegions(const llvm::CallInst &) const { return RegionVec(); }

  llvm::StringRef getName() const { return "DummyHeapAbstraction"; }
};

} // namespace clam
