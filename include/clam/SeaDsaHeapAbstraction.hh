#pragma once

#include "clam/HeapAbstraction.hh"
#include "clam/CfgBuilderParams.hh"
#include "llvm/ADT/StringRef.h"

// forward declarations
namespace seadsa {
class GlobalAnalysis;
class Node;
class Cell;
class AllocWrapInfo;
class DsaLibFuncInfo;
} // namespace seadsa

namespace llvm {
class DataLayout;
class CallGraph;
class TargetLibraryInfo;
class TargetLibraryInfoWrapperPass;
} // namespace llvm

namespace clam {
  
class SeaDsaHeapAbstractionImpl;

struct SeaDsaHeapAbstractionParams {
  CrabBuilderPrecision precision_level;
  bool is_context_sensitive;
  bool disambiguate_unknown;
  bool disambiguate_ptr_cast;
  bool disambiguate_external;
  
  SeaDsaHeapAbstractionParams():
    precision_level(CrabBuilderPrecision::MEM),
    is_context_sensitive(true),
    disambiguate_unknown(false),
    disambiguate_ptr_cast(false),
    disambiguate_external(false) {}
};
  
/*
 * Wrapper for sea-dsa (https://github.com/seahorn/sea-dsa)
 */
class SeaDsaHeapAbstraction : public HeapAbstraction {

public:
  using typename HeapAbstraction::RegionId;
  using typename HeapAbstraction::RegionVec;

  // This constructor creates and owns a sea-dsa GlobalAnalysis instance and
  // run it on M.
  SeaDsaHeapAbstraction(const llvm::Module &M, llvm::CallGraph &cg,
                        const llvm::TargetLibraryInfoWrapperPass &tli,
                        const seadsa::AllocWrapInfo &alloc_wrap_info,
                        const seadsa::DsaLibFuncInfo &spec_graph_info,
			SeaDsaHeapAbstractionParams params);

  // This constructor takes an existing sea-dsa Global Analysis instance.
  // It doesn't own it.
  SeaDsaHeapAbstraction(const llvm::Module &M, seadsa::GlobalAnalysis &dsa,
			SeaDsaHeapAbstractionParams params);

  ~SeaDsaHeapAbstraction();

  HeapAbstraction::ClassId getClassId() const override {
    return HeapAbstraction::ClassId::SEA_DSA;
  }

  seadsa::GlobalAnalysis *getSeaDsa();
  
  const seadsa::GlobalAnalysis *getSeaDsa() const;
  
  virtual Region getRegion(const llvm::Function &F, const llvm::Value &V) override;

  virtual Region getRegion(const llvm::Function &F, const llvm::Value &V,
			   unsigned offset, const llvm::Type &AccessedType) override;

  virtual RegionVec getOnlyReadRegions(const llvm::Function &F) const override;

  virtual RegionVec getModifiedRegions(const llvm::Function &F) const override;

  virtual RegionVec getNewRegions(const llvm::Function &F) const override;

  virtual RegionVec getOnlyReadRegions(const llvm::CallInst &I) const override;

  virtual RegionVec getModifiedRegions(const llvm::CallInst &I) const override;

  virtual RegionVec getNewRegions(const llvm::CallInst &I) const override;

  virtual std::vector<RegionVec> getEquivClassRegions(const llvm::Function &F) const override;
        
  virtual llvm::StringRef getName() const override {
    return "SeaDsaHeapAbstraction";
  }
 
private:
  std::unique_ptr<SeaDsaHeapAbstractionImpl> m_impl;
};

} // end namespace clam
