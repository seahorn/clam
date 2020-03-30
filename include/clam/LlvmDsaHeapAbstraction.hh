#pragma once

#include "clam/config.h"

#ifdef HAVE_DSA

#include "clam/HeapAbstraction.hh"
#include "dsa/DSNode.h" // not enough forward declaration
#include "llvm/ADT/StringRef.h"

#include <unordered_map>

// forward declarations
namespace llvm {
  class DSGraph;
  class DSCallSite;
  class DataStructures;
}

namespace clam {

  /* 
   * Wrapper for LLVM-dsa (https://github.com/seahorn/llvm-dsa)
   */
  class LlvmDsaHeapAbstraction: public HeapAbstraction {

   public:

    using typename HeapAbstraction::RegionVec;
    using typename HeapAbstraction::RegionId;
    
   private:
    
    const llvm::Module &m_M;
    llvm::DataStructures *m_dsa;
    
    /// map from DSNode to id
    llvm::DenseMap<const llvm::DSNode*, RegionId> m_node_ids;
    std::unordered_map<RegionId, const llvm::DSNode*> m_rev_node_ids;
    RegionId m_max_id;

    bool m_disambiguate_unknown;
    bool m_disambiguate_ptr_cast;
    bool m_disambiguate_external;
    
    llvm::DenseMap<const llvm::Function*, RegionVec> m_func_accessed;
    llvm::DenseMap<const llvm::Function*, RegionVec> m_func_mods;
    llvm::DenseMap<const llvm::Function*, RegionVec> m_func_news;

    llvm::DenseMap<const llvm::CallInst*, RegionVec> m_callsite_accessed;
    llvm::DenseMap<const llvm::CallInst*, RegionVec> m_callsite_mods;
    llvm::DenseMap<const llvm::CallInst*, RegionVec> m_callsite_news;

    RegionId getId(const llvm::DSNode* n, unsigned offset);
            
    // compute and cache the set of read, mod and new nodes of a whole
    // function such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodes(const llvm::Function &f);

    // Compute and cache the set of read, mod and new nodes of a
    // callsite such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodesFromCallSite(const llvm::CallInst &I);

    const llvm::Value* getSingleton(RegionId region) const;
    
   public:
    
    LlvmDsaHeapAbstraction(const llvm::Module &M, llvm::DataStructures *dsa,
			   bool disambiguate_unknown  = false,
			   bool disambiguate_ptr_cast = false,
			   bool disambiguate_external = false);

    HeapAbstraction::ClassId getClassId() const {
      return HeapAbstraction::ClassId::LLVM_DSA;
    }

    virtual bool isBasePtr(const llvm::Function &F, const llvm::Value *V) override {
      // To be implemented
      return false;
    }
    
    virtual Region getRegion(const llvm::Function &F,
			     const llvm::Instruction *I, const llvm::Value *V) override;
    
    virtual RegionVec getAccessedRegions(const llvm::Function &F) override;
    
    virtual RegionVec getOnlyReadRegions(const llvm::Function &F) override;
    
    virtual RegionVec getModifiedRegions(const llvm::Function &F) override;
    
    virtual RegionVec getNewRegions(const llvm::Function &F) override;
    
    virtual RegionVec getAccessedRegions(const llvm::CallInst &I) override;
    
    virtual RegionVec getOnlyReadRegions(const llvm::CallInst &I) override;
    
    virtual RegionVec getModifiedRegions(const llvm::CallInst &I) override;
    
    virtual RegionVec getNewRegions(const llvm::CallInst &I) override;
    
    virtual llvm::StringRef getName() const override {
      return "LlvmDsaHeapAbstraction";
    }
  }; 
} // end namespace clam

#endif /* HAVE_DSA */
