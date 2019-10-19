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

    using typename HeapAbstraction::region_t;
    using typename HeapAbstraction::region_vector_t;
    using typename HeapAbstraction::region_id_t;
    
   private:
    
    const llvm::Module &m_M;
    llvm::DataStructures *m_dsa;
    
    /// map from DSNode to id
    llvm::DenseMap<const llvm::DSNode*, region_id_t> m_node_ids;
    std::unordered_map<region_id_t, const llvm::DSNode*> m_rev_node_ids;
    region_id_t m_max_id;

    bool m_disambiguate_unknown;
    bool m_disambiguate_ptr_cast;
    bool m_disambiguate_external;
    
    llvm::DenseMap<const llvm::Function*, region_vector_t> m_func_accessed;
    llvm::DenseMap<const llvm::Function*, region_vector_t> m_func_mods;
    llvm::DenseMap<const llvm::Function*, region_vector_t> m_func_news;

    llvm::DenseMap<const llvm::CallInst*, region_vector_t> m_callsite_accessed;
    llvm::DenseMap<const llvm::CallInst*, region_vector_t> m_callsite_mods;
    llvm::DenseMap<const llvm::CallInst*, region_vector_t> m_callsite_news;

    region_id_t getId(const llvm::DSNode* n, unsigned offset);
            
    // compute and cache the set of read, mod and new nodes of a whole
    // function such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodes(const llvm::Function &f);

    // Compute and cache the set of read, mod and new nodes of a
    // callsite such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodesFromCallSite(const llvm::CallInst &I);

   public:
    
    LlvmDsaHeapAbstraction(const llvm::Module &M, llvm::DataStructures *dsa,
			   bool disambiguate_unknown  = false,
			   bool disambiguate_ptr_cast = false,
			   bool disambiguate_external = false);
    
    virtual region_t getRegion(const llvm::Function &F, const llvm::Value *V) override;
    
    virtual const llvm::Value* getSingleton(region_id_t region) const override;
    
    virtual region_vector_t getAccessedRegions(const llvm::Function &F) override;
    
    virtual region_vector_t getOnlyReadRegions(const llvm::Function &F) override;
    
    virtual region_vector_t getModifiedRegions(const llvm::Function &F) override;
    
    virtual region_vector_t getNewRegions(const llvm::Function &F) override;
    
    virtual region_vector_t getAccessedRegions(const llvm::CallInst &I) override;
    
    virtual region_vector_t getOnlyReadRegions(const llvm::CallInst &I) override;
    
    virtual region_vector_t getModifiedRegions(const llvm::CallInst &I) override;
    
    virtual region_vector_t getNewRegions(const llvm::CallInst &I) override;
    
    virtual llvm::StringRef getName() const override {
      return "LlvmDsaHeapAbstraction";
    }
  }; 
} // end namespace clam

#endif /* HAVE_DSA */
