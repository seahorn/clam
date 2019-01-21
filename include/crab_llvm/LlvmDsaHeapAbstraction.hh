#pragma once

#include "crab_llvm/config.h"

#ifdef HAVE_DSA

#include "llvm/ADT/StringRef.h"
#include "crab_llvm/HeapAbstraction.hh"
#include <boost/unordered_map.hpp>

#include "dsa/DSNode.h" // not enough forward declaration

// forward declarations
namespace llvm {
  class DSGraph;
  class DSCallSite;
  class DataStructures;
}

namespace crab_llvm {

  /* 
   * Wrapper for LLVM-dsa (https://github.com/seahorn/llvm-dsa)
   */
  class LlvmDsaHeapAbstraction: public HeapAbstraction {

   public:

     using typename HeapAbstraction::region_t;
     using typename HeapAbstraction::region_set_t;

   private:

    llvm::Module &m_M;
    llvm::DataStructures *m_dsa;
    
    /// map from DSNode to id
    llvm::DenseMap<const llvm::DSNode*, unsigned> m_node_ids;
    boost::unordered_map<unsigned, const llvm::DSNode*> m_rev_node_ids;
    unsigned m_max_id;

    bool m_disambiguate_unknown;
    bool m_disambiguate_ptr_cast;
    bool m_disambiguate_external;
    
    llvm::DenseMap<const llvm::Function*, region_set_t> m_func_accessed;
    llvm::DenseMap<const llvm::Function*, region_set_t> m_func_mods;
    llvm::DenseMap<const llvm::Function*, region_set_t> m_func_news;

    llvm::DenseMap<const llvm::CallInst*, region_set_t> m_callsite_accessed;
    llvm::DenseMap<const llvm::CallInst*, region_set_t> m_callsite_mods;
    llvm::DenseMap<const llvm::CallInst*, region_set_t> m_callsite_news;

    int getId(const llvm::DSNode* n, unsigned offset);
            
    // compute and cache the set of read, mod and new nodes of a whole
    // function such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodes(const llvm::Function &f);

    // Compute and cache the set of read, mod and new nodes of a
    // callsite such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodesFromCallSite(llvm::CallInst &I);

   public:
    
    LlvmDsaHeapAbstraction(llvm::Module &M, llvm::DataStructures *dsa,
			   bool disambiguate_unknown  = false,
			   bool disambiguate_ptr_cast = false,
			   bool disambiguate_external = false);
    
    virtual region_t getRegion(const llvm::Function &F, llvm::Value *V) override;
    
    virtual const llvm::Value* getSingleton(int region) const override;
    
    virtual region_set_t getAccessedRegions(const llvm::Function &F) override;
    
    virtual region_set_t getOnlyReadRegions(const llvm::Function &F) override;
    
    virtual region_set_t getModifiedRegions(const llvm::Function &F) override;
    
    virtual region_set_t getNewRegions(const llvm::Function &F) override;
    
    virtual region_set_t getAccessedRegions(llvm::CallInst &I) override;
    
    virtual region_set_t getOnlyReadRegions(llvm::CallInst &I) override;
    
    virtual region_set_t getModifiedRegions(llvm::CallInst &I) override;
    
    virtual region_set_t getNewRegions(llvm::CallInst &I) override;
    
    virtual llvm::StringRef getName() const override {
      return "LlvmDsaHeapAbstraction";
    }
  }; 
} // end namespace crab_llvm

#endif /* HAVE_DSA */
