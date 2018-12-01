#pragma once

#include "crab_llvm/config.h"
#include "crab_llvm/HeapAbstraction.hh"
#include "llvm/ADT/StringRef.h"
#include <boost/unordered_map.hpp>

// forward declarations
namespace sea_dsa {
  class GlobalAnalysis;
  class Node;
  class Cell;
}

namespace crab_llvm {
  /* 
   * Wrapper for sea-dsa (https://github.com/seahorn/sea-dsa)
   */
  class SeaDsaHeapAbstraction: public HeapAbstraction {

   public:

     using typename HeapAbstraction::region_t;
     using typename HeapAbstraction::region_set_t;

   private:

    llvm::Module &m_M;
    
    sea_dsa::GlobalAnalysis *m_dsa;
    
    /// map from Node to id
    llvm::DenseMap<const sea_dsa::Node*, unsigned> m_node_ids;
    boost::unordered_map<unsigned, const sea_dsa::Node*> m_rev_node_ids;
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

    int getId(const sea_dsa::Cell& c);

    // compute and cache the set of read, mod and new nodes of a whole
    // function such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodes(const llvm::Function &f);

    // Compute and cache the set of read, mod and new nodes of a
    // callsite such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodesFromCallSite(llvm::CallInst &I);

   public:
    
    SeaDsaHeapAbstraction(llvm::Module &M, sea_dsa::GlobalAnalysis *dsa,
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
      return "SeaDsaHeapAbstraction";
    }
  }; 
} // end namespace crab_llvm

