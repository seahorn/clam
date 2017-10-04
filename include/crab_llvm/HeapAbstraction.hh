#ifndef __HEAP_ABSTRACTION_HH_
#define __HEAP_ABSTRACTION_HH_

/* Wrapper for a heap abstraction used for heap disambiguation */

#include "crab_llvm/config.h"

#include <set>

// forward declarations
namespace llvm {
  class Module;
  class Function;
  class Value;
  class Instruction;
  class raw_ostream;
}

namespace crab_llvm {

   typedef enum { UNTYPED_REGION = 0,
		  BOOL_REGION = 1,
		  INT_REGION = 2,
		  PTR_REGION = 3} region_type_t;
  
   template<typename Mem>
   class Region {

     // A region different from unknown should represent a consecutive
     // sequence of bytes in memory that have compatible types and are
     // accessed uniformly so the analysis can use it in a safe
     // manner.

     friend struct DummyHeapAbstraction;
     #ifdef HAVE_DSA
     friend class LlvmDsaHeapAbstraction;
     #endif 

     Mem *m_mem;
     int m_id;
     region_type_t m_type;
     
     Region(Mem *mem, int id, region_type_t type)
       : m_mem(mem), m_id(id), m_type (type) { }
     
    public:

     Region(): m_mem(nullptr), m_id(-1), m_type(UNTYPED_REGION) { }
       
     bool isUnknown() const {
       return(m_id < 0 || m_type == UNTYPED_REGION);
     }
          
     const llvm::Value* getSingleton() const {
       if (!m_mem) 
         return nullptr;
       else
         return m_mem->getSingleton(m_id);
     }

     region_type_t get_type() const { return m_type;}
     
     bool operator<(const Region<Mem> & o) const {
       return (m_id < o.m_id);
     }

     bool operator==(const Region<Mem> & o) const {
       return (m_id == o.m_id);
     }

     int get_id() const {
       return m_id;
     }

     void write(llvm::raw_ostream& o) const {
       if (isUnknown()) 
         o << "unknown";
       else
         o << "R_" << m_id;
     }

     friend llvm::raw_ostream& operator<<(llvm::raw_ostream &o,
					  const Region<Mem>& r) {
       r.write(o);
       return o;
     }

   };

   template<typename Mem>
   inline llvm::raw_ostream& operator<<(llvm::raw_ostream &o,
					std::set<Region<Mem> > s) {
     o << "{";
     for (typename std::set<Region<Mem> >::iterator it=s.begin(),
	    et=s.end(); it!=et; ){
       o << *it;
       ++it;
       if (it != et) o << ",";
     }
     o << "}";
     return o;
   }

   class HeapAbstraction {
    
     template<typename Any>
     friend class Region;

    protected:

     // return a value if the region corresponds to a single-cell
     // global memory cell, or nullptr otherwise.
     virtual const llvm::Value* getSingleton(int region) const = 0;
     
    public:

     typedef Region<HeapAbstraction> region_t;
     typedef std::set<region_t> region_set_t;

    public:

     HeapAbstraction() { }
     
    // f is used to know in which DSGraph we should search for V
     virtual region_t getRegion(llvm::Function&, llvm::Value*) = 0;

     // read and written regions by the function
     virtual region_set_t getAccessedRegions(llvm::Function& ) = 0;

     // only read regions by the function
     virtual region_set_t getOnlyReadRegions(llvm::Function& ) = 0;

     // written regions by the function     
     virtual region_set_t getModifiedRegions(llvm::Function& ) = 0;

     // regions that are reachable only from the return of the function     
     virtual region_set_t getNewRegions(llvm::Function& ) = 0;

    // read and written regions by the callee     
     virtual region_set_t getAccessedRegions(llvm::CallInst& ) = 0;

     // only read regions by the function     
     virtual region_set_t getOnlyReadRegions(llvm::CallInst& ) = 0;

     // written regions by the callee
     virtual region_set_t getModifiedRegions(llvm::CallInst& ) = 0;

     // regions that are reachable only from the return of the callee     
     virtual region_set_t getNewRegions(llvm::CallInst& ) = 0;

     virtual const char* getHeapAbstractionName() const = 0;
   }; 

   struct DummyHeapAbstraction: public HeapAbstraction {

     using typename HeapAbstraction::region_t;
     using typename HeapAbstraction::region_set_t;

     DummyHeapAbstraction(): HeapAbstraction() { }
     const llvm::Value* getSingleton(int) const
     { return nullptr;}
     region_t getRegion(llvm::Function&, llvm::Value*)
     { return region_t(); }
     region_set_t getAccessedRegions(llvm::Function& )
     { return region_set_t(); }
     region_set_t getOnlyReadRegions(llvm::Function& )
     { return region_set_t(); }
     region_set_t getModifiedRegions(llvm::Function& )
     { return region_set_t(); }
     region_set_t getNewRegions(llvm::Function& )
     { return region_set_t(); }
     region_set_t getAccessedRegions(llvm::CallInst& )
     { return region_set_t(); }
     region_set_t getOnlyReadRegions(llvm::CallInst& )
     { return region_set_t(); }
     region_set_t getModifiedRegions(llvm::CallInst& )
     { return region_set_t(); }
     region_set_t getNewRegions(llvm::CallInst& )
     { return region_set_t(); }
     const char* getHeapAbstractionName() const
     { return "DummyHeapAbstraction"; }
   }; 

} // end namespace

#ifdef HAVE_DSA

#include <boost/unordered_map.hpp>

// forward declarations
namespace llvm {
  class DSNode;
  class DSGraph;
  class DSCallSite;
  class DataStructures;
}

namespace crab_llvm {

  /* 
   * Each DSA node is translated into a region.
   */
  class LlvmDsaHeapAbstraction: public HeapAbstraction {

   public:

     using typename HeapAbstraction::region_t;
     using typename HeapAbstraction::region_set_t;

   private:

    llvm::Module &m_M;
    llvm::DataStructures *m_dsa;
    
    /// map from DSNode to ids
    llvm::DenseMap<const llvm::DSNode*, unsigned> m_node_ids;
    boost::unordered_map<unsigned, const llvm::DSNode*> m_rev_node_ids;
    unsigned m_max_id;

    bool m_disambiguate_unknown;
    bool m_disambiguate_ptr_cast;
    bool m_disambiguate_external;
    
    /// reach - all reachable nodes from this function
    std::set<const llvm::DSNode*> m_reach;
    /// outReach - subset of reach that is only reachable from the
    ///            return node
    std::set<const llvm::DSNode*> m_retReach;
    
    llvm::DenseMap<const llvm::Function*, region_set_t> m_func_accessed;
    llvm::DenseMap<const llvm::Function*, region_set_t> m_func_mods;
    llvm::DenseMap<const llvm::Function*, region_set_t> m_func_news;
    //std::set<const llvm::Function*> cached_functions;

    llvm::DenseMap<const llvm::CallInst*, region_set_t> m_callsite_accessed;
    llvm::DenseMap<const llvm::CallInst*, region_set_t> m_callsite_mods;
    llvm::DenseMap<const llvm::CallInst*, region_set_t> m_callsite_news;
    //std::set<const llvm::CallInst*> cached_callsites;

   private:

    int getId(const llvm::DSNode *n, unsigned offset);
            
    template <typename Set1, typename Set2>
    void argReachableNodes(llvm::Function&f, Set1 &reach, Set2 &outReach);
    
    // compute and cache the set of read, mod and new nodes of a whole
    // function such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodes(llvm::Function &f);

    // Compute and cache the set of read, mod and new nodes of a
    // callsite such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodesFromCallSite(llvm::CallInst &I);

   public:
    
    LlvmDsaHeapAbstraction(llvm::Module &M, llvm::DataStructures *dsa,
			   bool disambiguate_unknown = false,
			   bool disambiguate_ptr_cast = false,
			   bool disambiguate_external = false);
    
    virtual region_t getRegion(llvm::Function &F, llvm::Value *V) override;
    virtual const llvm::Value* getSingleton(int region) const override;
    virtual region_set_t getAccessedRegions(llvm::Function &F) override;
    virtual region_set_t getOnlyReadRegions(llvm::Function &F) override;
    virtual region_set_t getModifiedRegions(llvm::Function &F) override;
    virtual region_set_t getNewRegions(llvm::Function &F) override;
    virtual region_set_t getAccessedRegions(llvm::CallInst &I) override;
    virtual region_set_t getOnlyReadRegions(llvm::CallInst &I) override;
    virtual region_set_t getModifiedRegions(llvm::CallInst &I) override;
    virtual region_set_t getNewRegions(llvm::CallInst &I) override;
    virtual const char* getHeapAbstractionName() const override
    { return "LlvmDsaHeapAbstraction"; }
  }; 
} // end namespace crab_llvm
#endif /* HAVE_DSA */
#endif 
