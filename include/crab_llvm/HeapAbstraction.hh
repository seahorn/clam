#pragma once

/* Generic class for a heap analysis */

#include "crab_llvm/config.h"
#include "crab/common/debug.hpp"
#include <vector>

// forward declarations
namespace llvm {
  class Module;
  class Function;
  class Value;
  class Instruction;
  class raw_ostream;
  class StringRef;
}

namespace crab_llvm {

   typedef enum { UNTYPED_REGION = 0,
		  BOOL_REGION = 1,
		  INT_REGION = 2,
		  PTR_REGION = 3} region_type_t;

   class region_info {
     region_type_t m_region_type;
     // if the region contains a basic type (bool or integer) then
     // m_bitwidth is the bitwidth of the basic type. Otherwise, it is
     // 0.
     unsigned m_bitwidth;
     
   public:
     region_info(region_type_t t, unsigned b)
       : m_region_type(t)
       , m_bitwidth(b){}

     region_info(const region_info& other) = default;
     region_info& operator=(const region_info& other) = default;
     bool operator==(const region_info& o) {
       return (get_type() == o.get_type() &&
	       get_bitwidth() == o.get_bitwidth());
     }
     region_type_t get_type() const { return m_region_type;}
     unsigned get_bitwidth() const { return m_bitwidth;}
   };
  
   template<typename Mem>
   class Region {

   public:
     
     using region_id_t = long unsigned int;

   private:
     
     // A region different from unknown should represent a consecutive
     // sequence of bytes in memory that have compatible types and are
     // accessed uniformly so the analysis can use it in a safe
     // manner.

     friend struct DummyHeapAbstraction;
     #ifdef HAVE_DSA
     friend class LlvmDsaHeapAbstraction;
     #endif
     #ifdef HAVE_SEA_DSA
     friend class SeaDsaHeapAbstraction;
     #endif 

     
     Mem *m_mem;
     region_id_t m_id;
     region_info m_info;
     
     Region(Mem *mem, region_id_t id, region_info info)
       : m_mem(mem)
       , m_id(id)
       , m_info(info) {}
         
    public:

     Region()
       : m_mem(nullptr)
       , m_id(0),
	 m_info(region_info(UNTYPED_REGION,0)) { }

     Region(const Region<Mem>& other) = default;

     Region(Region<Mem>&& other) = default;     

     Region<Mem>& operator=(const Region<Mem>& other) = default;
    
     bool isUnknown() const {
       return (m_info.get_type() == UNTYPED_REGION);
     }
          
     const llvm::Value* getSingleton() const {
       if (!m_mem) 
         return nullptr;
       else
         return m_mem->getSingleton(m_id);
     }

     region_type_t get_type() const { return m_info.get_type();}

     unsigned get_bitwidth() const { return m_info.get_bitwidth();}
     
     bool operator<(const Region<Mem>& o) const {
       return (m_id < o.m_id);
     }

     bool operator==(const Region<Mem>& o) const {
       return (m_id == o.m_id);
     }

     region_id_t get_id() const {
       return m_id;
     }

     void write(llvm::raw_ostream& o) const {
       if (isUnknown()) {
         o << "unknown";
       } else {
         o << "R_" << m_id << ":";
	 switch(get_type()) {
	 case UNTYPED_REGION: o << "U"; break;
	 case BOOL_REGION: o << "B"; break;
	 case INT_REGION:  o << "I"; break;
	 case PTR_REGION:  o << "P"; break;
	 }
       }
     }

     friend llvm::raw_ostream& operator<<(llvm::raw_ostream &o,
					  const Region<Mem>& r) {
       r.write(o);
       return o;
     }

   };

   template<typename Mem>
   inline llvm::raw_ostream& operator<<(llvm::raw_ostream &o,
					std::vector<Region<Mem> > s) {
     o << "{";
     for (typename std::vector<Region<Mem> >::iterator it=s.begin(),
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
     
    public:

     typedef Region<HeapAbstraction> region_t;
     typedef std::vector<region_t> region_vector_t;
     typedef typename region_t::region_id_t region_id_t;

    protected:

     // return a value if the region corresponds to a single-cell
     // global memory cell, or nullptr otherwise.
     virtual const llvm::Value* getSingleton(region_id_t region) const = 0;
     
    public:

     HeapAbstraction() { }

     virtual ~HeapAbstraction() { }
     
    // Function is used to know in which function the Value lives
     virtual region_t getRegion(const llvm::Function&, llvm::Value*) = 0;

     // read and written regions by the function
     virtual region_vector_t getAccessedRegions(const llvm::Function& ) = 0;

     // only read regions by the function
     virtual region_vector_t getOnlyReadRegions(const llvm::Function& ) = 0;

     // written regions by the function     
     virtual region_vector_t getModifiedRegions(const llvm::Function& ) = 0;

     // regions that are reachable only from the return of the function     
     virtual region_vector_t getNewRegions(const llvm::Function& ) = 0;

    // read and written regions by the callee     
     virtual region_vector_t getAccessedRegions(llvm::CallInst& ) = 0;

     // only read regions by the function     
     virtual region_vector_t getOnlyReadRegions(llvm::CallInst& ) = 0;

     // written regions by the callee
     virtual region_vector_t getModifiedRegions(llvm::CallInst& ) = 0;

     // regions that are reachable only from the return of the callee     
     virtual region_vector_t getNewRegions(llvm::CallInst& ) = 0;

     virtual llvm::StringRef getName() const = 0;
   }; 

} // end namespace

