#pragma once

/**
 * The HeapAnalysis class wraps a Dsa-like analysis.
 **/

#include "clam/config.h"
#include "crab/common/debug.hpp"

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Value.h"
#include <vector>

// forward declarations
namespace llvm {
class Module;
class Function;
class Instruction;
class StringRef;
class CallInst;
} // namespace llvm

namespace clam {

////
// Dsa analysis for memory disambiguation
////
enum class heap_analysis_t {
  // disable heap analysis
  NONE, 
  // use llvm-dsa (only context-insensitive)
  LLVM_DSA,
  // use context-insensitive sea-dsa
  CI_SEA_DSA,
  // use context-sensitive sea-dsa
  CS_SEA_DSA
};

typedef enum {
  UNTYPED_REGION = 0,
  BOOL_REGION = 1,
  INT_REGION = 2,
  PTR_REGION = 3
} region_type_t;

class RegionInfo {
  region_type_t m_region_type;
  // if the region contains a basic type (bool or integer) then
  // m_bitwidth is the bitwidth of the basic type. Otherwise, it is 0.
  unsigned m_bitwidth;

public:
  RegionInfo(region_type_t t, unsigned b):
    m_region_type(t), m_bitwidth(b) {}

  RegionInfo(const RegionInfo &other) = default;
  
  RegionInfo &operator=(const RegionInfo &other) = default;
  
  bool operator==(const RegionInfo &o) {
    return (get_type() == o.get_type() &&
	    get_bitwidth() == o.get_bitwidth());
  }
  
  region_type_t get_type() const { return m_region_type; }
  
  unsigned get_bitwidth() const { return m_bitwidth; }
};

/** 
 * A region abstracts a Dsa node field to create a symbolic name.
 **/
class Region {
public:
  using RegionId = long unsigned int;

private:
  // A region different from unknown should represent a consecutive
  // sequence of bytes in memory that have compatible types and are
  // accessed uniformly so the analysis can use it in a safe
  // manner.

  // Unique id
  RegionId m_id;
  // type of the region
  RegionInfo m_info;
  // whether the region contains a singleton value or not.
  const llvm::Value* m_singleton;
  
public:

  Region(RegionId id, RegionInfo info, const llvm::Value *singleton)
    : m_id(id), m_info(info), m_singleton(singleton)
  {}
  
  Region()
    : m_id(0), m_info(RegionInfo(UNTYPED_REGION, 0)), m_singleton(nullptr)
  {}

  Region(const Region &other) = default;

  Region(Region &&other) = default;

  Region &operator=(const Region &other) = default;

  RegionId get_id() const {
    return m_id;
  }
  
  bool isUnknown() const {
    return (m_info.get_type() == UNTYPED_REGION);
  }

  const llvm::Value *getSingleton() const {
    return m_singleton;
  }

  RegionInfo getRegionInfo() const {
    return m_info;
  }
  
  bool operator<(const Region &o) const {
    return (m_id < o.m_id);
  }

  bool operator==(const Region &o) const {
    return (m_id == o.m_id);
  }

  void write(llvm::raw_ostream &o) const {
    if (isUnknown()) {
      o << "unknown";
    } else {
      o << "R_" << m_id << ":";
      switch (getRegionInfo().get_type()) {
      case UNTYPED_REGION:
        o << "U";
        break;
      case BOOL_REGION:
        o << "B";
        break;
      case INT_REGION:
        o << "I";
        break;
      case PTR_REGION:
        o << "P";
        break;
      }
    }
  }

  friend
  llvm::raw_ostream &operator<<(llvm::raw_ostream &o, const Region &r) {
    r.write(o);
    return o;
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &o, std::vector<Region> s) {
  o << "{";
  for (typename std::vector<Region>::iterator it = s.begin(), et = s.end();
       it != et;) {
    o << *it;
    ++it;
    if (it != et)
      o << ",";
  }
  o << "}";
  return o;
}


/* A convenient wrapper for a Dsa-like analysis */
class HeapAbstraction {
  friend class Region;

public:
  typedef std::vector<Region> RegionVec;
  typedef typename Region::RegionId RegionId;

  // Add a new value if a new HeapAbstraction subclass is created
  // This is used to use static_cast.
  enum class ClassId { DUMMY, LLVM_DSA, SEA_DSA};
  
  HeapAbstraction() {}

  virtual ~HeapAbstraction() {}

  virtual ClassId getClassId() const = 0;
  
  // XXX: ideally all these methods should be marked as const but
  // neither sea-dsa nor llvm-dsa provide APIs to allow that.

  // return true if V points to the base address (zero offset).
  virtual bool isBasePtr(const llvm::Function &F, const llvm::Value *V) = 0;
  
  // fun is used to know in which function ptr lives.
  // If not null, i is the instruction that uses ptr.
  virtual Region getRegion(const llvm::Function &fun,
			   const llvm::Instruction *i, const llvm::Value *ptr) = 0;

  // read and written regions by the function
  virtual RegionVec getAccessedRegions(const llvm::Function &) = 0;

  // only read regions by the function
  virtual RegionVec getOnlyReadRegions(const llvm::Function &) = 0;

  // written regions by the function
  virtual RegionVec getModifiedRegions(const llvm::Function &) = 0;

  // regions that are reachable only from the return of the function
  virtual RegionVec getNewRegions(const llvm::Function &) = 0;

  // read and written regions by the callee
  virtual RegionVec getAccessedRegions(const llvm::CallInst &) = 0;

  // only read regions by the function
  virtual RegionVec getOnlyReadRegions(const llvm::CallInst &) = 0;

  // written regions by the callee
  virtual RegionVec getModifiedRegions(const llvm::CallInst &) = 0;

  // regions that are reachable only from the return of the callee
  virtual RegionVec getNewRegions(const llvm::CallInst &) = 0;

  virtual llvm::StringRef getName() const = 0;
};

} // namespace clam
