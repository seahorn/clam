#pragma once

/**
 * The HeapAnalysis class wraps a Dsa-like analysis.
 **/

#include "clam/config.h"
#include "crab/support/debug.hpp"

#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
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
  // use context-insensitive sea-dsa
  CI_SEA_DSA,
  // use context-sensitive sea-dsa
  CS_SEA_DSA
};

enum class region_type_t {
  UNTYPED_REGION = 0,
  BOOL_REGION = 1,
  INT_REGION = 2,
  PTR_REGION = 3
};

class RegionInfo {
  region_type_t m_region_type;
  // if PTR_REGION then bitwidth is the pointer size
  // if INT_REGION or BOOL_REGION then bitwidth is the integer's
  //    bitwidth or 1.
  unsigned m_bitwidth; // number of bits

  //--------------------------------------//
  /// Auxiliary bits about the region
  //--------------------------------------//  
  // whether the region is coming from a "sequence" sea-dsa node
  bool m_is_sequence;
  // whether the region is possibly allocated in the heap
  bool m_is_heap;
  // whether the region is possibly cyclic
  bool m_is_cyclic;

public:
  RegionInfo(region_type_t t, unsigned b, bool is_seq, bool is_heap, bool is_cyclic)
      : m_region_type(t), m_bitwidth(b), m_is_sequence(is_seq),
        m_is_heap(is_heap), m_is_cyclic(is_cyclic) {}

  RegionInfo(const RegionInfo &other) = default;

  RegionInfo &operator=(const RegionInfo &other) = default;

  bool isUntyped() const {
    return m_region_type == region_type_t::UNTYPED_REGION;
  }

  bool hasSameType(const RegionInfo &o) const {
    // note that ignores flags m_is_sequence and m_is_heap
    return (m_region_type == o.m_region_type && m_bitwidth == o.m_bitwidth);
  }

  bool operator==(const RegionInfo &o) const {
    return hasSameType(o);
  }

  bool operator<(const RegionInfo &o) const {
    // note that ignores flags m_is_sequence and m_is_heap    
    return (m_region_type == o.m_region_type ?
	    m_bitwidth < o.m_bitwidth
	    : m_region_type < o.m_region_type);
  }
  
  bool hasCompatibleType(const RegionInfo &o) const {
    if (isUntyped() || o.isUntyped()) {
      return true;
    } else {
      return hasSameType(o);
    }
  }

  bool containScalar() const {
    return m_region_type == region_type_t::BOOL_REGION ||
           m_region_type == region_type_t::INT_REGION;
  }

  bool containPointer() const {
    return m_region_type == region_type_t::PTR_REGION;
  }

  // Return a pair with the type (boolean, integer or pointer) and
  // bitwidth in **bits**. The bitwidth is 1 if boolean region, size
  // of the pointer if pointer region, or size of the integer if
  // integer region. Otherwise, it returns 0 if bitwdith cannot be
  // determined.
  std::pair<region_type_t, unsigned> getType() const {
    return std::make_pair(m_region_type, m_bitwidth);
  }
  
  // Whether the region corresponds to a "sequence" node
  bool isSequence() const { return m_is_sequence; }
  
  // Whether the region is potentially allocated via a malloc-like
  // function.
  bool isHeap() const { return m_is_heap; }

  // Whether the region is cyclic (linked lists, trees, etc)
  bool isCyclic() const { return m_is_cyclic; }
  
  void write(llvm::raw_ostream &o) const {
    switch (m_region_type) {
    case region_type_t::UNTYPED_REGION:
      o << "U";
      break;
    case region_type_t::BOOL_REGION:
      o << "B";
      break;
    case region_type_t::INT_REGION:
      o << "i" << m_bitwidth;
      break;
    case region_type_t::PTR_REGION:
      o << "P";
      break;
    }
    if (isSequence()) {
      o << ":S";
    }
    if (isHeap()) {
      o << ":H";
    }
  }

  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &o,
                                       const RegionInfo &rgnInfo) {
    rgnInfo.write(o);
    return o;
  }
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
  const llvm::Value *m_singleton;

public:
  Region(RegionId id, RegionInfo info, const llvm::Value *singleton)
      : m_id(id), m_info(info), m_singleton(singleton) {}

  Region()
      : m_id(0),
        m_info(RegionInfo(region_type_t::UNTYPED_REGION, 0, false, false, false)),
        m_singleton(nullptr) {}

  Region(const Region &other) = default;

  Region(Region &&other) = default;

  Region &operator=(const Region &other) = default;

  RegionId getId() const { return m_id; }

  bool isUnknown() const {
    return (m_info.getType().first == region_type_t::UNTYPED_REGION);
  }

  const llvm::Value *getSingleton() const { return m_singleton; }

  RegionInfo getRegionInfo() const { return m_info; }

  bool operator<(const Region &o) const { return (m_id < o.m_id); }

  bool operator==(const Region &o) const { return (m_id == o.m_id); }

  void write(llvm::raw_ostream &o) const {
    o << "R_" << m_id << ":" << getRegionInfo();
  }

  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &o, const Region &r) {
    r.write(o);
    return o;
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &o,
                                     std::vector<Region> s) {
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
  using RegionVec = std::vector<Region>;
  using RegionId = typename Region::RegionId;

  // Add a new value if a new HeapAbstraction subclass is created
  // This is used to use static_cast.
  enum class ClassId { DUMMY, SEA_DSA };

  HeapAbstraction() {}

  virtual ~HeapAbstraction() {}

  virtual ClassId getClassId() const = 0;

  virtual llvm::StringRef getName() const = 0;

  // fun is used to know in which function ptr lives.
  // If not null, i is the instruction that uses ptr.
  virtual Region getRegion(const llvm::Function &fun,
                           const llvm::Value &ptr) = 0;

 // Use F and V to get the sea-dsa node associated to V and extracts
  // the region associated to nodes's field offset if any.
  virtual Region getRegion(const llvm::Function &F, const llvm::Value &V,
			   unsigned offset, const llvm::Type &AccessedType) = 0;
  
  /**========  These functions allow to purify functions ========**/

  // Read-Only regions reachable by function parameters and globals
  // but not returns
  virtual RegionVec getOnlyReadRegions(const llvm::Function &) const = 0;

  // Written regions reachable by function parameters and globals but
  // not returns
  virtual RegionVec getModifiedRegions(const llvm::Function &) const  = 0;

  // Regions that are reachable only from the return of the function
  virtual RegionVec getNewRegions(const llvm::Function &) const = 0;

  // Read-only regions at the caller that are mapped to callee's
  // formal parameters and globals.
  virtual RegionVec getOnlyReadRegions(const llvm::CallInst &) const = 0;

  // Written regions at the caller that are mapped to callee's formal
  // parameters and globals.
  virtual RegionVec getModifiedRegions(const llvm::CallInst &) const = 0;

  // Regions at the caller that are mapped to those that are only
  // reachable from callee's returns.
  virtual RegionVec getNewRegions(const llvm::CallInst &) const = 0;

  // Two regions are in the same equivalence class if they are
  // originated from the same memory object. 
  virtual std::vector<RegionVec> getEquivClassRegions(const llvm::Function &) const = 0;
};

} // namespace clam
