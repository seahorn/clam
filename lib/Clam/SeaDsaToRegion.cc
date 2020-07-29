#include "clam/config.h"

#include "SeaDsaToRegion.hh"
#include "SeaDsaHeapAbstractionUtils.hh"

#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Module.h"

#include "seadsa/Graph.hh"

namespace {

using namespace llvm;
using namespace seadsa;

// return true if the cell (n,o) contains a value of a specified
// type by is_typed
template <typename Pred>
static bool isTypedCell(const Node *n, unsigned o, Pred &is_typed) {
  if (!n) {
    return false;
  }

  if (n->hasAccessedType(o)) {
    for (const Type *t : n->getAccessedType(o)) {
      if (!is_typed(t)) {
        return false;
      }
    }
    return true;
  }
  return false;
}

// return true if the cell (n,o) points to an array of elements of
// some type specified by is_typed.
template <typename Pred>
static bool isTypedArrayCell(const Node *n, unsigned o, Pred &is_typed) {
  if (!n) {
    return false;
  }
  // sea-dsa only allows arrays at offset 0, otherwise it collapses
  // the node.
  if (!n->isArray() || o != 0)
    return false;

  if (n->hasAccessedType(o)) {
    for (const Type *t : n->getAccessedType(o)) {
      if (!is_typed(t)) {
        return false;
      }
    }
    return true;
  }
  return false;
}

// Given [lb_a,ub_a) and [lb_b,ub_b) return true if they intersect.
static bool intersectInterval(std::pair<uint64_t, uint64_t> a,
                              std::pair<uint64_t, uint64_t> b) {
  return (b.first >= a.first && b.first < a.second) ||
         (a.first >= b.first && a.first < b.second);
}

static uint64_t storageSize(const Type *t, const DataLayout &dl) {
  return dl.getTypeStoreSize(const_cast<Type *>(t));
}

static Optional<uint64_t> sizeOf(const Graph::Set &types,
                                 const DataLayout &dl) {
  if (types.isEmpty()) {
    return 0;
  } else {
    uint64_t sz = storageSize(*(types.begin()), dl);
    if (types.isSingleton()) {
      return sz;
    } else {
      auto it = types.begin();
      ++it;
      if (std::all_of(it, types.end(), [dl, sz](const Type *t) {
            return (storageSize(t, dl) == sz);
          })) {
        return sz;
      } else {
        return None;
      }
    }
  }
}

// Return true if there is another cell overlapping with c.
static bool isOverlappingCell(const Cell &c, const DataLayout &dl) {
  const Node *n1 = c.getNode();
  unsigned o1 = c.getOffset();
  if (!n1->hasAccessedType(o1)) {
    // this might be unsound but assuming cell overlaps might be too
    // pessimistic.
    return false;
  }

  auto c1_sz = sizeOf(n1->getAccessedType(o1), dl);
  if (c1_sz.hasValue()) {
    uint64_t s1 = c1_sz.getValue();
    for (auto &kv : n1->types()) {
      unsigned o2 = kv.first;
      if (o1 == o2)
        continue;
      auto c2_sz = sizeOf(kv.second, dl);
      if (c2_sz.hasValue()) {
        uint64_t s2 = c2_sz.getValue();
        if (intersectInterval({o1, o1 + s1}, {o2, o2 + s2})) {
          return true;
        }
      } else {
        return false;
      }
    }
    return false;
  }
  return true;
}

// Make sure that there is no other overlapping cells.
static bool isDisjointCell(const Cell &c, const DataLayout &dl) {
  if (isOverlappingCell(c, dl)) {
    CRAB_LOG("heap-abs-seadsa-to-region",
             errs() << "\tCannot be converted to region because overlaps "
                       "with other cells.\n";);
    return false;
  }

  const Node *n = c.getNode();
  unsigned offset = c.getOffset();

  if (offset >= n->size()) {
    CRAB_LOG("heap-abs-seadsa-to-region", errs() << "\tCannot be converted to region because "
                                   "cell is out-of-bounds.\n";);
    return false;
  }

  // XXX: I think we don't need this strong condition for soundness.
  // This would disallow an array of struct.
  //
  // if (n->isArray()) {
  //   if (std::any_of(n->types().begin(), n->types().end(),
  //                   [offset](const Node::accessed_types_type::value_type &kv)
  //                   {
  //                     return (kv.first != offset);
  //                   })) {
  //     CRAB_LOG("heap-abs-seadsa-to-region",
  //              errs() << "\tCannot be converted to region because cell's node
  //              is "
  //                     << " an array accessed with different offsets\n";);
  //     return false;
  //   }
  // }

  // XXX: do we need to ignore recursive nodes?
  return true;
}
} // end namespace

namespace clam {

using namespace llvm;
using namespace seadsa;

// SeaDsaToRegion succeeds if returned valued != UNTYPED_REGION
// TODO: consider cells that contain pointers
RegionInfo SeaDsaToRegion(const Cell &c, const DataLayout &dl,
			  bool disambiguate_unknown, bool disambiguate_ptr_cast,
			  bool disambiguate_external) {

  auto defaultRegionInfo = []() {return RegionInfo(region_type_t::UNTYPED_REGION, 0, false); };
			   
  if (c.isNull()) {
    return defaultRegionInfo();
  }

  const Node *n = c.getNode();
  unsigned offset = c.getOffset();

  CRAB_LOG("heap-abs-seadsa-to-region",
	   errs() << "*** Checking whether node at offset "
	   << offset << " can be converted to region ... \n"
	   << "\t" << *n << "\n";);

  if (!n->isModified() && !n->isRead()) {
    CRAB_LOG("heap-abs-seadsa-to-region",
	     errs()
	     << "\tWe do not bother to disambiguate it because "
	     << "it is never accessed.\n";);
    return defaultRegionInfo();
  }

  if (n->isOffsetCollapsed()) {
    CRAB_LOG("heap-abs-seadsa-to-region",
	     errs() << "\tCannot be converted to region: node is already collapsed.\n";);
    return defaultRegionInfo();
  }

  if (n->isIntToPtr() || n->isPtrToInt()) {
    if (!disambiguate_ptr_cast) {
      CRAB_LOG("heap-abs-seadsa-to-region",
               errs() << "\tCannot be converted to region: node is casted "
                      << "from/to an integer.\n";);
      return defaultRegionInfo();
    }
  }

  if (n->isExternal()) {
    if (!disambiguate_external) {
      CRAB_LOG(
          "heap-abs-seadsa-to-region",
          errs() << "\tCannot be converted to region: node is external.\n";);
      return defaultRegionInfo();
    }
  }

  seadsa_heap_abs_impl::isInteger int_pred;
  if (isTypedCell(n, offset, int_pred) || isTypedArrayCell(n, offset, int_pred)) {
    if (isDisjointCell(c, dl)) {
      CRAB_LOG("heap-abs-seadsa-to-region",
	       errs() << "\tDisambiguation succeed!\n"
	       << "Found INT_REGION at offset " << offset
	       << " with bitwidth=" << int_pred.m_bitwidth
	       << "\n\t" << *n << "\n";);
                                  
      return RegionInfo(region_type_t::INT_REGION, int_pred.m_bitwidth, c.getNode()->isArray());
    } else {
      CRAB_LOG("heap-abs-seadsa-to-region",
	       errs() << "\tCannot be converted to region due to overlapping.\n";);
      return defaultRegionInfo();
    }
  }

  seadsa_heap_abs_impl::isBool bool_pred;
  if (isTypedCell(n, offset, bool_pred) || isTypedArrayCell(n, offset, bool_pred)) {
    if (isDisjointCell(c, dl)) {
      CRAB_LOG("heap-abs-seadsa-to-region",
	       errs() << "\tDisambiguation succeed!\n"
	       << "Found BOOL_REGION at offset " << offset
	       << " with bitwidth=1\n"
	       << "\t" << *n << "\n";);
      return RegionInfo(region_type_t::BOOL_REGION, 1, c.getNode()->isArray());
    } else {
      CRAB_LOG("heap-abs-seadsa-to-region",
	       errs() << "\tCannot be converted to region due to overlapping.\n";);      
      return defaultRegionInfo();
    }
  }

  seadsa_heap_abs_impl::isPointer ptr_pred;
  if (isTypedCell(n, offset, ptr_pred)) {
    if (isDisjointCell(c, dl)) {
      CRAB_LOG("heap-abs-seadsa-to-region",
	       errs() << "\tDisambiguation succeed!\n"
	       << "Found POINTER_REGION at offset " << offset << "\n"
	       << "\t" << *n << "\n";);
      return RegionInfo(region_type_t::PTR_REGION, dl.getPointerSizeInBits(), c.getNode()->isArray());
    } else {
      CRAB_LOG("heap-abs-seadsa-to-region",
	       errs() << "\tCannot be converted to region due to overlapping.\n";);      
      return defaultRegionInfo();
    }
  }
  
  // TODO: modify here to consider cells containing pointers.
  CRAB_LOG(
      "heap-abs-seadsa-to-region",
      errs() << "\tCannot be converted to region: do not contain integer.\n";);

  return defaultRegionInfo();
}

} // end namespace clam
