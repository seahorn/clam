#include "clam/config.h"

#ifdef HAVE_SEA_DSA
/**
 * Heap abstraction based on sea-dsa (https://github.com/seahorn/sea-dsa).
 */

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

#include "sea_dsa/Graph.hh"
#include "sea_dsa/Global.hh"
#include "sea_dsa/AllocWrapInfo.hh"

#include "clam/SeaDsaHeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "crab/common/debug.hpp"

#include <set>
#include <algorithm>

namespace clam {

using namespace llvm;
using namespace sea_dsa;

namespace seadsa_heap_abs_impl {
  
template <typename Set>
void set_difference(Set &s1, Set &s2) {
  Set s3;
  std::set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
		      std::inserter(s3, s3.end()));
  std::swap(s3, s1);
}
  
template <typename Set>
void set_union(Set &s1, Set &s2) {
  Set s3;
  std::set_union(s1.begin(), s1.end(), s2.begin(), s2.end(),
		 std::inserter(s3, s3.end()));
  std::swap(s3, s1);
}

// v1 is not sorted
template<typename Vector, typename Set>
inline void vector_difference(Vector& v1, Set& s2) {
  Vector v3;
  v3.reserve(v1.size());
  for(unsigned i=0,e=v1.size();i<e;++i) {
    if (!s2.count(v1[i])) {
      v3.push_back(v1[i]);
    }
  }
  std::swap(v3,v1);
}

struct isInteger: std::unary_function<const llvm::Type*, bool> {
  unsigned m_bitwidth;
  isInteger(): m_bitwidth(0) {}
  bool operator()(const llvm::Type* t) {
    bool is_int = (t->isIntegerTy() && !t->isIntegerTy(1));
    if (is_int) {
      // XXX: We use bitwidth for overflow purposes so taking the
      // minimum is the most conservative choice.
      m_bitwidth = (m_bitwidth == 0 ? t->getIntegerBitWidth() :
		    std::min(m_bitwidth, t->getIntegerBitWidth()));
    }
    return is_int;
  }
};

struct isBool: std::unary_function<const llvm::Type*, bool> {
  bool operator()(const llvm::Type* t) const {
    return t->isIntegerTy(1);
  }
};

struct isIntegerOrBool: std::unary_function<const llvm::Type*, bool> {
  bool operator()(const llvm::Type* t) const {
    return t->isIntegerTy();
  }
};

template <typename Set>
void markReachableNodes (const Node *n, Set &set) {
  if (!n) return;
  assert (!n->isForwarding () && "Cannot mark a forwarded node");
    
  if (set.insert (n).second) 
    for (auto const &edg : n->links ())
      markReachableNodes (edg.second->getNode (), set);
}
  
  
template <typename Set>
void reachableNodes (const Function &fn, Graph &g, Set &inputReach, Set& retReach) {
  // formal parameters
  for (Function::const_arg_iterator I = fn.arg_begin(), E = fn.arg_end(); I != E; ++I) {
    const Value &arg = *I;
    if (g.hasCell (arg)) {
      Cell &c = g.mkCell (arg, Cell ());
      markReachableNodes (c.getNode (), inputReach);
    }
  }
    
  // globals
  for (auto &kv : llvm::make_range(g.globals_begin(), g.globals_end())) {
    markReachableNodes (kv.second->getNode (), inputReach);
  }
    
  // return value
  if (g.hasRetCell (fn)) {
    markReachableNodes (g.getRetCell (fn).getNode(), retReach);
  }
}

struct NodeOrdering {
  bool operator()(const Node *n1, const Node *n2) const {
    return n1->getId() < n2->getId();
  }
};
  
/// Computes Node reachable from the call arguments in the graph.
/// reach - all reachable nodes
/// outReach - subset of reach that is only reachable from the return node
template <typename Set1, typename Set2>
void argReachableNodes(const llvm::Function&fn, Graph &G,
		       Set1 &reach, Set2 &outReach) {
  reachableNodes (fn, G, reach, outReach);
  seadsa_heap_abs_impl::set_difference (outReach, reach);
  seadsa_heap_abs_impl::set_union (reach, outReach);
}
  
} // end namespace seadsa_heap_abs_impl

  
// return a value if the node corresponds to a typed single-cell
// global memory cell, or nullptr otherwise.
template<typename Pred>
static const llvm::Value* getTypedSingleton(const Node* n, Pred& is_typed) {
  if (!n) return nullptr;
  if (const llvm::Value* v = n->getUniqueScalar()) {
    if (const llvm::GlobalVariable *gv =
	llvm::dyn_cast<const llvm::GlobalVariable>(v)) {
      if (is_typed(gv->getType()->getElementType()))
	return v;
    }
  }
  return nullptr;
}

// return true if the cell (n,o) contains a value of a specified
// type by is_typed
template<typename Pred>
static bool isTypedCell(const Node* n, unsigned  o, Pred& is_typed) {
  if (!n) {
    return false;
  }

  if (n->hasAccessedType(o)) {
    for (const llvm::Type* t: n->getAccessedType(o)){
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
template<typename Pred>
static bool isTypedArrayCell(const Node* n, unsigned o, Pred& is_typed) {
  if (!n) {
    return false;
  }
  // sea-dsa only allows arrays at offset 0, otherwise it collapses
  // the node.
  if (!n->isArray() || o != 0)
    return false;

  if (n->hasAccessedType(o)) {
    for (const llvm::Type* t: n->getAccessedType(o)) {
      if (!is_typed(t)) {
	return false;
      }
    }
    return true;
  }
  return false;    
}

// Given [lb_a,ub_a) and [lb_b,ub_b) return true if they intersect.
static bool intersectInterval(std::pair<uint64_t,uint64_t> a,
			       std::pair<uint64_t, uint64_t> b) {
  return (b.first >= a.first && b.first < a.second) ||
    (a.first >= b.first && a.first < b.second);
}

static uint64_t storageSize(const Type *t, const DataLayout &dl) {
  return dl.getTypeStoreSize(const_cast<Type*>(t));
}
  
static llvm::Optional<uint64_t>
sizeOf(const Graph::Set& types, const DataLayout &dl) {
  if (types.isEmpty()) {
    return 0;
  } else {
    uint64_t sz = storageSize(*(types.begin()), dl);
    if (types.isSingleton()) {
      return sz;
    } else {
      auto it = types.begin();
      ++it;
      if (std::all_of(it, types.end(),
		      [dl,sz](const Type *t) {
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
static bool isOverlappingCell(const Cell& c, const DataLayout &dl) {
  const Node* n1 = c.getNode();
  unsigned o1 = c.getOffset();
  if (!n1->hasAccessedType(o1)) {
    // this might be unsound but assuming cell overlaps might be too
    // pessimistic.
    return false;
  }

  auto c1_sz = sizeOf(n1->getAccessedType(o1), dl);
  if (c1_sz.hasValue()) {
    uint64_t s1 = c1_sz.getValue();
    for (auto& kv: n1->types()) {
      unsigned o2 = kv.first;
      if (o1 == o2) continue;
      auto c2_sz =  sizeOf(kv.second, dl);
      if (c2_sz.hasValue()) {
	uint64_t s2 = c2_sz.getValue();
	if (intersectInterval({o1, o1+s1}, {o2, o2+s2})) {
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

// Extra conditions required for array smashing-like abstractions 
static bool isSafeForWeakArrayDomains(const Cell& c, const llvm::DataLayout& dl) {
  if (isOverlappingCell(c, dl)) {
    CRAB_LOG("heap-abs",
      llvm::errs() << "\tCannot be disambiguated because overlaps with other cells.\n";);
    return false;
  }
  
  const Node* n = c.getNode();
  unsigned offset = c.getOffset();

  if (offset >= n->size()) {
    CRAB_LOG("heap-abs",
      llvm::errs() << "\tCannot be disambiguated because cell is out-of-bounds.\n";);    
    return false;
  }
  
  if (n->isArray()) {
    if (std::any_of(n->types().begin(), n->types().end(),
		    [offset](const Node::accessed_types_type::value_type& kv) {
		      return (kv.first != offset);
		    })) {
      CRAB_LOG("heap-abs",
        llvm::errs() << "\tCannot be disambiguated because cell's node is "
	             << " an array accessed with different offsets\n";);
      return false;
    }
  }
  
  // XXX: do we need to ignore recursive nodes?
  return true;
}

// canBeDisambiguated succeeds if returned valued != UNTYPED_REGION
static region_info canBeDisambiguated(const Cell& c, const llvm::DataLayout& dl,
				      bool disambiguate_unknown,
				      bool disambiguate_ptr_cast,
				      bool disambiguate_external) {
  if (c.isNull()) {
    return region_info(UNTYPED_REGION, 0);
  }
    
  const Node* n = c.getNode();
  unsigned offset = c.getOffset();
    
  CRAB_LOG("heap-abs", 
	   llvm::errs () << "*** Checking whether node at offset " << offset
	                 << " can be disambiguated ... \n" 
	                 << "\t" << *n << "\n";);

  if (!n->isModified() && !n->isRead()) {
    CRAB_LOG("heap-abs",
	     llvm::errs() << "\tWe do not bother to disambiguate it because "
	                  << "it is never accessed.\n";);
    return region_info(UNTYPED_REGION, 0);      
  }
    
  if (n->isOffsetCollapsed()) {
    CRAB_LOG("heap-abs", 
	     llvm::errs() << "\tCannot be disambiguated: node is already collapsed.\n";);
    return region_info(UNTYPED_REGION, 0);
  }
    
  if (n->isIntToPtr() || n->isPtrToInt()) {
    if (!disambiguate_ptr_cast) {
      CRAB_LOG("heap-abs", 
	       llvm::errs() << "\tCannot be disambiguated: node is casted "
	                    << "from/to an integer.\n";);
      return region_info(UNTYPED_REGION, 0);
    }
  }
    
  if (n->isExternal()) {
    if (!disambiguate_external) {
      CRAB_LOG("heap-abs", 
	       llvm::errs() << "\tCannot be disambiguated: node is external.\n";);
      return region_info(UNTYPED_REGION, 0);
    }
  }
    
  seadsa_heap_abs_impl::isInteger int_pred;
  if (isTypedCell(n, offset, int_pred) || isTypedArrayCell(n, offset, int_pred)) {
    if (isSafeForWeakArrayDomains(c, dl)) {
      CRAB_LOG("heap-abs",
	       llvm::errs() << "\tDisambiguation succeed!\n"
	                    << "Found INT_REGION at offset " << offset
	                    << " with bitwidth=" << int_pred.m_bitwidth << "\n"
	                    << "\t" << *n << "\n";);    
      return region_info(INT_REGION, int_pred.m_bitwidth);
    } else {
      CRAB_LOG("heap-abs", 
      llvm::errs() << "\tCannot be disambiguated because it's not safe weak array domains\n";);
      return region_info(UNTYPED_REGION, 0);      
    } 
  } 

  seadsa_heap_abs_impl::isBool bool_pred;
  if (isTypedCell(n, offset, bool_pred) || isTypedArrayCell(n, offset, bool_pred)) {
    if (isSafeForWeakArrayDomains(c, dl)) {
      CRAB_LOG("heap-abs",
	       llvm::errs() << "\tDisambiguation succeed!\n"
	                    << "Found BOOL_REGION at offset " << offset
	                    << " with bitwidth=1\n"
	                    << "\t" << *n << "\n";);      
      return region_info(BOOL_REGION, 1);
    } else {
      CRAB_LOG("heap-abs", 
      llvm::errs() << "\tCannot be disambiguated because it's not safe weak array domains\n";);
      return region_info(UNTYPED_REGION, 0);            
    } 
  } 

  // TODO: modify here to consider cells containing pointers.
  CRAB_LOG("heap-abs",
	   llvm::errs() << "\tCannot be disambiguated: do not contain integer.\n";);
    
  return region_info(UNTYPED_REGION, 0);
}

///////
// class methods
///////

SeaDsaHeapAbstraction::region_t
SeaDsaHeapAbstraction::mkRegion(SeaDsaHeapAbstraction* heap_abs,
				const Cell& c, 
				region_info ri) {
  SeaDsaHeapAbstraction::region_t out(static_cast<HeapAbstraction*>(heap_abs), getId(c), ri);
  // // sanity check
  // if (const llvm::Value* v = out.getSingleton()) {
  //   if (const llvm::GlobalVariable *gv = llvm::dyn_cast<const llvm::GlobalVariable>(v)) {
  //     switch(out.get_type()) {
  //     case BOOL_REGION:
  // 	if (!gv->getType()->getElementType()->isIntegerTy(1)) {
  // 	  assert(false);	  
  // 	  CLAM_ERROR("Type mismatch while creating a heap Boolean region");
  // 	}
  // 	break;
  //     case INT_REGION:
  // 	if (!(gv->getType()->getElementType()->isIntegerTy() &&
  // 	      !gv->getType()->getElementType()->isIntegerTy(1))) {
  // 	  assert(false);
  // 	  CLAM_ERROR("Type mismatch while creating a heap integer region");	       
  // 	}
  // 	break;
  //     default:;
  //     }
  //   }
  // }
  return out;
}

SeaDsaHeapAbstraction::region_id_t SeaDsaHeapAbstraction::getId(const Cell& c) {
  const Node* n = c.getNode();
  unsigned offset = c.getOffset();
  
  auto it = m_node_ids.find(n);
  if (it != m_node_ids.end()) {
    return it->second + offset;
  }
    
  region_id_t id = m_max_id;
  m_node_ids[n] = id;

  // XXX: we only have the reverse map for the offset 0.  That's
  // fine because we use this map only in getSingleton which can
  // only succeed if offset 0.
  if (offset == 0 && n->getUniqueScalar()) {
    m_rev_node_ids[id] = n;
  }
    
  if (n->size() == 0) {
    ++m_max_id;
    return id;
  }
    
  // -- allocate enough ids for every byte of the object
  assert (n->size() > 0);
  m_max_id += n->size();
  return id + offset;
}

// compute and cache the set of read, mod and new nodes of a whole
// function such that mod nodes are a subset of the read nodes and
// the new nodes are disjoint from mod nodes.
void  SeaDsaHeapAbstraction::computeReadModNewNodes(const llvm::Function& f) {
    
  if (!m_dsa || !(m_dsa->hasGraph(f))) {
    return;
  }

  Graph &G = m_dsa->getGraph(f);
  // hook: skip shadow mem functions created by SeaHorn
  // We treat them as readnone functions
  if (f.getName().startswith("shadow.mem")) return;

  std::set<const Node*, seadsa_heap_abs_impl::NodeOrdering> reach, retReach;
  seadsa_heap_abs_impl::argReachableNodes(f, G, reach, retReach);

  std::vector<region_t> reads, mods, news;
  for (const Node* n : reach) {
    if (!n->isRead() && !n->isModified()) {
      continue;
    }
    // Iterate over all cells of the node and extract regions from there
    // FIXME: n->types does not return pairs sorted by offset
    // build a vector of <cells, regions> if cells or regions do not match error?
    // if a cell is not disambiguate then region is untyped.
    for (auto &kv: n->types()) {

      Cell c(const_cast<Node*>(n), kv.first);
      region_info r_info = canBeDisambiguated(c, m_dl, 
					      m_disambiguate_unknown,
					      m_disambiguate_ptr_cast,
					      m_disambiguate_external);
	
      if (r_info.get_type() != UNTYPED_REGION) {
	region_t reg(mkRegion(this, c, r_info));
	if ((n->isRead() || n->isModified()) && !retReach.count(n)) {
	  reads.push_back(reg);
	}
	if (n->isModified() && !retReach.count(n)) {
	  mods.push_back(reg);
	}
	if (n->isModified() && retReach.count(n)) {
	  news.push_back(reg);
	}
      }
    }
  }
  m_func_accessed[&f] = reads;
  m_func_mods[&f] = mods;
  m_func_news[&f] = news;
}

// Compute and cache the set of read, mod and new nodes of a
// callsite such that mod nodes are a subset of the read nodes and
// the new nodes are disjoint from mod nodes.
void  SeaDsaHeapAbstraction::computeReadModNewNodesFromCallSite(const llvm::CallInst& I,
								callsite_map_t& accessed_map,
								callsite_map_t& mods_map,
								callsite_map_t& news_map) {
  if (!m_dsa)
    return;
    
  /// ignore inline assembly
  if (I.isInlineAsm())
    return;

  ImmutableCallSite ICS(&I);
  DsaCallSite CS(ICS);

  if (!CS.getCallee())
    return;
    
  // hook: skip shadow mem functions created by SeaHorn
  // We treat them as readnone functions
  if (CS.getCallee()->getName().startswith("shadow.mem"))
    return;

  const Function &CalleeF = *CS.getCallee();
  const Function &CallerF = *CS.getCaller();
  if (!m_dsa->hasGraph(CalleeF))
    return;
  if (!m_dsa->hasGraph(CallerF))
    return;
    
  Graph &callerG = m_dsa->getGraph(CallerF);
  Graph &calleeG = m_dsa->getGraph(CalleeF);
    
  // -- compute callee nodes reachable from arguments and returns
  std::set<const Node*, seadsa_heap_abs_impl::NodeOrdering> reach, retReach;  
  seadsa_heap_abs_impl::argReachableNodes (CalleeF, calleeG, reach, retReach);
    
  // -- compute mapping between callee and caller graphs
  SimulationMapper simMap;
  Graph::computeCalleeCallerMapping (CS, calleeG, callerG, simMap); 

  std::vector<region_bool_t> reads, mods, news;
  for (const Node* n : reach) {
    if (!n->isRead() && !n->isModified())
      continue;

    // Iterate over all cells of the node and extract regions
    // FIXME: n->types does not return pairs sorted by offset
    // build a vector of <cells, regions> if cells or regions do not match error?
    // if a cell is not disambiguate then region is untyped.
    for (auto &kv: n->types()) {

      Cell calleeC(const_cast<Node*>(n), kv.first);
      region_info calleeRI = canBeDisambiguated(calleeC, m_dl, 
					      m_disambiguate_unknown,
					      m_disambiguate_ptr_cast,
					      m_disambiguate_external);

      if (calleeRI.get_type() != UNTYPED_REGION) {
	// Map the callee node to the node in the caller's callsite
	Cell callerC = simMap.get(calleeC);
	if (callerC.isNull()) {
	  // This can cause an inconsistency between the number of
	  // regions between a callsite and the callee's declaration.
	  CLAM_ERROR("caller cell cannot be mapped to callee cell");
	}

	region_info callerRI = canBeDisambiguated(callerC, m_dl,
						  m_disambiguate_unknown,
						  m_disambiguate_ptr_cast,
						  m_disambiguate_external);       
	/**  
	 * FIXME: assert(calleeRI == callerRI) should always hold.
	 * 
	 * However, there are sometimes inconsistencies between caller
	 * and callee at the callsite. This is possibly a problem in
	 * sea-dsa. For instance, we saw in the caller cells with
	 * offset 10 but size=10 while callee is offset 10 and
	 * size=12. This means that the caller is accessing
	 * out-of-bounds which shouldn't happen while the callee is
	 * ok. We temporary solve the problem by having a boolean that
	 * says whether caller and callee agree. Only consistent
	 * regions are exposed to clients.
	 **/
	bool is_consistent_callsite = (calleeRI == callerRI);
	region_t reg(mkRegion(this, callerC, calleeRI));
	if ((n->isRead() || n->isModified()) && !retReach.count(n)) {
	  reads.push_back({reg, is_consistent_callsite});
	} 
	if (n->isModified() && !retReach.count(n)) {
	  mods.push_back({reg, is_consistent_callsite});
	}
	if (n->isModified() && retReach.count(n)) {
	  news.push_back({reg, is_consistent_callsite});
	}
      } else {
	// if a callee's region is untyped then we should be ok
	// because when we extract regions from the function
	// declaration that region should be untyped.
      }
    }
  }
    
  // -- add the region of the lhs of the call site
  // region_t ret = getRegion(*(I.getParent()->getParent()), &I);
  // if (!ret.isUnknown()) mods.push_back(ret); 
    
  accessed_map[&I] = reads; 
  mods_map[&I] = mods; 
  news_map[&I] = news;
}

SeaDsaHeapAbstraction::SeaDsaHeapAbstraction(const llvm::Module& M, llvm::CallGraph& cg,
					     const llvm::DataLayout& dl,
					     const llvm::TargetLibraryInfo& tli,
					     const sea_dsa::AllocWrapInfo& alloc_info,
					     bool is_context_sensitive,
					     bool disambiguate_unknown,
					     bool disambiguate_ptr_cast,
					     bool disambiguate_external)
  : m_m(M), m_dl(dl), 
    m_dsa(nullptr), m_fac(nullptr), m_max_id(0),
    m_disambiguate_unknown(disambiguate_unknown),
    m_disambiguate_ptr_cast(disambiguate_ptr_cast),
    m_disambiguate_external(disambiguate_external) {

  // The factory must be alive while sea_dsa is in use    
  m_fac = new SetFactory();
    
  // -- Run sea-dsa
  if (!is_context_sensitive) {
    m_dsa = new sea_dsa::ContextInsensitiveGlobalAnalysis(m_dl, tli, alloc_info,
							  cg, *m_fac, false); 
  } else {
    m_dsa = new sea_dsa::ContextSensitiveGlobalAnalysis(m_dl, tli, alloc_info, cg, *m_fac);
  }
    
  m_dsa->runOnModule(const_cast<Module&>(m_m));
    
  // --- Pre-compute all the information per function and
  //     callsites
    
  CRAB_LOG("heap-abs", 
	   llvm::errs() << "========= HeapAbstraction using sea-dsa =========\n");

  CRAB_VERBOSE_IF(3, 
		  for (auto& F: M) {
		    if (m_dsa->hasGraph(F)) {
		      auto& G = m_dsa->getGraph(F);
		      G.write(errs());
		      errs() << "\n";
		    }
		  });

  callsite_map_t cs_accessed, cs_mods, cs_news;
  for (auto const &F: m_m) {    
    computeReadModNewNodes(F);
    auto InstIt = inst_begin(F), InstItEnd = inst_end(F);
    for (; InstIt != InstItEnd; ++InstIt) {
      if (const llvm::CallInst *Call = llvm::dyn_cast<llvm::CallInst>(&*InstIt)) {
	computeReadModNewNodesFromCallSite(*Call, cs_accessed, cs_mods, cs_news);
      }
    }
  }

  for (auto const &F: m_m) {
    std::vector<region_t>& readsF = m_func_accessed[&F];
    std::vector<region_t>& modsF  = m_func_mods[&F];
    std::vector<region_t>& newsF  = m_func_news[&F];

    // Initialized to true (i.e., all regions are consistent until the
    // opposite is proven)
    std::vector<bool> readsB(readsF.size(), true);
    std::vector<bool> modsB(modsF.size(), true);
    std::vector<bool> newsB(newsF.size(), true);
    
    std::vector<CallInst*> worklist;
    /// First pass: for each memory region we check whether caller and
    /// callee agree on it.
    for (const Use &U : F.uses()) {
      CallSite CS(U.getUser());
      // Must be a direct call instruction
      if (CS.getInstruction() == nullptr || !CS.isCallee(&U)) {
	continue;
      }
      CallInst* CI = dyn_cast<CallInst>(CS.getInstruction());
      if (!CI) {
	continue;
      }
      worklist.push_back(CI);

      std::vector<region_bool_t>& readsC = cs_accessed[CI];
      std::vector<region_bool_t>& modsC = cs_mods[CI];
      std::vector<region_bool_t>& newsC = cs_news[CI];

      // Check that at the beginning caller and callee agree on the
      // number of memory regions, othewrwise there is nothing we can do.
      if (readsC.size() != readsF.size()) {
	CLAM_ERROR("Different num of regions between callsite and its callee "
		       << F.getName());
      }
      if (modsC.size() != modsF.size()) {
	CLAM_ERROR("Different num of regions between callsite and its callee "
		       << F.getName());
      }
      if (newsC.size() != newsF.size()) {
	CLAM_ERROR("Different num of regions between callsite and its callee "
		       << F.getName());
      }

      // Keep track of inconsistent memory regions (i.e., regions on
      // which caller and callee disagree)
      for(unsigned i=0, e=readsC.size();i<e;i++) {
	readsB[i] = readsB[i] & readsC[i].second;
      }
      for(unsigned i=0, e=modsC.size();i<e;i++) {
	modsB[i] = modsB[i] & modsC[i].second;
      }
      for(unsigned i=0, e=newsC.size();i<e;i++) {
	newsB[i] = newsB[i] & newsC[i].second;
      }
    }

    /// Second phase: cache final regions.
    std::vector<region_t> readsF_out, modsF_out, newsF_out; 
    for(unsigned i=0, e=readsB.size();i<e;i++) {
      if (readsB[i]) {
	readsF_out.push_back(readsF[i]);
      }
    }
    for(unsigned i=0, e=modsB.size();i<e;i++) {
      if (modsB[i]) {
	modsF_out.push_back(modsF[i]);
      }
    }
    for(unsigned i=0, e=newsB.size();i<e;i++) {
      if (newsB[i]) {
	newsF_out.push_back(newsF[i]);
      }
    }
    
    m_func_accessed[&F] = readsF_out;
    m_func_mods[&F] = modsF_out;
    m_func_news[&F] = newsF_out;
    
    while(!worklist.empty()) {
      CallInst* CI = worklist.back();
      worklist.pop_back();
      
      std::vector<region_t> readsC_out, modsC_out, newsC_out;      
      std::vector<region_bool_t>& readsC = cs_accessed[CI];
      std::vector<region_bool_t>& modsC = cs_mods[CI];
      std::vector<region_bool_t>& newsC = cs_news[CI];
      for(unsigned i=0, e=readsB.size();i<e;i++) {
	if (readsB[i]) {
	  readsC_out.push_back(readsC[i].first);
	}
      }
      for(unsigned i=0, e=modsB.size();i<e;i++) {
	if (modsB[i]) {
	  modsC_out.push_back(modsC[i].first);
	}
      }
      for(unsigned i=0, e=newsB.size();i<e;i++) {
	if (newsB[i]) {
	  newsC_out.push_back(newsC[i].first);
	}
      }
      m_callsite_accessed[CI] = readsC_out;
      m_callsite_mods[CI] = modsC_out;
      m_callsite_news[CI] = newsC_out;            
    }
  }
}

SeaDsaHeapAbstraction::~SeaDsaHeapAbstraction() {
  delete m_dsa;
  delete m_fac;
}
  
// f is used to know in which Graph we should search for V
SeaDsaHeapAbstraction::region_t
SeaDsaHeapAbstraction::getRegion(const llvm::Function& fn, const llvm::Value* V)  {
  if (!m_dsa || !m_dsa->hasGraph(fn)) {
    return region_t();
  }
      
  Graph& G = m_dsa->getGraph(fn);
  if (!G.hasCell(*V)) {
    return region_t();
  }

  const Cell& c = G.getCell(*V);
  if (c.isNull()) {
    return region_t();
  }
    
  region_info r_info = canBeDisambiguated(c, m_dl,
					  m_disambiguate_unknown,
					  m_disambiguate_ptr_cast,
					  m_disambiguate_external);
    
  if (r_info.get_type() == UNTYPED_REGION) {
    return region_t();
  } else {
    return mkRegion(this, c, r_info);
  }
}
  
const llvm::Value* SeaDsaHeapAbstraction::getSingleton(region_id_t region) const {
  auto const it = m_rev_node_ids.find(region);
  if (it == m_rev_node_ids.end()) 
    return nullptr;
  //  TODO: consider also singleton containing pointers.
  seadsa_heap_abs_impl::isIntegerOrBool pred;
  return getTypedSingleton(it->second, pred);
}

SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getAccessedRegions(const llvm::Function& fn)  {
  return m_func_accessed[&fn];
}

SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::Function& fn)  {
  region_vector_t v1 = m_func_accessed[&fn]; 
  region_vector_t v2 = m_func_mods[&fn]; 
  std::set<SeaDsaHeapAbstraction::region_t> s2(v2.begin(), v2.end());
  seadsa_heap_abs_impl::vector_difference(v1,s2);
  return v1;
}
  
SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getModifiedRegions(const llvm::Function& fn) {
  return m_func_mods[&fn];
}
  
SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getNewRegions(const llvm::Function& fn) {
  return m_func_news[&fn];
}
  
SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getAccessedRegions(const llvm::CallInst& I) {
  return m_callsite_accessed[&I];
}
  
SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::CallInst& I)  {
  region_vector_t v1 = m_callsite_accessed[&I]; 
  region_vector_t v2 = m_callsite_mods[&I]; 
  std::set<SeaDsaHeapAbstraction::region_t> s2(v2.begin(), v2.end());
  seadsa_heap_abs_impl::vector_difference(v1,s2);
  return v1;
}
  
SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getModifiedRegions(const llvm::CallInst& I) {
  return m_callsite_mods[&I];
}
  
SeaDsaHeapAbstraction::region_vector_t
SeaDsaHeapAbstraction::getNewRegions(const llvm::CallInst& I)  {
  return m_callsite_news[&I];
}  
  
} // end namespace
#endif /*HAVE_SEA_DSA*/
