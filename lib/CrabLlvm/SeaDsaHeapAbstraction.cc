#include "crab_llvm/config.h"

/**
 * Heap abstraction based on sea-dsa (https://github.com/seahorn/sea-dsa).
 */

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

#include "sea_dsa/Graph.hh"
#include "sea_dsa/Global.hh"
#include "sea_dsa/AllocWrapInfo.hh"

#include "crab_llvm/SeaDsaHeapAbstraction.hh"
#include "crab/common/debug.hpp"

#include <set>
#include <boost/unordered_map.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/algorithm/set_algorithm.hpp>
#include <boost/optional.hpp>

namespace crab_llvm {

using namespace llvm;
using namespace sea_dsa;

namespace seadsa_heap_abs_impl {
  
  template <typename Set>
  void set_difference(Set &s1, Set &s2) {
    Set s3;
    boost::set_difference(s1, s2, std::inserter(s3, s3.end()));
    std::swap(s3, s1);
  }
  
  template <typename Set>
  void set_union(Set &s1, Set &s2) {
    Set s3;
    boost::set_union(s1, s2, std::inserter(s3, s3.end()));
    std::swap(s3, s1);
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
    for (auto &kv : boost::make_iterator_range (g.globals_begin (),
						g.globals_end ())) {
      markReachableNodes (kv.second->getNode (), inputReach);
    }
    
    // return value
    if (g.hasRetCell (fn)) {
      markReachableNodes (g.getRetCell (fn).getNode(), retReach);
    }
  }

  
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
    }
    
    return true;
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
    }
    
    return true;
  }

  // Given [lb_a,ub_a) and [lb_b,ub_b) return true if they intersect.
  static bool intersect_interval(std::pair<uint64_t,uint64_t> a,
				 std::pair<uint64_t, uint64_t> b) {
    return (b.first >= a.first && b.first < a.second) ||
           (a.first >= b.first && a.first < b.second);
  }

  static uint64_t storage_size(const Type *t, const DataLayout &dl) {
    return dl.getTypeStoreSize(const_cast<Type*>(t));
  }
  
  static boost::optional<uint64_t>
  size_of(const Graph::Set& types, const DataLayout &dl) {
    if (types.isEmpty()) {
      return 0;
    } else {
      uint64_t sz = storage_size(*(types.begin()), dl);
      if (types.isSingleton()) {
	return sz;
      } else {
	auto it = types.begin();
	++it;
	if (std::all_of(it, types.end(),
			[dl,sz](const Type *t) {
			  return (storage_size(t, dl) == sz);
			})) {
	  return sz;
	} else {
	  return boost::none;
	}
      }
    }
  }

  // Return true if there is another cell overlapping with c.
  static bool is_overlapping_cell(const Cell& c, const DataLayout &dl) {
    const Node* n1 = c.getNode();
    unsigned o1 = c.getOffset();
    if (!n1->hasAccessedType(o1)) {
      // this might be unsound but assuming cell overlaps might be too
      // pessimistic.
      return false;
    }
    
    if (auto c1_sz = size_of(n1->getAccessedType(o1), dl)) {
      uint64_t s1 = *c1_sz;
      for (auto& kv: n1->types()) {
	unsigned o2 = kv.first;
	if (o1 == o2) continue;
	if (auto c2_sz =  size_of(kv.second, dl)) {
	  uint64_t s2 = *c2_sz;
	  if (intersect_interval({o1, o1+s1}, {o2, o2+s2})) {
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
  
  // canBeDisambiguated succeeds if returned valued != UNTYPED_REGION
  static region_info canBeDisambiguated(const Cell& c,
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
      CRAB_LOG("heap-abs", llvm::errs() << "\tDisambiguation succeed!\n";);
      return region_info(INT_REGION, int_pred.m_bitwidth);
    } 

    seadsa_heap_abs_impl::isBool bool_pred;
    if (isTypedCell(n, offset, bool_pred) || isTypedArrayCell(n, offset, bool_pred)) {
      CRAB_LOG("heap-abs", llvm::errs() << "\tDisambiguation succeed!\n";);
      return region_info(BOOL_REGION, 1);
    } 

    // TODO: modify here to consider cells containing pointers.
    CRAB_LOG("heap-abs",
	     llvm::errs() << "\tCannot be disambiguated: do not contain integer.\n";);
    
    return region_info(UNTYPED_REGION, 0);
  }

  ///////
  // class methods
  ///////

  // Return -1 if it cannot assign an id to the cell.
  int SeaDsaHeapAbstraction::getId(const Cell& c) {
    const Node* n = c.getNode();
    unsigned offset = c.getOffset();
    
    /** 
     * Begin extra conditions for array smashing-like abstractions 
     * TODO: we can move these extra conditions to canBeDisambiguated.
     **/
    if (!n->isModified() && !n->isRead()) {
      CRAB_LOG("heap-abs",
	       llvm::errs() << "\tBut discarding it because it is never accessed.\n";);
      return -1;      
    }

    // TODO: caching
    if (is_overlapping_cell(c, m_dl)) {
      // TOIMPROVE: we can assign same id to all overlapping cells but it
      // wouldn't be sound for array domains that ignore pointer
      // arithmetic. Maybe have a flag?
      CRAB_LOG("heap-abs",
	       llvm::errs() << "\tBut discarding it because overlaps with other cells.\n";);
      return -1;
    }
    
    if (n->isArray()) {
      // TOIMPROVE: we can assign the same id to all node's cells but it
      // would be unsound for array domains that ignore pointer
      // arithmetic. Maybe have a flag?
      // offset = 0;
      
      if (std::any_of(n->types().begin(), n->types().end(),
		      [offset](const Node::accessed_types_type::value_type& kv) {
			return (kv.first != offset);
		      })) {
	CRAB_LOG("heap-abs",
		 llvm::errs() << "\tBut discarding it because cell's node is marked as array "
		              << "and it can be accessed with different offsets.\n";);
	return -1;
      }
    }

    // FIXME: do we need to ignore recursive nodes?
    
    /** 
     * End extra conditions for array smashing-like abstractions 
     **/

    
    auto it = m_node_ids.find(n);
    if (it != m_node_ids.end()) {
      return it->second + offset;
    }
    
    unsigned id = m_max_id;
    m_node_ids[n] = id;

    // XXX: we only have the reverse map for the offset 0.  That's
    // fine because we use this map only in getSingleton which can
    // only succeed if offset 0.
    m_rev_node_ids[id] = n;
    
    if (n->size() == 0) {
      // XXX: nodes can have zero size
      assert (offset == 0);
      m_max_id++;
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
  void  SeaDsaHeapAbstraction::cacheReadModNewNodes(const llvm::Function& f) {
    
    if (!m_dsa || !(m_dsa->hasGraph(f))) {
      return;
    }

    Graph &G = m_dsa->getGraph(f);
    
    // hook: skip shadow mem functions created by SeaHorn
    // We treat them as readnone functions
    if (f.getName().startswith("shadow.mem")) return;

    
    std::set<const Node*> reach, retReach;
    seadsa_heap_abs_impl::argReachableNodes(f, G, reach, retReach);
    
    region_set_t reads, mods, news;
    for (const Node* n : reach) {
      if (!n->isRead() && !n->isModified()) {
	continue;
      }

      // Iterate over all cells of the node and extract regions from there
      for (auto &kv: n->types()) {

	Cell c(const_cast<Node*>(n), kv.first);
	region_info r_info = canBeDisambiguated(c, 
						m_disambiguate_unknown,
						m_disambiguate_ptr_cast,
						m_disambiguate_external);
	
	if (r_info.get_type() != UNTYPED_REGION) {
	  int id = getId(c);
	  if (id < 0) continue;	  
	  if ((n->isRead() || n->isModified()) && !retReach.count(n)) {
	    reads.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	  }
	  if (n->isModified() && !retReach.count(n)) {
	    mods.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	  }
	  if (n->isModified() && retReach.count(n)) {
	    news.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
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
  void  SeaDsaHeapAbstraction::cacheReadModNewNodesFromCallSite(llvm::CallInst& I) {
    
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
    std::set<const Node*> reach;
    std::set<const Node*> retReach;
    seadsa_heap_abs_impl::argReachableNodes (CalleeF, calleeG, reach, retReach);
    
    // -- compute mapping between callee and caller graphs
    SimulationMapper simMap;
    Graph::computeCalleeCallerMapping (CS, calleeG, callerG, simMap); 

    region_set_t reads, mods, news;
    for (const Node* n : reach) {
      if (!n->isRead() && !n->isModified())
	continue;

      // Iterate over all cells of the node and extract regions
      for (auto &kv: n->types()) {

	Cell c(const_cast<Node*>(n), kv.first);
	region_info r_info = canBeDisambiguated(c, 
						m_disambiguate_unknown,
						m_disambiguate_ptr_cast,
						m_disambiguate_external);
	
	if (r_info.get_type() != UNTYPED_REGION) {
	  // Map the callee node to the node in the caller's callsite
	  Cell callerC = simMap.get(c);
	  if (callerC.isNull()) {
	    continue;
	  }
	  int id = getId(callerC);
	  if (id < 0) continue;
	  if ((n->isRead() || n->isModified()) && !retReach.count(n)) {
	    reads.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	  } 
	  if (n->isModified() && !retReach.count(n)) {
	    mods.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	  }
	  if (n->isModified() && retReach.count(n)) {
	    news.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	  }
	}
      }
    }
    
    // -- add the region of the lhs of the call site
    region_t ret = getRegion(*(I.getParent()->getParent()), &I);
    if (!ret.isUnknown()) mods.insert(ret); 
    
    m_callsite_accessed[&I] = reads; 
    m_callsite_mods[&I] = mods; 
    m_callsite_news[&I] = news; 
  }

  SeaDsaHeapAbstraction::SeaDsaHeapAbstraction(llvm::Module& M, llvm::CallGraph& cg,
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
    
    m_dsa->runOnModule(m_m);
    
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

    // populate caches
    for (auto &F: boost::make_iterator_range(m_m)) {    
      cacheReadModNewNodes(F);
      llvm::inst_iterator InstIt = inst_begin(F), InstItEnd = inst_end(F);
      for (; InstIt != InstItEnd; ++InstIt) {
	if (llvm::CallInst *Call = llvm::dyn_cast<llvm::CallInst>(&*InstIt)) {
	  cacheReadModNewNodesFromCallSite(*Call);
	}
      }
    }
  }

  SeaDsaHeapAbstraction::~SeaDsaHeapAbstraction() {
    delete m_dsa;
    delete m_fac;
  }
  
  // f is used to know in which Graph we should search for V
  SeaDsaHeapAbstraction::region_t
  SeaDsaHeapAbstraction::getRegion(const llvm::Function& fn, llvm::Value* V)  {
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
    
    // -- A cell might not be translated to a memory region because
    //    either canBeDisambiguated or getId fails.
    region_info r_info = canBeDisambiguated(c,
					    m_disambiguate_unknown,
					    m_disambiguate_ptr_cast,
					    m_disambiguate_external);
    
    if (r_info.get_type() == UNTYPED_REGION) {
      return region_t();
    } else {
      int id = getId(c);
      if (id < 0) {
	return region_t();
      } else {
	return region_t(static_cast<HeapAbstraction*>(this), id, r_info);
      }
    }
  }
  
  const llvm::Value* SeaDsaHeapAbstraction::getSingleton(int region) const {
    auto const it = m_rev_node_ids.find(region);
    if (it == m_rev_node_ids.end()) 
      return nullptr;
    //  TODO: consider also singleton containing pointers.
    seadsa_heap_abs_impl::isIntegerOrBool pred;
    return getTypedSingleton(it->second, pred);
  }
  
  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getAccessedRegions(const llvm::Function& fn)  {
    return m_func_accessed[&fn];
  }

  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::Function& fn)  {
    region_set_t s1 = m_func_accessed[&fn];
    region_set_t s2 = m_func_mods[&fn];
    seadsa_heap_abs_impl::set_difference(s1,s2);
    return s1;
  }
  
  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getModifiedRegions(const llvm::Function& fn) {
    return m_func_mods[&fn];
  }
  
  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getNewRegions(const llvm::Function& fn) {
    return m_func_news[&fn];
  }
  
  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getAccessedRegions(llvm::CallInst& I) {
    return m_callsite_accessed[&I];
  }
  
  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getOnlyReadRegions(llvm::CallInst& I)  {
    region_set_t s1 = m_callsite_accessed[&I];
    region_set_t s2 = m_callsite_mods[&I];
    seadsa_heap_abs_impl::set_difference(s1,s2);
    return s1;
  }
  
  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getModifiedRegions(llvm::CallInst& I) {
    return m_callsite_mods[&I];
  }
  
  SeaDsaHeapAbstraction::region_set_t
  SeaDsaHeapAbstraction::getNewRegions(llvm::CallInst& I)  {
    return m_callsite_news[&I];
  }  
  
} // end namespace
