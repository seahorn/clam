#include "crab_llvm/config.h"

#ifdef HAVE_DSA
/**
 * This heap abstraction helps to reason about memory contents based
 * on llvm-dsa (https://github.com/seahorn/llvm-dsa).
 * 
 * It only disambiguates pointers that contain integers or arrays
 * whose elements are integers. Although this is restrictive there are
 * two key things to keep in mind:
 * 
 * - These pointers and arrays can be, in principle, stored deep in
 *   the heap.
 * 
 * - The DSA analysis is based on pointer uses and not on type
 *   definitions. This means that we could have an array of a struct
 *   type but if only an integer field is used in the program then
 *   it's considered as an array of integers.
 */
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

#include "dsa/DataStructure.h"
#include "dsa/DSGraph.h"
#include "dsa/DSNode.h"

#include "crab_llvm/LlvmDsaHeapAbstraction.hh"
#include "crab/common/debug.hpp"

#include <set>
#include <boost/unordered_map.hpp>
#include <boost/range/iterator_range.hpp>
#include "boost/range/algorithm/set_algorithm.hpp"

namespace crab_llvm {

  template <typename Set>
  inline void set_difference(Set &s1, Set &s2) {
    Set s3;
    boost::set_difference(s1, s2, std::inserter(s3, s3.end()));
    std::swap(s3, s1);
  }
  
  template <typename Set>
  inline void set_union(Set &s1, Set &s2) {
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

  struct isBool: std::unary_function<const llvm::Type*, bool>
  { bool operator()(const llvm::Type* t) const {
    return t->isIntegerTy(1); } };

  struct isIntegerOrBool: std::unary_function<const llvm::Type*, bool>
  { bool operator()(const llvm::Type* t) const {
    return t->isIntegerTy(); } };
  
  // return a value if the node corresponds to a typed single-cell
  // global memory cell, or nullptr otherwise.
  template<typename Pred>
  static const llvm::Value* getTypedSingleton(const llvm::DSNode *n, Pred& is_typed) {
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
  static bool isTypedCell(const llvm::DSNode *n, unsigned  o, Pred& is_typed) {
    if (!n) return false;

    // We need to traverse all types of the node
    for (auto &kv: boost::make_iterator_range(n->type_begin(), n->type_end())){
      if (kv.first == o) {
	// Check that all types of field are satisfied by is_typed.
	for (auto ty: *(kv.second))
	  if (!is_typed(ty)) return false;
	return true;
      }
    }
    return false;
  }
  
  // return true if the cell (n,o) points to an array of elements of
  // some type specified by is_typed.
  template<typename Pred>
  static bool isTypedArray(const llvm::DSNode *n, unsigned o, Pred& is_typed) {
    if (!n) return false;

    // llvm-dsa only allows arrays at offset 0, otherwise it collapses
    // the node.
    if (!n->isArrayNode() || o != 0)
      return false;

    // Check that the array has only offset 0
    if (std::distance(n->type_begin(), n->type_end()) != 1) return false;
    auto it = n->type_begin();
    if (it->first != 0) return false;

    // Check that all types of the array satisfy is_typed
    for (auto ty: *(it->second))
      if (!is_typed(ty)) return false;
    
    return true;
  }

  // canBeDisambiguated succeeds if returned valued != UNTYPED_REGION
  static region_info canBeDisambiguated(const llvm::DSNode *n, unsigned offset,
					bool disambiguate_unknown,
					bool disambiguate_ptr_cast,
					bool disambiguate_external) {
    CRAB_LOG("heap-abs", 
	     llvm::errs () << "\t*** Checking whether node at offset " << offset
	                   << " can be disambiguated ... \n";
	     n->dump(););

    if (n->isNodeCompletelyFolded() || n->isCollapsedNode()) {
      CRAB_LOG("heap-abs", 
	       llvm::errs() << "\tCannot be disambiguated: node is already collapsed.\n";);
      return region_info(UNTYPED_REGION, 0);
    }

    if (n->isUnknownNode()) {
      if (!disambiguate_unknown) {
	CRAB_LOG("heap-abs",
		 llvm::errs() << "\tCannot be disambiguated: node is unknown.\n";);
	return region_info(UNTYPED_REGION, 0);
      }
    }

    if (n->isIncompleteNode()) {
      if (!disambiguate_external) {
	CRAB_LOG("heap-abs", 
		 llvm::errs() << "\tCannot be disambiguated: node is incomplete.\n";);
	return region_info(UNTYPED_REGION, 0);
      }
    }
    
    if (n->isIntToPtrNode() || n->isPtrToIntNode()) {
      if (!disambiguate_ptr_cast) {
	CRAB_LOG("heap-abs", 
		 llvm::errs() << "\tCannot be disambiguated: node is casted "
		              << "from/to an integer.\n";);
	return region_info(UNTYPED_REGION, 0);
      }
    }
    
    if (n->isExternalNode()) {
      if (!disambiguate_external) {
	CRAB_LOG("heap-abs", 
		 llvm::errs() << "\tCannot be disambiguated: node is external.\n";);
	return region_info(UNTYPED_REGION, 0);
      }
    }
    
    isInteger int_pred;
    if (isTypedCell(n, offset, int_pred) || isTypedArray(n, offset, int_pred)) {
      CRAB_LOG("heap-abs", llvm::errs() << "\tDisambiguation succeed!\n";);
      return region_info(INT_REGION, int_pred.m_bitwidth);
    } 

    isBool bool_pred;
    if (isTypedCell(n, offset, bool_pred) || isTypedArray(n, offset, bool_pred)) {
      CRAB_LOG("heap-abs", llvm::errs() << "\tDisambiguation succeed!\n";);
      return region_info(BOOL_REGION, 1);
    } 

    // TODO: modify here to consider cells containing pointers.
    
    CRAB_LOG("heap-abs",
	     llvm::errs() << "\tCannot be disambiguated: do not contain integer.\n";);
    
    return region_info(UNTYPED_REGION, 0);
  }
  
  template <typename Set>
  static void markReachableNodes(const llvm::DSNode *n, Set &set) {
    if (!n) return;
    assert(!n->isForwarding() && "Cannot mark a forwarded node");
    if (set.insert(n).second)
      for (auto &edg : boost::make_iterator_range(n->edge_begin(),
						  n->edge_end()))
	markReachableNodes(edg.second.getNode(), set);
  }
    
  template <typename Set>
  static void inputReachableNodes(const llvm::DSCallSite &cs,
				  llvm::DSGraph &dsg, Set &set) {
    markReachableNodes(cs.getVAVal().getNode(), set);
    if (cs.isIndirectCall()) markReachableNodes(cs.getCalleeNode(), set);
    for (unsigned i = 0, e = cs.getNumPtrArgs(); i != e; ++i)
      markReachableNodes(cs.getPtrArg(i).getNode(), set);
    
    // globals
    llvm::DSScalarMap &sm = dsg.getScalarMap();
    for (auto &gv : boost::make_iterator_range(sm.global_begin(), sm.global_end()))
      markReachableNodes(sm[gv].getNode(), set);
  }
  
  template <typename Set>
  static void retReachableNodes(const llvm::DSCallSite &cs, Set &set) { 
    markReachableNodes(cs.getRetVal().getNode(), set);
  }
    
  /// Computes DSNode reachable from the call arguments
  /// reach - all reachable nodes
  /// outReach - subset of reach that is only reachable from the return node
  template <typename Set1, typename Set2>
  static void argReachableNodesFromCall(llvm::DSCallSite CS,
					llvm::DSGraph &dsg, 
					Set1 &reach, Set2 &outReach) {
    inputReachableNodes(CS, dsg, reach);
    retReachableNodes(CS, outReach);
    set_difference(outReach, reach);
    set_union(reach, outReach);
  }

  template <typename Set1, typename Set2>
  static void argReachableNodes(const llvm::Function&f, llvm::DSGraph* g,
				Set1 &reach, Set2 &outReach) {
    llvm::DSCallSite CCS = g->getCallSiteForArguments(f);
    argReachableNodesFromCall(CCS, *g, reach, outReach);
  }
  
  ///////
  // class methods
  ///////
  

  // Return -1 if it cannot assign an id.
  int LlvmDsaHeapAbstraction::getId(const llvm::DSNode *n, unsigned offset) {
    auto it = m_node_ids.find(n);
    if (it != m_node_ids.end()) {
      return it->second + offset;
    }

    /** 
     * FIXME: we need to check for some extra conditions so that we
     * can generate translations that can be analyzed by array
     * smashing-like abstractions. See SeaDsaHeapAbstraction.cc
     **/
    
    
    unsigned id = m_max_id;
    m_node_ids[n] = id;

    // XXX: we only have the reverse map for the offset 0.  That's
    // fine because we use this map only in getSingleton which can
    // only succeed if offset 0.
    m_rev_node_ids[id] = n;
    
    if (n->getSize() == 0) {
      // XXX: nodes can have zero size
      assert (offset == 0);
      m_max_id++;
      return id;
    }
    
    // -- allocate enough ids for every byte of the object
    assert (n->getSize() > 0);
    m_max_id += n->getSize();
    return id + offset;
  }

  // compute and cache the set of read, mod and new nodes of a whole
  // function such that mod nodes are a subset of the read nodes and
  // the new nodes are disjoint from mod nodes.
  void  LlvmDsaHeapAbstraction::cacheReadModNewNodes(const llvm::Function& f) {
    
    if (!m_dsa) return;
    
    // hook: skip shadow mem functions created by SeaHorn
    // We treat them as readnone functions
    if (f.getName().startswith("shadow.mem")) return;
    
    std::set<const llvm::DSNode*> reach, retReach;
    if (llvm::DSGraph *g = m_dsa->getDSGraph(f)) {
      argReachableNodes(f, g, reach, retReach);
    }
    
    //CRAB_LOG("heap-abs", llvm::errs() << f.getName() << "\n");
    
    region_set_t reads, mods, news;
    for (const llvm::DSNode* n : reach) {
      if (!n->isReadNode() && !n->isModifiedNode()) {
	continue;
      }
      
      // Iterate over each node field and extract regions from there
      for (auto &kv: boost::make_iterator_range(n->type_begin(), n->type_end())) {
	unsigned o = kv.first;
	region_info r_info = canBeDisambiguated(n, o,
						m_disambiguate_unknown,
						m_disambiguate_ptr_cast,
						m_disambiguate_external);
	if (r_info.get_type() == UNTYPED_REGION) {
	  continue;
	}

	int id = getId(n,o);
	if (id < 0) {
	  continue;
	}
	if ((n->isReadNode() || n->isModifiedNode()) && retReach.count(n) <= 0)
	  reads.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	if (n->isModifiedNode() && retReach.count(n) <= 0)
	  mods.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	if (n->isModifiedNode() && retReach.count(n)) 
	  news.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
			       
      }
    }
    m_func_accessed [&f] = reads;
    m_func_mods [&f] = mods;
    m_func_news [&f] = news;
  }

  // Compute and cache the set of read, mod and new nodes of a
  // callsite such that mod nodes are a subset of the read nodes and
  // the new nodes are disjoint from mod nodes.
  void  LlvmDsaHeapAbstraction::cacheReadModNewNodesFromCallSite(llvm::CallInst& I) {
    
    if (!m_dsa) return;
    
    /// ignore inline assembly
    if (I.isInlineAsm()) return;

    const llvm::Function* Called = llvm::CallSite(&I).getCalledFunction();

    if (!Called) {
      //llvm::errs () << "CRABLLVM WARNING: DSA cannot resolve " << I << "\n";
      return;
    }
    
    // hook: skip shadow mem functions created by SeaHorn
    // We treat them as readnone functions
    if (Called->getName().startswith("shadow.mem"))
      return;
    
    llvm::DSGraph *dsg = m_dsa->getDSGraph(*(I.getParent()->getParent()));
    llvm::DSCallSite CS = dsg->getDSCallSiteForCallSite(llvm::CallSite(&I));
    
    assert(CS.isDirectCall());
          
    if (!m_dsa->hasDSGraph(*CS.getCalleeFunc())) return;
    
    const llvm::Function &CF = *CS.getCalleeFunc();
    llvm::DSGraph *cdsg = m_dsa->getDSGraph(CF);
    if (!cdsg) return;
    
    // -- compute callee nodes reachable from arguments and returns
    llvm::DSCallSite CCS = cdsg->getCallSiteForArguments(CF);
    std::set<const llvm::DSNode*> reach;
    std::set<const llvm::DSNode*> retReach;
    argReachableNodesFromCall(CCS, *cdsg, reach, retReach);
    // XXX: assume that DSA analysis is context-insensitive
    // 
    //  llvm::DSGraph::NodeMapTy nodeMap;
    //  dsg->computeCalleeCallerMapping(CS, CF, *cdsg, nodeMap);
    
    region_set_t reads, mods, news;
    for (const llvm::DSNode* n : reach) {
      if (!n->isReadNode() && !n->isModifiedNode())
	continue;
      
      // Iterate over all node's fields and extract regions from
      // there.
      for (auto &kv: boost::make_iterator_range(n->type_begin(), n->type_end())) {
	unsigned o = kv.first;
	region_info r_info = canBeDisambiguated(n,o,
						m_disambiguate_unknown,
						m_disambiguate_ptr_cast,
						m_disambiguate_external);
	if (r_info.get_type() == UNTYPED_REGION) {
	  continue;
	}

	int id = getId(n,o);
	if (id < 0) {
	  continue;
	}
	
	if ((n->isReadNode() || n->isModifiedNode()) && retReach.count(n) <= 0)
	  reads.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	if (n->isModifiedNode() && retReach.count(n) <= 0)
	  mods.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info));
	if (n->isModifiedNode() && retReach.count(n))
	  news.insert(region_t(static_cast<HeapAbstraction*>(this), id, r_info)); 
			       
      }
    }
    
    // -- add the region of the lhs of the call site
    region_t ret = getRegion(*(I.getParent()->getParent()), &I);
    if (!ret.isUnknown()) mods.insert(ret); 
    
    m_callsite_accessed [&I] = reads; 
    m_callsite_mods [&I] = mods; 
    m_callsite_news [&I] = news; 
  }

  LlvmDsaHeapAbstraction::LlvmDsaHeapAbstraction(llvm::Module& M,
						 llvm::DataStructures* dsa,
						 bool disambiguate_unknown,
						 bool disambiguate_ptr_cast,
						 bool disambiguate_external)
    : m_M(M), m_dsa(dsa), m_max_id(0),
      m_disambiguate_unknown (disambiguate_unknown),
      m_disambiguate_ptr_cast (disambiguate_ptr_cast),
      m_disambiguate_external (disambiguate_external) {
    
    // --- Pre-compute all the information per function and
    //     callsites
    
    CRAB_LOG("heap-abs", 
	     llvm::errs() << "========= HeapAbstraction using llvm-dsa =========\n");
    
    for (auto &F: boost::make_iterator_range(m_M)) {
      cacheReadModNewNodes(F);
      
      llvm::inst_iterator InstIt = inst_begin(F), InstItEnd = inst_end(F);
      for (; InstIt != InstItEnd; ++InstIt) {
	if (llvm::CallInst *Call =
	    llvm::dyn_cast<llvm::CallInst>(&*InstIt)) {
	  cacheReadModNewNodesFromCallSite(*Call);
	}
      }
    }
  }

  // f is used to know in which DSGraph we should search for V
  LlvmDsaHeapAbstraction::region_t
  LlvmDsaHeapAbstraction::getRegion(const llvm::Function& F, llvm::Value* V)  {
    // Note each function has its own graph and a copy of the global
    // graph. Nodes in both graphs are merged.  However, m_dsa has
    // its own global graph which seems not to be merged with
    // function's graphs, and thus, it cannot be used here.
    if (!m_dsa) return region_t();
    
    llvm::DSGraph *dsg = m_dsa->getDSGraph(F);
    if (!dsg) return region_t();

    llvm::DSNodeHandle &cell = dsg->getNodeForValue(V);
    llvm::DSNode *n = cell.getNode();
    if (!n) {
      llvm::DSGraph *gDsg = dsg->getGlobalsGraph();
      cell = gDsg->getNodeForValue(V);
      n = cell.getNode();
    }
    if (!n) return region_t();

    region_info r_info = canBeDisambiguated(n, cell.getOffset(),
					    m_disambiguate_unknown,
					    m_disambiguate_ptr_cast,
					    m_disambiguate_external);

    if (r_info.get_type() == UNTYPED_REGION) {
      return region_t();
    } else {
      int id = getId(n,cell.getOffset());
      if (id < 0) {
	return region_t();
      } else {
	return region_t(static_cast<HeapAbstraction*>(this), id, r_info);
      }
    }
  }
  
  const llvm::Value* LlvmDsaHeapAbstraction::getSingleton(int region) const {
    auto const it = m_rev_node_ids.find(region);
    if (it == m_rev_node_ids.end()) 
      return nullptr;
    //  TODO: consider also singleton containing pointers.
    isIntegerOrBool pred;
    return getTypedSingleton(it->second, pred);
  }
  
  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getAccessedRegions(const llvm::Function& F)  {
    return m_func_accessed [&F];
  }

  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getOnlyReadRegions(const llvm::Function& F)  {
    region_set_t s1 = m_func_accessed [&F];
    region_set_t s2 = m_func_mods [&F];
    set_difference(s1,s2);
    return s1;
  }
  
  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getModifiedRegions(const llvm::Function& F) {
    return m_func_mods [&F];
  }
  
  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getNewRegions(const llvm::Function& F) {
    return m_func_news [&F];
  }
  
  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getAccessedRegions(llvm::CallInst& I) {
    return m_callsite_accessed [&I];
  }
  
  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getOnlyReadRegions(llvm::CallInst& I)  {
    region_set_t s1 = m_callsite_accessed [&I];
    region_set_t s2 = m_callsite_mods [&I];
    set_difference(s1,s2);
    return s1;
  }
  
  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getModifiedRegions(llvm::CallInst& I) {
    return m_callsite_mods [&I];
  }
  
  LlvmDsaHeapAbstraction::region_set_t
  LlvmDsaHeapAbstraction::getNewRegions(llvm::CallInst& I)  {
    return m_callsite_news [&I];
  }
} // end namespace
#endif 
