#include "clam/config.h"

/**
 * Heap abstraction based on sea-dsa (https://github.com/seahorn/sea-dsa).
 *
 * The implementation currently ignores InvokeInst and assumes that
 * the callgraph is complete.
 */

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

#include "seadsa/AllocWrapInfo.hh"
#include "seadsa/CallSite.hh"
#include "seadsa/DsaLibFuncInfo.hh"
#include "seadsa/Global.hh"
#include "seadsa/Graph.hh"

#include "SeaDsaHeapAbstractionUtils.hh"
#include "SeaDsaToRegion.hh"
#include "clam/SeaDsaHeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "crab/support/debug.hpp"

#include <algorithm>
#include <set>

namespace clam {

using namespace seadsa;
using namespace llvm;

Region SeaDsaHeapAbstraction::mkRegion(const Cell &c, RegionInfo ri) {
  auto id = getId(c);
  return Region(id, ri, getSingleton(id));
}

SeaDsaHeapAbstraction::RegionId SeaDsaHeapAbstraction::getId(const Cell &c) {
  const Node *n = c.getNode();
  unsigned offset = c.getOffset();

  auto it = m_node_ids.find(n);
  if (it != m_node_ids.end()) {
    return it->second + offset;
  }

  RegionId id = m_max_id;
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
  assert(n->size() > 0);
  m_max_id += n->size();
  return id + offset;
}

static std::vector<unsigned> extractFields(const Node *n,
                                           bool forceZeroOffset = true) {
  std::vector<unsigned> fields;
  if (n->isOffsetCollapsed()) {
    fields.push_back(0);
  } else {
    for (auto &kv : n->types()) {
      fields.push_back(kv.first);
    }
    if (fields.empty() && forceZeroOffset) {
      fields.push_back(0);
    }    
  }
  std::sort(fields.begin(), fields.end());  
  return fields;
}
   
// compute and cache the set of read, mod and new reachable nodes from
// globals and function's parameters and returns such that mod nodes
// are a subset of the read nodes and the new nodes are disjoint from
// mod nodes.
void SeaDsaHeapAbstraction::computeReadModNewNodes(const llvm::Function &f) {

  if (!m_dsa || !(m_dsa->hasGraph(f))) {
    return;
  }

  Graph &G = m_dsa->getGraph(f);
  // hook: skip shadow mem functions created by SeaHorn
  // We treat them as readnone functions
  if (f.getName().startswith("shadow.mem")) {
    return;
  }

  seadsa_heap_abs_impl::NodeSet reach, retReach;
  seadsa_heap_abs_impl::argReachableNodes(f, G, reach, retReach);

  RegionVec reads, mods, news;
  std::vector<RegionVec> equivClasses;
  for (const Node *n : reach) {
    bool isRetReach = retReach.count(n) > 0;

    if (!isRetReach && !n->isRead() && !n->isModified()) {
      continue;
    }

    // Extract all fields from the node
    std::vector<unsigned> fields = extractFields(n);
    // Create a region for each node's field
    for (auto field : fields) {
      Cell c(const_cast<Node *>(n), field);
      RegionInfo r_info =
          SeaDsaToRegion(c, m_dl, m_disambiguate_unknown,
                         m_disambiguate_ptr_cast, m_disambiguate_external);
      Region rgn = mkRegion(c, r_info);
      if (!isRetReach) {
        // reachable from function arguments or globals
        reads.push_back(rgn);
        if (n->isModified()) {
          mods.push_back(rgn);
        }
      } else {
        // not reachable from function arguments or globals but
        // reachable from return
        // if (n->isModified()) {
        news.push_back(rgn);
        //}
      }
    } // end for each field
  } // end for each node
  CRAB_LOG(
      "heap-abs-regions", llvm::errs()
                              << "### HEAP_ABS: " << f.getName() << " ###\n";
      llvm::errs()
      << "Read regions reachable from arguments and globals {";
      for (auto &r
           : reads) { llvm::errs() << r << ";"; } llvm::errs()
      << "}\n";
      llvm::errs() << "Modified regions reachable from arguments and globals {";
      for (auto &r
           : mods) { llvm::errs() << r << ";"; } llvm::errs()
      << "}\n";
      llvm::errs() << "New regions (only reachable from returns) {";
      for (auto &r
           : news) { llvm::errs() << r << ";"; } llvm::errs()
      << "}\n";);
  
  m_func_accessed[&f] = std::move(reads);
  m_func_mods[&f] = std::move(mods);
  m_func_news[&f] = std::move(news);  
}

/* 
 * Remember regions originated from the same seadsa node.
 * 
 * This method iterates *again* over inputs and outputs. Part of the
 * work can be done in computeReadModNewNode but I prefer to redo the
 * work and keep it clean and simple.
 */
void SeaDsaHeapAbstraction::computeEquivClasses(const llvm::Function &f) {
  if (!m_dsa || !(m_dsa->hasGraph(f))) {
    return;
  }
  
  Graph &G = m_dsa->getGraph(f);
  if (f.getName().startswith("shadow.mem")) {
    return;
  }

  // to store the equivalence classes
  std::vector<RegionVec> equivClasses;
  
  // reachable from inputs and outputs
  seadsa_heap_abs_impl::NodeSet reach, retReach;
  seadsa_heap_abs_impl::argReachableNodes(f, G, reach, retReach);
  // and also reachable from locals
  for(auto &kv: G.scalars()) {
    if (const Node *n = kv.second->getNode()) {
      markReachableNodes(n, reach);
    }    
  }
  
  for (const Node *n : reach) {
    bool isRetReach = retReach.count(n) > 0;
    if (!isRetReach && !n->isRead() && !n->isModified()) {
      continue;
    }
    RegionVec nodeRgns;    
    std::vector<unsigned> fields = extractFields(n);
    for (auto field : fields) {
      Cell c(const_cast<Node *>(n), field);
      RegionInfo r_info =
	SeaDsaToRegion(c, m_dl, m_disambiguate_unknown,
		       m_disambiguate_ptr_cast, m_disambiguate_external);
      nodeRgns.emplace_back(mkRegion(c, r_info));
    } 
    equivClasses.emplace_back(std::move(nodeRgns));
  }
  
  m_func_equiv_class_regions[&f] = std::move(equivClasses);  
}
  
// Compute and cache the set of read, mod and new nodes of a
// callsite such that mod nodes are a subset of the read nodes and
// the new nodes are disjoint from mod nodes.
void SeaDsaHeapAbstraction::computeReadModNewNodesFromCallSite(
    const llvm::CallInst &I, callsite_map_t &accessed_map,
    callsite_map_t &mods_map, callsite_map_t &news_map) {
  if (!m_dsa)
    return;

  /// ignore inline assembly
  if (I.isInlineAsm())
    return;

  DsaCallSite CS(I);

  if (!CS.getCallee()) {
    CRAB_LOG("heap-abs-regions", llvm::errs()
                                     << "HEAP_ABS: skipped " << I << "\n";);
    return;
  }

  // hook: skip shadow mem functions created by SeaHorn
  // We treat them as readnone functions
  if (CS.getCallee()->getName().startswith("shadow.mem")) {
    return;
  }

  const Function &CalleeF = *CS.getCallee();
  const Function &CallerF = *CS.getCaller();
  if (!m_dsa->hasGraph(CalleeF)) {
    return;
  }
  if (!m_dsa->hasGraph(CallerF)) {
    return;
  }

  Graph &callerG = m_dsa->getGraph(CallerF);
  Graph &calleeG = m_dsa->getGraph(CalleeF);

  // -- compute callee nodes reachable from arguments and returns
  seadsa_heap_abs_impl::NodeSet reach, retReach;
  seadsa_heap_abs_impl::argReachableNodes(CalleeF, calleeG, reach, retReach);

  // -- compute mapping between callee and caller graphs
  SimulationMapper simMap;
  Graph::computeCalleeCallerMapping(CS, calleeG, callerG, simMap);

  std::vector<region_bool_t> reads, mods, news;
  for (const Node *n : reach) {
    bool isRetReach = retReach.count(n) > 0;

    if (!isRetReach && !n->isRead() && !n->isModified()) {
      continue;
    }

    // Extract all fields from the node
    std::vector<unsigned> fields = extractFields(n);

    for (auto field : fields) {
      Cell calleeC(const_cast<Node *>(n), field);
      // Map the callee node to the node in the caller's callsite
      Cell callerC = simMap.get(calleeC);
      if (callerC.isNull()) {
        CLAM_ERROR("caller cell cannot be mapped to callee cell");
      }
      RegionInfo callerRI =
          SeaDsaToRegion(callerC, m_dl, m_disambiguate_unknown,
                         m_disambiguate_ptr_cast, m_disambiguate_external);
      Region rgn(mkRegion(callerC, callerRI));

      //  == begin sanity check ==
      RegionInfo calleeRI =
          SeaDsaToRegion(calleeC, m_dl, m_disambiguate_unknown,
                         m_disambiguate_ptr_cast, m_disambiguate_external);
      if (!calleeRI.hasCompatibleType(callerRI)) {
        CLAM_WARNING("Caller region info="
                     << callerRI
                     << " different from callee region info=" << calleeRI);
      }
      bool is_compat_callsite = calleeRI.hasCompatibleType(callerRI);
      //  == end sanity check ==

      if (!isRetReach) {
        reads.push_back({rgn, is_compat_callsite});
        if (n->isModified()) {
          mods.push_back({rgn, is_compat_callsite});
        }
      } else {
        // if (n->isModified()) {
        news.push_back({rgn, is_compat_callsite});
        //}
      }
    }
  }

  accessed_map[&I] = reads;
  mods_map[&I] = mods;
  news_map[&I] = news;

  CRAB_LOG(
      "heap-abs-regions2", llvm::errs() << "### HEAP_ABS: " << I << " ###\n";
      llvm::errs() << "Read regions at caller mapped to "
                   << "those reachable from callee's arguments and globals {";
      for (auto &r
           : reads) {
        llvm::errs() << r.first << "#" << r.second << ";";
      } llvm::errs()
      << "}\n";
      llvm::errs() << "Modified regions at caller mapped to "
                   << "those reachable from calle's arguments and globals {";
      for (auto &r
           : mods) {
        llvm::errs() << r.first << "#" << r.second << ";";
      } llvm::errs()
      << "}\n";
      llvm::errs()
      << "New regions at the caller (only reachable from callee's returns) {";
      for (auto &r
           : news) {
        llvm::errs() << r.first << "#" << r.second << ";";
      } llvm::errs()
      << "}\n";);
}

// Pre-compute all the information per function and callsites
void SeaDsaHeapAbstraction::initialize(const llvm::Module &M) {

  CRAB_LOG(
      "heap-abs",
      llvm::errs() << "========= HeapAbstraction using sea-dsa =========\n";);

  CRAB_LOG(
      "heap-abs-graphs", for (auto &F
                              : M) {
        if (m_dsa->hasGraph(F)) {
          llvm::errs() << "#### " << F.getName() << "####\n";
          auto &G = m_dsa->getGraph(F);
          G.write(llvm::errs());
          llvm::errs() << "\n";
        }
      });

  callsite_map_t CSAccessed, CSMods, CSNews;
  // For the sanity check implemented below
  DenseMap<const Function *, std::vector<DsaCallSite>> FunctionCallsMap;

  std::vector<const Function*> functions;
  for (auto const &F : M) {
    functions.push_back(&F);
  }
  std::sort(functions.begin(), functions.end());
  
  //for (auto const &F : M) {
  for (const Function *FF: functions) {
    auto &F = *FF;
    computeReadModNewNodes(F);
    computeEquivClasses(F);
    auto InstIt = inst_begin(F), InstItEnd = inst_end(F);
    for (; InstIt != InstItEnd; ++InstIt) {
      if (const CallInst *CI = dyn_cast<llvm::CallInst>(&*InstIt)) {
        computeReadModNewNodesFromCallSite(*CI, CSAccessed, CSMods, CSNews);

        // For the sanity check implemented below
        DsaCallSite CS(*CI);
        if (const Function *calleeF = CS.getCallee()) {
          if (!calleeF->empty()) {
            FunctionCallsMap[calleeF].push_back(CS);
          }
        }
      }
    }
  }

  /// Sanity check: compatibility check between function regions and
  /// all its callsites' regions. The check should always pass. Note
  /// that whether two region's types are compatible is defined by
  /// "hasCompatibleType".
  for (auto const &F : M) {
    RegionVec &readsF = m_func_accessed[&F];
    RegionVec &modsF = m_func_mods[&F];
    RegionVec &newsF = m_func_news[&F];

    // Initialized to true (i.e., all regions are consistent until the
    // opposite is proven)
    std::vector<bool> readsB(readsF.size(), true);
    std::vector<bool> modsB(modsF.size(), true);
    std::vector<bool> newsB(newsF.size(), true);

    /// First pass: for each memory region we check whether caller and
    /// callee agree on it.
    std::vector<const CallInst *> FCalls;
    for (DsaCallSite CS : FunctionCallsMap[&F]) {
      const CallInst *CI = cast<CallInst>(CS.getInstruction());
      FCalls.push_back(CI);

      std::vector<region_bool_t> &readsC = CSAccessed[CI];
      std::vector<region_bool_t> &modsC = CSMods[CI];
      std::vector<region_bool_t> &newsC = CSNews[CI];

      // Check that at the beginning caller and callee agree on the
      // number of memory regions, othewrwise there is nothing we can do.
      if (readsC.size() != readsF.size()) {
        CLAM_ERROR("Different num of read and modified regions between "
                   "callsite and its callee "
                   << F.getName() << " and callsite=" << *CI);
      }
      if (modsC.size() != modsF.size()) {
        CLAM_ERROR(
            "Different num of modified regions between callsite and its callee "
            << F.getName() << " and callsite=" << *CI);
      }
      if (newsC.size() != newsF.size()) {
        CLAM_ERROR(
            "Different num of new regions between callsite and its callee "
            << F.getName() << " and callsite=" << *CI);
      }

      // Keep track of inconsistent memory regions (i.e., regions on
      // which caller and callee disagree)
      for (unsigned i = 0, e = readsC.size(); i < e; i++) {
        readsB[i] = readsB[i] && readsC[i].second;
	if (!readsC[i].second) {
	  CLAM_WARNING("Caller and callee disagree on some read region type\n"
		       << "Callsite=" << *CI << "\n"
		       << "Caller=" << *CS.getCaller() << "\n");
	}
      }
      for (unsigned i = 0, e = modsC.size(); i < e; i++) {
        modsB[i] = modsB[i] && modsC[i].second;
	if (!modsC[i].second) {
	  CLAM_WARNING("Caller and callee disagree on some modified region type\n"
		       << "Callsite=" << *CI << "\n"
		       << "Caller=" << *CS.getCaller() << "\n");
	}	
      }
      for (unsigned i = 0, e = newsC.size(); i < e; i++) {
        newsB[i] = newsB[i] && newsC[i].second;
	if (!newsC[i].second) {
	  CLAM_WARNING("Caller and callee disagree on some new region type\n"
		       << "Callsite=" << *CI << "\n"
		       << "Caller=" << *CS.getCaller() << "\n");
	}
      }
    }

    /// Second phase: cache final regions.
    RegionVec readsF_out, modsF_out, newsF_out;
    for (unsigned i = 0, e = readsB.size(); i < e; i++) {
      if (readsB[i]) {
        readsF_out.push_back(readsF[i]);
      }
    }
    for (unsigned i = 0, e = modsB.size(); i < e; i++) {
      if (modsB[i]) {
        modsF_out.push_back(modsF[i]);
      }
    }
    for (unsigned i = 0, e = newsB.size(); i < e; i++) {
      if (newsB[i]) {
        newsF_out.push_back(newsF[i]);
      }
    }

    m_func_accessed[&F] = readsF_out;
    m_func_mods[&F] = modsF_out;
    m_func_news[&F] = newsF_out;

    while (!FCalls.empty()) {
      const CallInst *CI = FCalls.back();
      FCalls.pop_back();
      RegionVec readsC_out, modsC_out, newsC_out;
      std::vector<region_bool_t> &readsC = CSAccessed[CI];
      std::vector<region_bool_t> &modsC = CSMods[CI];
      std::vector<region_bool_t> &newsC = CSNews[CI];
      for (unsigned i = 0, e = readsB.size(); i < e; i++) {
        if (readsB[i]) {
          readsC_out.push_back(readsC[i].first);
        }
      }
      for (unsigned i = 0, e = modsB.size(); i < e; i++) {
        if (modsB[i]) {
          modsC_out.push_back(modsC[i].first);
        }
      }
      for (unsigned i = 0, e = newsB.size(); i < e; i++) {
        if (newsB[i]) {
          newsC_out.push_back(newsC[i].first);
        }
      }

      m_callsite_accessed[CI] = readsC_out;
      m_callsite_mods[CI] = modsC_out;
      m_callsite_news[CI] = newsC_out;
      CRAB_LOG(
          "heap-abs-regions", llvm::errs()
                                  << "### HEAP_ABS: " << *CI << " ###\n";
          llvm::errs()
          << "Read regions at caller mapped to "
          << "those reachable from callee's arguments and globals {";
          for (auto &r
               : readsC_out) { llvm::errs() << r << ";"; } llvm::errs()
          << "}\n";
          llvm::errs()
          << "Modified regions at caller mapped to "
          << "those reachable from calle's arguments and globals {";
          for (auto &r
               : modsC_out) { llvm::errs() << r << ";"; } llvm::errs()
          << "}\n";
          llvm::errs() << "New regions at the caller (only reachable from "
                          "callee's returns) {";
          for (auto &r
               : newsC_out) { llvm::errs() << r << ";"; } llvm::errs()
          << "}\n";);
    }
  }
}

SeaDsaHeapAbstraction::SeaDsaHeapAbstraction(
    const llvm::Module &M, llvm::CallGraph &cg,
    const llvm::TargetLibraryInfoWrapperPass &tli,
    const seadsa::AllocWrapInfo &alloc_info,
    const seadsa::DsaLibFuncInfo &spec_graph_info, bool is_context_sensitive,
    bool disambiguate_unknown, bool disambiguate_ptr_cast,
    bool disambiguate_external)
  : m_dsa(nullptr), m_fac(new SetFactory()), m_dl(M.getDataLayout()), m_max_id(0),
    m_disambiguate_unknown(disambiguate_unknown),
    m_disambiguate_ptr_cast(disambiguate_ptr_cast),
    m_disambiguate_external(disambiguate_external) {

  // -- Run sea-dsa
  if (!is_context_sensitive) {
    m_dsa = new seadsa::ContextInsensitiveGlobalAnalysis(
        m_dl, *(const_cast<llvm::TargetLibraryInfoWrapperPass *>(&tli)),
        alloc_info, spec_graph_info, cg, *m_fac, false);
  } else {
    m_dsa = new seadsa::ContextSensitiveGlobalAnalysis(
        m_dl, *(const_cast<llvm::TargetLibraryInfoWrapperPass *>(&tli)),
        alloc_info, spec_graph_info, cg, *m_fac);
  }
  m_dsa->runOnModule(const_cast<Module &>(M));
  
  initialize(M);
}

SeaDsaHeapAbstraction::SeaDsaHeapAbstraction(const llvm::Module &M,
                                             seadsa::GlobalAnalysis &dsa,
                                             bool disambiguate_unknown,
                                             bool disambiguate_ptr_cast,
                                             bool disambiguate_external)
    : m_dsa(&dsa), m_fac(nullptr), m_dl(M.getDataLayout()), m_max_id(0),
      m_disambiguate_unknown(disambiguate_unknown),
      m_disambiguate_ptr_cast(disambiguate_ptr_cast),
      m_disambiguate_external(disambiguate_external) {

  initialize(M);
}

SeaDsaHeapAbstraction::~SeaDsaHeapAbstraction() {
  if (m_fac) {
    // if m_fac is not null we know that we own m_dsa
    delete m_dsa;
  }
}

// f is used to know in which Graph we should search for V
Region SeaDsaHeapAbstraction::getRegion(const llvm::Function &fn,
                                        const llvm::Value &V) {
  if (!m_dsa || !m_dsa->hasGraph(fn)) {
    return Region();
  }

  Graph &G = m_dsa->getGraph(fn);
  if (!G.hasCell(V)) {
    return Region();
  }

  const Cell &c = G.getCell(V);
  if (c.isNull()) {
    return Region();
  }

  RegionInfo r_info =
      SeaDsaToRegion(c, m_dl, m_disambiguate_unknown, m_disambiguate_ptr_cast,
                     m_disambiguate_external);
  return mkRegion(c, r_info);
}

Region SeaDsaHeapAbstraction::getRegion(const llvm::Function &fn,
                                        const llvm::Value &V, unsigned offset,
                                        const Type &AccessedType) {

  if (!m_dsa || !m_dsa->hasGraph(fn)) {
    return Region();
  }

  Graph &G = m_dsa->getGraph(fn);
  if (!G.hasCell(V)) {
    return Region();
  }

  if (Node *n = G.getCell(V).getNode()) {
    if (n->hasAccessedType(offset)) {
      Cell c(n, offset);
      RegionInfo r_info =
          SeaDsaToRegion(c, m_dl, m_disambiguate_unknown,
                         m_disambiguate_ptr_cast, m_disambiguate_external);
      return mkRegion(c, r_info);
    }
  }
  return Region();
}

const llvm::Value *SeaDsaHeapAbstraction::getSingleton(RegionId region) const {
  auto const it = m_rev_node_ids.find(region);
  if (it == m_rev_node_ids.end())
    return nullptr;

  const Node *n = it->second;
  if (!n)
    return nullptr;
  if (const Value *v = n->getUniqueScalar()) {
    if (const GlobalVariable *gv = dyn_cast<const GlobalVariable>(v)) {
      seadsa_heap_abs_impl::isIntegerOrBool is_typed;
      if (is_typed(gv->getType()->getPointerElementType()))
        return v;
    }
  }
  return nullptr;
}

// Return v1 \ v2 by keeping the same ordering in v1.
// Precondition: v1 can have duplicates but v2 cannot.
//
// e.g., [1,3,3,4] \ [1,3] = [3,4]
// e.g., [1,3,3,3,4,5] \ [1,3,3] = [3,4,5]
static SeaDsaHeapAbstraction::RegionVec
stable_difference(SeaDsaHeapAbstraction::RegionVec &v1,
                  SeaDsaHeapAbstraction::RegionVec &v2) {
  // v1 and v2 can be modified because we pass copies

  std::sort(v2.begin(), v2.end());
  SeaDsaHeapAbstraction::RegionVec out;
  out.reserve(v1.size());
  for (auto rgn : v1) {
    auto lower = std::lower_bound(v2.begin(), v2.end(), rgn);
    if (lower != v2.end() && rgn == *lower) {
      // found
      //
      // remove from v2 so we handle correctly duplicates
      // FIXME: do not use erase, it's expensive
      v2.erase(lower);
    } else if (lower == v2.end() || rgn < *lower) {
      // not found
      out.push_back(rgn);
    }
  }
  return out;
}

template<class Map, class MapKey>
static SeaDsaHeapAbstraction::RegionVec lookup(const Map &m, const MapKey func) {
  auto it = m.find(func);
  if (it != m.end()) {
    return it->second;
  } else {
    return SeaDsaHeapAbstraction::RegionVec();
  }
}
  
SeaDsaHeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::Function &fn) const {
  RegionVec v1 = lookup(m_func_accessed, &fn);
  RegionVec v2 = lookup(m_func_mods,&fn);
  return stable_difference(v1, v2);
}

SeaDsaHeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getModifiedRegions(const llvm::Function &fn) const {
  return lookup(m_func_mods, &fn);
}

SeaDsaHeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getNewRegions(const llvm::Function &fn) const {
  return lookup(m_func_news, &fn);
}

SeaDsaHeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::CallInst &I) const {
  RegionVec v1 = lookup(m_callsite_accessed, &I);
  RegionVec v2 = lookup(m_callsite_mods, &I);
  return stable_difference(v1, v2);
}

SeaDsaHeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getModifiedRegions(const llvm::CallInst &I) const {
  return lookup(m_callsite_mods, &I);
}

SeaDsaHeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getNewRegions(const llvm::CallInst &I) const {
  return lookup(m_callsite_news, &I);
}

std::vector<SeaDsaHeapAbstraction::RegionVec>
SeaDsaHeapAbstraction::getEquivClassRegions(const llvm::Function &fn) const {
  auto it = m_func_equiv_class_regions.find(&fn);
  if (it != m_func_equiv_class_regions.end()) {
    return it->second;    
  } else {
    return std::vector<SeaDsaHeapAbstraction::RegionVec>();
  }
}
  
} // namespace clam
