#include "clam/config.h"

/**
 * Heap abstraction based on sea-dsa (https://github.com/seahorn/sea-dsa).
 */

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

#include "sea_dsa/AllocWrapInfo.hh"
#include "sea_dsa/Global.hh"
#include "sea_dsa/Graph.hh"

#include "SeaDsaHeapAbstractionDsaToRegion.hh"
#include "SeaDsaHeapAbstractionUtils.hh"
#include "clam/SeaDsaHeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "crab/common/debug.hpp"

#include <algorithm>
#include <set>

namespace clam {

using namespace sea_dsa;
using namespace llvm;

Region LegacySeaDsaHeapAbstraction::mkRegion(const Cell &c, RegionInfo ri) {
  auto id = getId(c);
  return Region(id, ri, getSingleton(id));
}

LegacySeaDsaHeapAbstraction::RegionId
LegacySeaDsaHeapAbstraction::getId(const Cell &c) {
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

// compute and cache the set of read, mod and new nodes of a whole
// function such that mod nodes are a subset of the read nodes and
// the new nodes are disjoint from mod nodes.
void LegacySeaDsaHeapAbstraction::computeReadModNewNodes(
    const llvm::Function &f) {

  if (!m_dsa || !(m_dsa->hasGraph(f))) {
    return;
  }

  Graph &G = m_dsa->getGraph(f);
  // hook: skip shadow mem functions created by SeaHorn
  // We treat them as readnone functions
  if (f.getName().startswith("shadow.mem"))
    return;

  seadsa_heap_abs_impl::NodeSet reach, retReach;
  seadsa_heap_abs_impl::argReachableNodes(f, G, reach, retReach);

  RegionVec reads, mods, news;
  for (const Node *n : reach) {
    if (!n->isRead() && !n->isModified()) {
      continue;
    }
    // Iterate over all cells of the node and extract regions from there
    // FIXME: n->types does not return pairs sorted by offset
    // build a vector of <cells, regions> if cells or regions do not match
    // error? if a cell is not disambiguate then region is untyped.
    for (auto &kv : n->types()) {

      Cell c(const_cast<Node *>(n), kv.first);
      RegionInfo r_info =
	DsaToRegion(c, m_dl, true /*split dsa nodes*/,
		    m_disambiguate_for_array_smashing,
		    m_disambiguate_unknown,
		    m_disambiguate_ptr_cast, m_disambiguate_external);

      if (r_info.get_type() != UNTYPED_REGION) {
        Region reg(mkRegion(c, r_info));
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
void LegacySeaDsaHeapAbstraction::computeReadModNewNodesFromCallSite(
    const llvm::CallInst &I, callsite_map_t &accessed_map,
    callsite_map_t &mods_map, callsite_map_t &news_map) {
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
  seadsa_heap_abs_impl::NodeSet reach, retReach;  
  seadsa_heap_abs_impl::argReachableNodes (CalleeF, calleeG, reach, retReach);
    
  // -- compute mapping between callee and caller graphs
  SimulationMapper simMap;
  Graph::computeCalleeCallerMapping(CS, calleeG, callerG, simMap);

  std::vector<region_bool_t> reads, mods, news;
  for (const Node *n : reach) {
    if (!n->isRead() && !n->isModified())
      continue;

    // Iterate over all cells of the node and extract regions
    // FIXME: n->types does not return pairs sorted by offset
    // build a vector of <cells, regions> if cells or regions do not match
    // error? if a cell is not disambiguate then region is untyped.
    for (auto &kv : n->types()) {

      Cell calleeC(const_cast<Node *>(n), kv.first);
      RegionInfo calleeRI =
	DsaToRegion(calleeC, m_dl, true /*split_dsa_nodes*/,
		    m_disambiguate_for_array_smashing,		    
		    m_disambiguate_unknown,
		    m_disambiguate_ptr_cast, m_disambiguate_external);
      
      if (calleeRI.get_type() != UNTYPED_REGION) {
        // Map the callee node to the node in the caller's callsite
        Cell callerC = simMap.get(calleeC);
        if (callerC.isNull()) {
          // This can cause an inconsistency between the number of
          // regions between a callsite and the callee's declaration.
          CLAM_ERROR("caller cell cannot be mapped to callee cell");
        }

        RegionInfo callerRI = DsaToRegion(
            callerC, m_dl, true /*split_dsa_nodes*/,
	    m_disambiguate_for_array_smashing,
	    m_disambiguate_unknown, m_disambiguate_ptr_cast,
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
        Region reg(mkRegion(callerC, calleeRI));
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
  // Region ret = getRegion(*(I.getParent()->getParent()), &I, &I);
  // if (!ret.isUnknown()) mods.push_back(ret);

  accessed_map[&I] = reads;
  mods_map[&I] = mods;
  news_map[&I] = news;
}

// Pre-compute all the information per function and callsites
void LegacySeaDsaHeapAbstraction::initialize(const llvm::Module &M) {

  CRAB_LOG(
      "heap-abs", llvm::errs()
                      << "========= HeapAbstraction using sea-dsa =========\n");

  CRAB_VERBOSE_IF(3, for (auto &F
                          : M) {
    if (m_dsa->hasGraph(F)) {
      auto &G = m_dsa->getGraph(F);
      G.write(errs());
      errs() << "\n";
    }
  });

  callsite_map_t cs_accessed, cs_mods, cs_news;
  for (auto const &F : M) {
    computeReadModNewNodes(F);
    auto InstIt = inst_begin(F), InstItEnd = inst_end(F);
    for (; InstIt != InstItEnd; ++InstIt) {
      if (const llvm::CallInst *Call =
              llvm::dyn_cast<llvm::CallInst>(&*InstIt)) {
        computeReadModNewNodesFromCallSite(*Call, cs_accessed, cs_mods,
                                           cs_news);
      }
    }
  }

  for (auto const &F : M) {
    RegionVec &readsF = m_func_accessed[&F];
    RegionVec &modsF = m_func_mods[&F];
    RegionVec &newsF = m_func_news[&F];

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
      CallInst *CI = dyn_cast<CallInst>(CS.getInstruction());
      if (!CI) {
        continue;
      }
      worklist.push_back(CI);

      std::vector<region_bool_t> &readsC = cs_accessed[CI];
      std::vector<region_bool_t> &modsC = cs_mods[CI];
      std::vector<region_bool_t> &newsC = cs_news[CI];

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
      for (unsigned i = 0, e = readsC.size(); i < e; i++) {
        readsB[i] = readsB[i] & readsC[i].second;
      }
      for (unsigned i = 0, e = modsC.size(); i < e; i++) {
        modsB[i] = modsB[i] & modsC[i].second;
      }
      for (unsigned i = 0, e = newsC.size(); i < e; i++) {
        newsB[i] = newsB[i] & newsC[i].second;
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

    while (!worklist.empty()) {
      CallInst *CI = worklist.back();
      worklist.pop_back();

      RegionVec readsC_out, modsC_out, newsC_out;
      std::vector<region_bool_t> &readsC = cs_accessed[CI];
      std::vector<region_bool_t> &modsC = cs_mods[CI];
      std::vector<region_bool_t> &newsC = cs_news[CI];
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
    }
  }

}

LegacySeaDsaHeapAbstraction::LegacySeaDsaHeapAbstraction(
    const llvm::Module &M, llvm::CallGraph &cg, const llvm::DataLayout &dl,
    const llvm::TargetLibraryInfo &tli,
    const sea_dsa::AllocWrapInfo &alloc_info, bool is_context_sensitive,
    bool disambiguate_for_array_smashing, bool disambiguate_unknown,
    bool disambiguate_ptr_cast, bool disambiguate_external)
    : m_dsa(nullptr), m_fac(nullptr), m_dl(dl), m_max_id(0),
      m_disambiguate_for_array_smashing(disambiguate_for_array_smashing),
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
    m_dsa = new sea_dsa::ContextSensitiveGlobalAnalysis(m_dl, tli, alloc_info,
                                                        cg, *m_fac);
  }

  m_dsa->runOnModule(const_cast<Module &>(M));
  
  initialize(M);
}

LegacySeaDsaHeapAbstraction::LegacySeaDsaHeapAbstraction(
    const llvm::Module &M, const llvm::DataLayout &dl,
    sea_dsa::GlobalAnalysis &dsa, 
    bool disambiguate_for_array_smashing, bool disambiguate_unknown,
    bool disambiguate_ptr_cast, bool disambiguate_external)
    : m_dsa(&dsa), m_fac(nullptr), m_dl(dl), m_max_id(0),
      m_disambiguate_for_array_smashing(disambiguate_for_array_smashing),
      m_disambiguate_unknown(disambiguate_unknown),
      m_disambiguate_ptr_cast(disambiguate_ptr_cast),
      m_disambiguate_external(disambiguate_external) {

  initialize(M);
}

LegacySeaDsaHeapAbstraction::~LegacySeaDsaHeapAbstraction() {
  if (m_fac) {
    // if m_fac is not null we know that we own m_dsa
    delete m_dsa;
    delete m_fac;
  }
}

bool LegacySeaDsaHeapAbstraction::isBasePtr(const llvm::Function &fn,
					    const llvm::Value *V) {

  if (!m_dsa || !m_dsa->hasGraph(fn)) {
    return false;
  }

  Graph &G = m_dsa->getGraph(fn);
  if (!G.hasCell(*V)) {
    return false;
  }

  const Cell &c = G.getCell(*V);
  if (c.isNull()) {
    return false;
  }

  Node *N = c.getNode();
  if (N->isOffsetCollapsed() || N->isArray() || N->isIntToPtr() || N->isPtrToInt()) {
    return false;
  }
  
  if (c.getOffset() != 0) {
    return false;
  }

  return true;
  
  // auto &allocSites = N->getAllocSites();
  // bool res = (allocSites.size() >= 1);
  // return res;
}

// f is used to know in which Graph we should search for V
Region LegacySeaDsaHeapAbstraction::getRegion(const llvm::Function &fn,
					      const llvm::Instruction *I/*unused*/,
					      const llvm::Value *V) {
  if (!m_dsa || !m_dsa->hasGraph(fn)) {
    return Region();
  }

  Graph &G = m_dsa->getGraph(fn);
  if (!G.hasCell(*V)) {
    return Region();
  }

  const Cell &c = G.getCell(*V);
  if (c.isNull()) {
    return Region();
  }

  RegionInfo r_info =
      DsaToRegion(c, m_dl, true /*split_dsa_nodes*/,
		  m_disambiguate_for_array_smashing,
		  m_disambiguate_unknown,
		  m_disambiguate_ptr_cast, m_disambiguate_external);

  if (r_info.get_type() == UNTYPED_REGION) {
    return Region();
  } else {
    return mkRegion(c, r_info);
  }
}

const llvm::Value *
LegacySeaDsaHeapAbstraction::getSingleton(RegionId region) const {
  auto const it = m_rev_node_ids.find(region);
  if (it == m_rev_node_ids.end())
    return nullptr;

  const Node *n = it->second;
  if (!n)
    return nullptr;
  if (const Value *v = n->getUniqueScalar()) {
    if (const GlobalVariable *gv = dyn_cast<const GlobalVariable>(v)) {
      seadsa_heap_abs_impl::isIntegerOrBool is_typed;
      if (is_typed(gv->getType()->getElementType()))
        return v;
    }
  }
  return nullptr;
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getAccessedRegions(const llvm::Function &fn) {
  return m_func_accessed[&fn];
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::Function &fn) {
  RegionVec v1 = m_func_accessed[&fn];
  RegionVec v2 = m_func_mods[&fn];
  std::set<Region> s2(v2.begin(), v2.end());

  // return v1 \ s2
  RegionVec out;
  out.reserve(v1.size());
  for (unsigned i = 0, e = v1.size(); i < e; ++i) {
    if (!s2.count(v1[i])) {
      out.push_back(v1[i]);
    }
  }
  return out;
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getModifiedRegions(const llvm::Function &fn) {
  return m_func_mods[&fn];
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getNewRegions(const llvm::Function &fn) {
  return m_func_news[&fn];
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getAccessedRegions(const llvm::CallInst &I) {
  return m_callsite_accessed[&I];
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::CallInst &I) {
  RegionVec v1 = m_callsite_accessed[&I];
  RegionVec v2 = m_callsite_mods[&I];
  std::set<Region> s2(v2.begin(), v2.end());

  // return v1 \ s2
  RegionVec out;
  out.reserve(v1.size());
  for (unsigned i = 0, e = v1.size(); i < e; ++i) {
    if (!s2.count(v1[i])) {
      out.push_back(v1[i]);
    }
  }
  return out;
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getModifiedRegions(const llvm::CallInst &I) {
  return m_callsite_mods[&I];
}

LegacySeaDsaHeapAbstraction::RegionVec
LegacySeaDsaHeapAbstraction::getNewRegions(const llvm::CallInst &I) {
  return m_callsite_news[&I];
}

} // namespace clam

