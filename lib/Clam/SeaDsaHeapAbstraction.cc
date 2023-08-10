#include "clam/config.h"

/**
 * Heap abstraction based on sea-dsa (https://github.com/seahorn/sea-dsa).
 *
 * The implementation currently ignores InvokeInst and assumes that
 * the callgraph is complete.
 */

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/ImmutableSet.h"
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
#include "clam/HeapAbstraction.hh"
#include "clam/SeaDsaHeapAbstraction.hh"
#include "clam/Support/Debug.hh"
#include "crab/support/debug.hpp"

#include <algorithm>
#include <memory>
#include <set>
#include <unordered_map>

namespace clam {

using namespace seadsa;
using namespace llvm;

class SeaDsaHeapAbstractionImpl {
  // XXX: We should use seadsa::Graph::SetFactory.
  // We copy here the definition of seadsa::Graph::SetFactory so
  // that we don't need to include Graph.hh
  using Set = llvm::ImmutableSet<llvm::Type *>;
  using SetFactory = typename Set::Factory;
  using callsite_map_t =
      llvm::DenseMap<const llvm::CallInst *, std::vector<std::pair<Region, bool>>>;

  Region mkRegion(const seadsa::Cell &c, RegionInfo ri);

  HeapAbstraction::RegionId getId(const seadsa::Cell &c);

  void initialize(const llvm::Module &M);

  // compute and cache the set of read, mod and new nodes of a whole
  // function such that mod nodes are a subset of the read nodes and
  // the new nodes are disjoint from mod nodes.
  void computeReadModNewNodes(const llvm::Function &f);

  // Compute and cache the set of read, mod and new nodes of a
  // callsite such that mod nodes are a subset of the read nodes and
  // the new nodes are disjoint from mod nodes.
  void computeReadModNewNodesFromCallSite(const llvm::CallInst &I,
                                          callsite_map_t &accessed,
                                          callsite_map_t &mods,
                                          callsite_map_t &news);

  void computeEquivClasses(const llvm::Function &f);

  const llvm::Value *getSingleton(HeapAbstraction::RegionId region) const;

public:
  // This constructor creates and owns a sea-dsa GlobalAnalysis instance and
  // run it on M.
  SeaDsaHeapAbstractionImpl(const llvm::Module &M, llvm::CallGraph &cg,
                            const llvm::TargetLibraryInfoWrapperPass &tli,
                            const seadsa::AllocWrapInfo &alloc_wrap_info,
                            const seadsa::DsaLibFuncInfo &spec_graph_info,
			    SeaDsaHeapAbstractionParams params);

  // This constructor takes an existing sea-dsa Global Analysis instance.
  // It doesn't own it.
  SeaDsaHeapAbstractionImpl(const llvm::Module &M, seadsa::GlobalAnalysis &dsa,
			    SeaDsaHeapAbstractionParams params);

  ~SeaDsaHeapAbstractionImpl();

  seadsa::GlobalAnalysis *getSeaDsa() { return m_dsa; }

  const seadsa::GlobalAnalysis *getSeaDsa() const { return m_dsa; }

  // Use F and V to get sea-dsa cell associated to it.
  Region getRegion(const llvm::Function &F, const llvm::Value &V);

  // Use F and V to get the sea-dsa node associated to V and extracts
  // the region associated to nodes's field offset if any.
  Region getRegion(const llvm::Function &F, const llvm::Value &V,
                   unsigned offset, const llvm::Type &AccessedType);

  HeapAbstraction::RegionVec getOnlyReadRegions(const llvm::Function &F) const;

  HeapAbstraction::RegionVec getModifiedRegions(const llvm::Function &F) const;

  HeapAbstraction::RegionVec getNewRegions(const llvm::Function &F) const;

  HeapAbstraction::RegionVec getOnlyReadRegions(const llvm::CallInst &I) const;

  HeapAbstraction::RegionVec getModifiedRegions(const llvm::CallInst &I) const;

  HeapAbstraction::RegionVec getNewRegions(const llvm::CallInst &I) const;

  std::vector<HeapAbstraction::RegionVec>
  getEquivClassRegions(const llvm::Function &F) const;

private:
  seadsa::GlobalAnalysis *m_dsa;
  std::unique_ptr<SetFactory> m_fac;
  const llvm::DataLayout &m_dl;
  /// map from Node to id
  llvm::DenseMap<const seadsa::Node *, HeapAbstraction::RegionId> m_node_ids;
  /// reverse map
  std::unordered_map<HeapAbstraction::RegionId, const seadsa::Node *>
      m_rev_node_ids;
  HeapAbstraction::RegionId m_max_id;
  SeaDsaHeapAbstractionParams m_params;

  // read or modified regions reachable from inputs or globals
  llvm::DenseMap<const llvm::Function *, HeapAbstraction::RegionVec>
      m_func_accessed;
  // modified regions reachable from inputs or globals
  llvm::DenseMap<const llvm::Function *, HeapAbstraction::RegionVec>
      m_func_mods;
  // regions reachable only from return parameters
  llvm::DenseMap<const llvm::Function *, HeapAbstraction::RegionVec>
      m_func_news;
  llvm::DenseMap<const llvm::CallInst *, HeapAbstraction::RegionVec>
      m_callsite_accessed;
  llvm::DenseMap<const llvm::CallInst *, HeapAbstraction::RegionVec>
      m_callsite_mods;
  llvm::DenseMap<const llvm::CallInst *, HeapAbstraction::RegionVec>
      m_callsite_news;

  // Group together all the regions originated from the same node
  llvm::DenseMap<const llvm::Function *,
                 std::vector<HeapAbstraction::RegionVec>>
      m_func_equiv_class_regions;
};

Region SeaDsaHeapAbstractionImpl::mkRegion(const Cell &c, RegionInfo ri) {
  auto id = getId(c);
  return Region(id, ri, getSingleton(id));
}

HeapAbstraction::RegionId SeaDsaHeapAbstractionImpl::getId(const Cell &c) {
  const Node *n = c.getNode();
  unsigned offset = c.getOffset();

  auto it = m_node_ids.find(n);
  if (it != m_node_ids.end()) {
    return it->second + offset;
  }

  HeapAbstraction::RegionId id = m_max_id;
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
void SeaDsaHeapAbstractionImpl::computeReadModNewNodes(
    const llvm::Function &f) {

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

  HeapAbstraction::RegionVec reads, mods, news;
  std::vector<HeapAbstraction::RegionVec> equivClasses;
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
          SeaDsaToRegion(c, m_dl,
			 m_params.disambiguate_unknown,
                         m_params.disambiguate_ptr_cast,
			 m_params.disambiguate_external);
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
  }   // end for each node
  CRAB_LOG(
      "heap-abs-regions", llvm::errs()
                              << "### HEAP_ABS: " << f.getName() << " ###\n";
      llvm::errs() << "Read regions reachable from arguments and globals {";
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
void SeaDsaHeapAbstractionImpl::computeEquivClasses(const llvm::Function &f) {
  if (!m_dsa || !(m_dsa->hasGraph(f))) {
    return;
  }

  Graph &G = m_dsa->getGraph(f);
  if (f.getName().startswith("shadow.mem")) {
    return;
  }

  // to store the equivalence classes
  std::vector<HeapAbstraction::RegionVec> equivClasses;

  // reachable from inputs and outputs
  seadsa_heap_abs_impl::NodeSet reach, retReach;
  seadsa_heap_abs_impl::argReachableNodes(f, G, reach, retReach);
  // and also reachable from locals
  for (auto &kv : G.scalars()) {
    if (const Node *n = kv.second->getNode()) {
      markReachableNodes(n, reach);
    }
  }

  for (const Node *n : reach) {
    bool isRetReach = retReach.count(n) > 0;
    if (!isRetReach && !n->isRead() && !n->isModified()) {
      continue;
    }
    HeapAbstraction::RegionVec nodeRgns;
    std::vector<unsigned> fields = extractFields(n);
    for (auto field : fields) {
      Cell c(const_cast<Node *>(n), field);
      RegionInfo r_info =
          SeaDsaToRegion(c, m_dl,
			 m_params.disambiguate_unknown,
                         m_params.disambiguate_ptr_cast,
			 m_params.disambiguate_external);
      nodeRgns.emplace_back(mkRegion(c, r_info));
    }
    equivClasses.emplace_back(std::move(nodeRgns));
  }

  m_func_equiv_class_regions[&f] = std::move(equivClasses);
}

// Compute and cache the set of read, mod and new nodes of a
// callsite such that mod nodes are a subset of the read nodes and
// the new nodes are disjoint from mod nodes.
void SeaDsaHeapAbstractionImpl::computeReadModNewNodesFromCallSite(
    const llvm::CallInst &I, callsite_map_t &accessed_map,
    callsite_map_t &mods_map, callsite_map_t &news_map) {

  auto checkConsistency = [this](const RegionInfo &r1, const RegionInfo &r2) {
    bool res = r1.hasCompatibleType(r2);
    if (m_params.precision_level == CrabBuilderPrecision::SINGLETON_MEM) {
      // seadsa does not guarantee that callsites agree with their
      // corresponding function parameters on dsa node flags.
      res &= (r1.isHeap() == r2.isHeap()); 
    }
    return res;
  };
  
  if (!m_dsa) {
    return;
  }

  /// ignore inline assembly
  if (I.isInlineAsm()) {
    return;
  }
  
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

  std::vector<std::pair<Region, bool>> reads, mods, news;
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
          SeaDsaToRegion(callerC, m_dl,
			 m_params.disambiguate_unknown,
                         m_params.disambiguate_ptr_cast,
			 m_params.disambiguate_external);
      RegionInfo calleeRI =
          SeaDsaToRegion(calleeC, m_dl,
			 m_params.disambiguate_unknown,
                         m_params.disambiguate_ptr_cast,
			 m_params.disambiguate_external);
      bool isConsistent = checkConsistency(callerRI, calleeRI);
      
      Region rgn(mkRegion(callerC, callerRI));      
      if (!isRetReach) {
        reads.push_back({rgn, isConsistent});
        if (n->isModified()) {
          mods.push_back({rgn, isConsistent});
        }
      } else {
        // if (n->isModified()) {
        news.push_back({rgn, isConsistent});
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
void SeaDsaHeapAbstractionImpl::initialize(const llvm::Module &M) {

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

  std::vector<const Function *> functions;
  for (auto const &F : M) {
    functions.push_back(&F);
  }
  std::sort(functions.begin(), functions.end());

  for (const Function *FF : functions) {
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
    HeapAbstraction::RegionVec &readsF = m_func_accessed[&F];
    HeapAbstraction::RegionVec &modsF = m_func_mods[&F];
    HeapAbstraction::RegionVec &newsF = m_func_news[&F];

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

      std::vector<std::pair<Region, bool>> &readsC = CSAccessed[CI];
      std::vector<std::pair<Region, bool>> &modsC = CSMods[CI];
      std::vector<std::pair<Region, bool>> &newsC = CSNews[CI];

      // Check that at the beginning caller and callee agree on the
      // number of memory regions, otherwwise there is nothing we can do.
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
      // which caller and callee disagree). Note that there are
      // legitimate cases where they can disagree.
      for (unsigned i = 0, e = readsC.size(); i < e; i++) {
        readsB[i] = readsB[i] && readsC[i].second;
	CRAB_LOG("heap-abs-regions",
		  if (!readsC[i].second) {
		    llvm::errs() << "Caller and callee disagree on some read region type\n"
		                 << "\tCallsite=" << *CI << "\n"
		                 << "\tRegion at caller=" << readsF[i] << "\n"
		                 << "\tRegion at callee=" << readsC[i].first << "\n";
		  });
		  
      }
      for (unsigned i = 0, e = modsC.size(); i < e; i++) {
        modsB[i] = modsB[i] && modsC[i].second;
	CRAB_LOG("heap-abs-regions",
		  if (!modsC[i].second) {
		    llvm::errs() << "Caller and callee disagree on some modified region type\n"
		                 << "\tCallsite=" << *CI << "\n"
		                 << "\tRegion at caller=" << modsF[i] << "\n"
		                 << "\tRegion at callee=" << modsC[i].first << "\n";
		  });
      }
      for (unsigned i = 0, e = newsC.size(); i < e; i++) {
        newsB[i] = newsB[i] && newsC[i].second;
	CRAB_LOG("heap-abs-regions",
		  if (!newsC[i].second) {
		    llvm::errs() << "Caller and callee disagree on some new region type\n"
		                 << "\tCallsite=" << *CI << "\n"
		                 << "\tRegion at caller=" << newsF[i] << "\n"
		                 << "\tRegion at callee=" << newsC[i].first << "\n";
		  });
      }
    }

    /// Second phase: cache final regions.
    HeapAbstraction::RegionVec readsF_out, modsF_out, newsF_out;
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
      HeapAbstraction::RegionVec readsC_out, modsC_out, newsC_out;
      std::vector<std::pair<Region, bool>> &readsC = CSAccessed[CI];
      std::vector<std::pair<Region, bool>> &modsC = CSMods[CI];
      std::vector<std::pair<Region, bool>> &newsC = CSNews[CI];
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

SeaDsaHeapAbstractionImpl::SeaDsaHeapAbstractionImpl(
    const llvm::Module &M, llvm::CallGraph &cg,
    const llvm::TargetLibraryInfoWrapperPass &tli,
    const seadsa::AllocWrapInfo &alloc_info,
    const seadsa::DsaLibFuncInfo &spec_graph_info,
    SeaDsaHeapAbstractionParams params)
    : m_dsa(nullptr), m_fac(new SetFactory()), m_dl(M.getDataLayout()),
      m_max_id(0), m_params(params) {

  // -- Run sea-dsa
  if (!m_params.is_context_sensitive) {
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

SeaDsaHeapAbstractionImpl::SeaDsaHeapAbstractionImpl(
    const llvm::Module &M, seadsa::GlobalAnalysis &dsa,
    SeaDsaHeapAbstractionParams params)
    : m_dsa(&dsa), m_fac(nullptr), m_dl(M.getDataLayout()), m_max_id(0),
      m_params(params) {
  initialize(M);
}

SeaDsaHeapAbstractionImpl::~SeaDsaHeapAbstractionImpl() {
  if (m_fac) {
    // if m_fac is not null we know that we own m_dsa
    delete m_dsa;
  }
}

// f is used to know in which Graph we should search for V
Region SeaDsaHeapAbstractionImpl::getRegion(const llvm::Function &fn,
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

  RegionInfo r_info = SeaDsaToRegion(c, m_dl,
				     m_params.disambiguate_unknown,
				     m_params.disambiguate_ptr_cast,
				     m_params.disambiguate_external);
  return mkRegion(c, r_info);
}

Region SeaDsaHeapAbstractionImpl::getRegion(const llvm::Function &fn,
                                            const llvm::Value &V,
                                            unsigned offset,
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
          SeaDsaToRegion(c, m_dl,
			 m_params.disambiguate_unknown,
                         m_params.disambiguate_ptr_cast,
			 m_params.disambiguate_external);
      return mkRegion(c, r_info);
    }
  }
  return Region();
}

const llvm::Value *SeaDsaHeapAbstractionImpl::getSingleton(
    HeapAbstraction::RegionId region) const {
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
static HeapAbstraction::RegionVec
stable_difference(HeapAbstraction::RegionVec &v1,
                  HeapAbstraction::RegionVec &v2) {
  // v1 and v2 can be modified because we pass copies

  std::sort(v2.begin(), v2.end());
  HeapAbstraction::RegionVec out;
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

template <class Map, class MapKey>
static HeapAbstraction::RegionVec lookup(const Map &m, const MapKey func) {
  auto it = m.find(func);
  if (it != m.end()) {
    return it->second;
  } else {
    return HeapAbstraction::RegionVec();
  }
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstractionImpl::getOnlyReadRegions(const llvm::Function &fn) const {
  HeapAbstraction::RegionVec v1 = lookup(m_func_accessed, &fn);
  HeapAbstraction::RegionVec v2 = lookup(m_func_mods, &fn);
  return stable_difference(v1, v2);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstractionImpl::getModifiedRegions(const llvm::Function &fn) const {
  return lookup(m_func_mods, &fn);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstractionImpl::getNewRegions(const llvm::Function &fn) const {
  return lookup(m_func_news, &fn);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstractionImpl::getOnlyReadRegions(const llvm::CallInst &I) const {
  HeapAbstraction::RegionVec v1 = lookup(m_callsite_accessed, &I);
  HeapAbstraction::RegionVec v2 = lookup(m_callsite_mods, &I);
  return stable_difference(v1, v2);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstractionImpl::getModifiedRegions(const llvm::CallInst &I) const {
  return lookup(m_callsite_mods, &I);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstractionImpl::getNewRegions(const llvm::CallInst &I) const {
  return lookup(m_callsite_news, &I);
}

std::vector<HeapAbstraction::RegionVec>
SeaDsaHeapAbstractionImpl::getEquivClassRegions(
    const llvm::Function &fn) const {
  auto it = m_func_equiv_class_regions.find(&fn);
  if (it != m_func_equiv_class_regions.end()) {
    return it->second;
  } else {
    return std::vector<HeapAbstraction::RegionVec>();
  }
}

SeaDsaHeapAbstraction::SeaDsaHeapAbstraction(
    const llvm::Module &M, llvm::CallGraph &cg,
    const llvm::TargetLibraryInfoWrapperPass &tli,
    const seadsa::AllocWrapInfo &awi, const seadsa::DsaLibFuncInfo &sgi,
    SeaDsaHeapAbstractionParams params) {
  m_impl = std::make_unique<SeaDsaHeapAbstractionImpl>(
      M, cg, tli, awi, sgi, params);
}

SeaDsaHeapAbstraction::SeaDsaHeapAbstraction(const llvm::Module &M,
                                             seadsa::GlobalAnalysis &dsa,
					     SeaDsaHeapAbstractionParams params) {
  m_impl = std::make_unique<SeaDsaHeapAbstractionImpl>(M, dsa, params);
}

SeaDsaHeapAbstraction::~SeaDsaHeapAbstraction() {}

seadsa::GlobalAnalysis *SeaDsaHeapAbstraction::getSeaDsa() {
  return m_impl->getSeaDsa();
}

const seadsa::GlobalAnalysis *SeaDsaHeapAbstraction::getSeaDsa() const {
  return m_impl->getSeaDsa();
}

Region SeaDsaHeapAbstraction::getRegion(const llvm::Function &F,
                                        const llvm::Value &V) {
  return m_impl->getRegion(F, V);
}

Region SeaDsaHeapAbstraction::getRegion(const llvm::Function &F,
                                        const llvm::Value &V, unsigned offset,
                                        const llvm::Type &accessedType) {
  return m_impl->getRegion(F, V, offset, accessedType);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::Function &F) const {
  return m_impl->getOnlyReadRegions(F);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getModifiedRegions(const llvm::Function &F) const {
  return m_impl->getModifiedRegions(F);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getNewRegions(const llvm::Function &F) const {
  return m_impl->getNewRegions(F);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getOnlyReadRegions(const llvm::CallInst &I) const {
  return m_impl->getOnlyReadRegions(I);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getModifiedRegions(const llvm::CallInst &I) const {
  return m_impl->getModifiedRegions(I);
}

HeapAbstraction::RegionVec
SeaDsaHeapAbstraction::getNewRegions(const llvm::CallInst &I) const {
  return m_impl->getNewRegions(I);
}

std::vector<HeapAbstraction::RegionVec>
SeaDsaHeapAbstraction::getEquivClassRegions(const llvm::Function &F) const {
  return m_impl->getEquivClassRegions(F);
}

} // namespace clam
