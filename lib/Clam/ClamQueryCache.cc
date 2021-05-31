#pragma once

#include "llvm/ADT/Optional.h"
#include "ClamQueryCache.hh"
#include "clam/CfgBuilder.hh"
#include "crab/analysis/fwd_analyzer.hpp"

namespace clam {
using namespace llvm;

static ClamQueryAPI::Range getFullRange() {
  return std::make_pair(std::numeric_limits<int64_t>::min(),
                        std::numeric_limits<int64_t>::max());
}

static bool match(const Instruction &I, statement_t &s) {
  if (s.get_live().num_defs() == 1) {
    var_t v = *(s.get_live().defs_begin());
    if (v.name().get()) {
      if (auto II = dyn_cast<const Instruction>(*(v.name().get()))) {
        return II == &I;
      }
    }
  }
  return false;
}

ClamQueryCache::ClamQueryCache(CrabBuilderManager &man)
    : m_crab_builder_man(man) {}

AliasResult ClamQueryCache::alias(const MemoryLocation &loc1,
                                  const MemoryLocation &loc2, AAQueryInfo &) {
  // TODO: implementation
  return AliasResult::MayAlias;
}

ClamQueryAPI::Range
ClamQueryCache::range(const llvm::Instruction &I,
                      Optional<clam_abstract_domain> invAtEntry) {
  auto it = m_range_inst_cache.find(&I);
  if (it != m_range_inst_cache.end()) {
    return it->second;
  }

  if (invAtEntry.hasValue()) {
    const BasicBlock &BB = *(I.getParent());
    const Function &fParent = *(BB.getParent());
    if (m_crab_builder_man.hasCfg(fParent)) {
      auto &crabCfg = m_crab_builder_man.getCfg(fParent);
      auto crabCfgBuilder = m_crab_builder_man.getCfgBuilder(fParent);
      auto &crabBB = crabCfg.get_node(crabCfgBuilder->getCrabBasicBlock(&BB));
      // Forward propagation through the basic block but ignoring
      // callsites. This might be imprecise if the analysis was
      // inter-procedural because we cannot reconstruct all the context
      // that the inter-procedural analysis had during the analysis.
      using abs_tr_t =
        crab::analyzer::intra_abs_transformer<basic_block_t,
                                              clam_abstract_domain>;
      abs_tr_t vis(invAtEntry.getValue());
      for (auto &crabStmt : crabBB) {
	crabStmt.accept(&vis); // propagate the invariant one statement forward
	const clam_abstract_domain &nextInv = vis.get_abs_value();
	if (match(I, crabStmt)) {
	  llvm::Optional<var_t> crabVar = crabCfgBuilder->getCrabVariable(I);
	  if (crabVar.hasValue()) {
	    clam_abstract_domain tmp(nextInv);
	    auto crabInterval = tmp[crabVar.getValue()];
	    if (crabInterval.lb().is_finite() && crabInterval.ub().is_finite()) {
	      auto min = *(crabInterval.lb().number());
	      auto max = *(crabInterval.ub().number());
	      if (min.fits_int64() && max.fits_int64()) {
		auto interval = std::make_pair((int64_t)min, (int64_t)max);
		m_range_inst_cache[&I] = interval;
		return interval;
	      }
	    }
	  }
	}
      }
    }
  }
  return getFullRange();
}

ClamQueryAPI::Range
ClamQueryCache::range(const llvm::BasicBlock &BB, const llvm::Value &V,
                      Optional<clam_abstract_domain> invAtEntry) {
  auto it = m_range_value_cache.find({&BB, &V});
  if (it != m_range_value_cache.end()) {
    return it->second;
  }

  if (invAtEntry.hasValue()) {
    const Function &F = *(BB.getParent());
    auto crabCfgBuilder = m_crab_builder_man.getCfgBuilder(F);    
    llvm::Optional<var_t> crabVar = crabCfgBuilder->getCrabVariable(V);
    if (crabVar.hasValue()) {
      auto crabInterval = invAtEntry.getValue()[crabVar.getValue()];
      if (crabInterval.lb().is_finite() && crabInterval.ub().is_finite()) {
        auto min = *(crabInterval.lb().number());
        auto max = *(crabInterval.ub().number());
        if (min.fits_int64() && max.fits_int64()) {
          auto interval = std::make_pair((int64_t)min, (int64_t)max);
          auto k = std::make_pair(&BB, &V);
          m_range_value_cache[k] = interval;
          return interval;
        }
      }
    }
  }
  return getFullRange();
}

Optional<ClamQueryAPI::TagVector>
ClamQueryCache::tags(const llvm::Instruction &I,
		     Optional<clam_abstract_domain> invAtEntry) {

  auto it = m_tag_inst_cache.find(&I);
  if (it != m_tag_inst_cache.end()) {
    return it->second;
  }
  
  if (invAtEntry.hasValue()) {
    const BasicBlock &BB = *(I.getParent());
    const Function &fParent = *(BB.getParent());
    if (m_crab_builder_man.hasCfg(fParent)) {
      auto &crabCfg = m_crab_builder_man.getCfg(fParent);
      auto crabCfgBuilder = m_crab_builder_man.getCfgBuilder(fParent);
      auto &crabBB = crabCfg.get_node(crabCfgBuilder->getCrabBasicBlock(&BB));
      // Forward propagation through the basic block but ignoring
      // callsites. This might be imprecise if the analysis was
      // inter-procedural because we cannot reconstruct all the context
      // that the inter-procedural analysis had during the analysis.
      using abs_tr_t =
        crab::analyzer::intra_abs_transformer<basic_block_t,
                                              clam_abstract_domain>;
      abs_tr_t vis(invAtEntry.getValue());
      for (auto &crabStmt : crabBB) {
	crabStmt.accept(&vis); // propagate the invariant one statement forward
	const clam_abstract_domain &nextInv = vis.get_abs_value();
	if (match(I, crabStmt)) {
	  clam_abstract_domain tmp(nextInv);
	  llvm::Optional<var_t> crabRefVar = crabCfgBuilder->getCrabVariable(I);
	  llvm::Optional<var_t> crabRgnVar = crabCfgBuilder->getCrabRegionVariable(fParent, I);	
	  if (crabRefVar.hasValue() && crabRgnVar.hasValue()) {
	    std::vector<uint64_t> tags;
	    bool known = invAtEntry.getValue().get_tags(crabRgnVar.getValue(),
							crabRefVar.getValue(), tags);
	    if (known) {
	      m_tag_inst_cache[&I] = tags;
	      return tags;
	    } else {
	      return None;
	    }
	  }
	}
      }
    }
  }
  return None;
}

Optional<ClamQueryAPI::TagVector>
ClamQueryCache::tags(const llvm::BasicBlock &BB, const llvm::Value &V,
		     Optional<clam_abstract_domain> invAtEntry) {

  auto it = m_tag_value_cache.find({&BB, &V});
  if (it != m_tag_value_cache.end()) {
    return it->second;
  }
  
  if (invAtEntry.hasValue()) {
    const Function &F = *(BB.getParent());
    CfgBuilderPtr cfgBuilder =  m_crab_builder_man.getCfgBuilder(F);
    llvm::Optional<var_t> crabRgnVar = cfgBuilder->getCrabRegionVariable(F, V);
    llvm::Optional<var_t> crabRefVar = cfgBuilder->getCrabVariable(V);
    if (crabRgnVar.hasValue() && crabRefVar.hasValue()) {
      std::vector<uint64_t> tags;
      bool known = invAtEntry.getValue().get_tags(crabRgnVar.getValue(),
						  crabRefVar.getValue(), tags);
      if (known) {
	auto k = std::make_pair(&BB, &V);
	m_tag_value_cache[k] = tags;
	return tags;
      } else {
	return None;
      }
    }
  }
  return None;
}

} // end namespace clam
