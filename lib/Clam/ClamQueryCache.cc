#pragma once

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

  if (!invAtEntry.hasValue()) {
    return getFullRange();
  }

  const BasicBlock &BB = *(I.getParent());
  const Function &fParent = *(BB.getParent());
  if (m_crab_builder_man.hasCfg(fParent)) {
    auto &crab_cfg = m_crab_builder_man.getCfg(fParent);
    auto crab_cfg_builder = m_crab_builder_man.getCfgBuilder(fParent);
    auto &crab_bb = crab_cfg.get_node(crab_cfg_builder->getCrabBasicBlock(&BB));
    // Forward propagation through the basic block but ignoring
    // callsites. This might be imprecise if the analysis was
    // inter-procedural because we cannot reconstruct all the context
    // that the inter-procedural analysis had during the analysis.
    using abs_tr_t =
        crab::analyzer::intra_abs_transformer<basic_block_t,
                                              clam_abstract_domain>;
    abs_tr_t vis(invAtEntry.getValue());
    for (auto &crab_stmt : crab_bb) {
      crab_stmt.accept(&vis); // propagate the invariant one statement forward
      const clam_abstract_domain &next_inv = vis.get_abs_value();
      if (match(I, crab_stmt)) {
        clam_abstract_domain tmp(next_inv);
        var_t crab_var = *(crab_stmt.get_live().defs_begin());
        auto crab_interval = tmp[crab_var];
        if (crab_interval.lb().is_finite() && crab_interval.ub().is_finite()) {
          auto min = *(crab_interval.lb().number());
          auto max = *(crab_interval.ub().number());
          if (min.fits_int64() && max.fits_int64()) {
            auto interval = std::make_pair((int64_t)min, (int64_t)max);
            m_range_inst_cache[&I] = interval;
            return interval;
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
    llvm::Optional<var_t> crab_var =
        m_crab_builder_man.getCfgBuilder(*(BB.getParent()))
            ->getCrabVariable(&V);
    if (crab_var.hasValue()) {
      auto crab_interval = invAtEntry.getValue()[crab_var.getValue()];
      if (crab_interval.lb().is_finite() && crab_interval.ub().is_finite()) {
        auto min = *(crab_interval.lb().number());
        auto max = *(crab_interval.ub().number());
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

} // end namespace clam
