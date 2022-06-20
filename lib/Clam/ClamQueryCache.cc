#include "llvm/ADT/Optional.h"
#include "ClamQueryCache.hh"
#include "clam/CfgBuilder.hh"
#include "clam/Support/Debug.hh"
#include "crab/analysis/fwd_analyzer.hpp"

namespace clam {
using namespace llvm;


static unsigned getBitWidth(const DataLayout &DL, const Type *Ty) {
  if (Ty->isIntegerTy()) {
    const IntegerType *ITy = cast<IntegerType>(Ty);
    return ITy->getBitWidth();
  } else if (Ty->isPointerTy()) {
    return DL.getPointerTypeSizeInBits(const_cast<Type*>(Ty));
  } else {
    CLAM_ERROR("getConstantRange should called only on integers or pointers");
  }
}

static unsigned getBitWidth(const Instruction &I) {
  const DataLayout &DL = I.getParent()->getParent()->getParent()->getDataLayout();
  return getBitWidth(DL, I.getType());
}

static unsigned getBitWidth(const BasicBlock &B, const Value& V) {
  const DataLayout &DL = B.getParent()->getParent()->getDataLayout();
  return getBitWidth(DL, V.getType());
}

static ConstantRange getFullRange(unsigned bitwidth) {
  return ConstantRange::getFull(bitwidth);
}

static ConstantRange getEmptyRange(unsigned bitwidth) {
  return ConstantRange::getEmpty(bitwidth);
}

static ConstantRange getConstantRange(int64_t lower, int64_t upper, unsigned bitwidth) {
  const bool isSigned = true;
  if (lower == upper) {
    return ConstantRange(APInt(bitwidth, lower, isSigned));
  } else {
    return ConstantRange(APInt(bitwidth, lower, isSigned),
			 APInt(bitwidth, upper+1, isSigned));
  }
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

ConstantRange
ClamQueryCache::range(const Instruction &I,
                      Optional<clam_abstract_domain> invAtEntry) {
  auto it = m_range_inst_cache.find(&I);
  if (it != m_range_inst_cache.end()) {
    return it->second;
  }

  unsigned bitwidth = getBitWidth(I);
  if (invAtEntry.hasValue()) {
    const BasicBlock &BB = *(I.getParent());
    const Function &fParent = *(BB.getParent());
    if (m_crab_builder_man.hasCfg(fParent)) {
      auto &crabCfg = m_crab_builder_man.getCfg(fParent);
      auto crabCfgBuilder = m_crab_builder_man.getCfgBuilder(fParent);
      assert(crabCfgBuilder);
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
	  if (nextInv.is_bottom()) {
	    ConstantRange interval = getEmptyRange(bitwidth);
	    m_range_inst_cache.insert(std::make_pair(&I, interval));
	    return interval;
	  }
	  
	  Optional<var_t> crabVar = crabCfgBuilder->getCrabVariable(I);
	  if (crabVar.hasValue()) {
	    clam_abstract_domain tmp(nextInv);
	    auto crabInterval = tmp[crabVar.getValue()];
	    if (crabInterval.lb().is_finite() && crabInterval.ub().is_finite()) {
	      auto min = *(crabInterval.lb().number());
	      auto max = *(crabInterval.ub().number());
	      if (min.fits_int64() && max.fits_int64()) {
		ConstantRange interval = getConstantRange((int64_t)min, (int64_t)max, bitwidth);
		m_range_inst_cache.insert(std::make_pair(&I, interval));
		return interval;
	      }
	    }
	  }
	}
      }
    }
  }
  return getFullRange(bitwidth);
}

ConstantRange
ClamQueryCache::range(const BasicBlock &BB, const Value &V,
                      Optional<clam_abstract_domain> invAtEntry) {
  auto it = m_range_value_cache.find({&BB, &V});
  if (it != m_range_value_cache.end()) {
    return it->second;
  }

  unsigned bitwidth = getBitWidth(BB, V);  
  if (invAtEntry.hasValue()) {
    if (invAtEntry.getValue().is_bottom()) {
      // we don't cache it
      return getEmptyRange(bitwidth);
    }
    const Function &F = *(BB.getParent());
    if (auto crabCfgBuilder = m_crab_builder_man.getCfgBuilder(F)) {
      Optional<var_t> crabVar = crabCfgBuilder->getCrabVariable(V);
      if (crabVar.hasValue()) {
	auto crabInterval = invAtEntry.getValue()[crabVar.getValue()];
	if (crabInterval.lb().is_finite() && crabInterval.ub().is_finite()) {
	  auto min = *(crabInterval.lb().number());
	  auto max = *(crabInterval.ub().number());
	  if (min.fits_int64() && max.fits_int64()) {
	    ConstantRange interval = getConstantRange((int64_t)min, (int64_t)max, bitwidth);
	    m_range_value_cache.insert(std::make_pair(std::make_pair(&BB, &V),
						      interval));
	    return interval;
	  }
	}
      }
    }
  }
  return getFullRange(bitwidth);
}

Optional<ClamQueryAPI::TagVector>
ClamQueryCache::tags(const Instruction &I,
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
      assert(crabCfgBuilder);
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
	  Optional<var_t> crabRefVar = crabCfgBuilder->getCrabVariable(I);
	  Optional<var_t> crabRgnVar = crabCfgBuilder->getCrabRegionVariable(fParent, I);	
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
ClamQueryCache::tags(const BasicBlock &BB, const Value &V,
		     Optional<clam_abstract_domain> invAtEntry) {

  auto it = m_tag_value_cache.find({&BB, &V});
  if (it != m_tag_value_cache.end()) {
    return it->second;
  }
  
  if (invAtEntry.hasValue()) {
    const Function &F = *(BB.getParent());
    if (CfgBuilder *cfgBuilder =  m_crab_builder_man.getCfgBuilder(F)) {
      Optional<var_t> crabRgnVar = cfgBuilder->getCrabRegionVariable(F, V);
      Optional<var_t> crabRefVar = cfgBuilder->getCrabVariable(V);
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
  }
  return None;
}

} // end namespace clam
