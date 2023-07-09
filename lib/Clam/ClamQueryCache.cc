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
  
// TOIMPROVE: only find the instruction if s has a definition   
static const Instruction* getInstruction(statement_t &s) {
  if (s.get_live().num_defs() == 1) {
    var_t v = *(s.get_live().defs_begin());
    if (v.name().get()) {
      if (auto I = dyn_cast<const Instruction>(*(v.name().get()))) {
	return I;
      }
    }
  }
  return nullptr;
}

ClamQueryCache::ClamQueryCache(CrabBuilderManager &man)
    : m_crab_builder_man(man) {}

AliasResult ClamQueryCache::alias(const MemoryLocation &loc1,
                                  const MemoryLocation &loc2, AAQueryInfo &) const {
  // TODO: implementation
  return AliasResult::MayAlias;
}

const ConstantRange* getArgRange(const DenseMap<unsigned, ConstantRange>& argMap, unsigned i) {
  auto it = argMap.find(i);
  if (it != argMap.end()) {
    return &it->second;
  } else {
    return nullptr;
  }
}
  
const ConstantRange*
ClamQueryCache::getCachedRange(const Instruction &Inst, unsigned i) const {
  auto it = m_range_inst_cache.find(&Inst);
  if (it != m_range_inst_cache.end()) {
    auto &argMap = it->second;
    return getArgRange(argMap, i);
  } else {
    return nullptr;
  }
}

static ConstantRange getRange(const var_t &v, 
			      const clam_abstract_domain &absVal, bool is_bottom, unsigned bitwidth) {
  if (is_bottom) {
    return getEmptyRange(bitwidth);
  } else {
    auto crabInterval = absVal.at(v);
    if (crabInterval.lb().is_finite() && crabInterval.ub().is_finite()) {
      auto min = *(crabInterval.lb().number());
      auto max = *(crabInterval.ub().number());
      if (min.fits_int64() && max.fits_int64()) {
	return getConstantRange((int64_t)min, (int64_t)max, bitwidth);
      }
    }
    return getFullRange(bitwidth);  
  }
  
}
  
// Populate m_range_inst_cache for all instructions located at I's parent block.
void ClamQueryCache::populateInstCache(const BasicBlock &BB, clam_abstract_domain invAtEntry) const {
  const Function &fParent = *(BB.getParent());
  assert(m_crab_builder_man.hasCfg(fParent));
  auto &crabCfg = m_crab_builder_man.getCfg(fParent);
  auto crabCfgBuilder = m_crab_builder_man.getCfgBuilder(fParent);
  assert(crabCfgBuilder);
  auto &crabBB = crabCfg.get_node(crabCfgBuilder->getCrabBasicBlock(&BB));
  // Forward propagation through the basic block but ignoring
  // callsites. This might be imprecise if the analysis was
  // inter-procedural because we cannot reconstruct all the context
  // that the inter-procedural analysis had during the analysis.
  using abs_tr_t = crab::analyzer::intra_abs_transformer<basic_block_t,
							 clam_abstract_domain>;
  abs_tr_t vis(invAtEntry);
  for (auto &crabStmt : crabBB) {
    crabStmt.accept(&vis); // propagate the invariant one statement forward
    const Instruction *I = getInstruction(crabStmt);
    if (I == nullptr) {
      continue;
    }
    unsigned bitwidth = getBitWidth(*I);    
    const clam_abstract_domain &absVal = vis.get_abs_value();
    bool is_bottom = absVal.is_bottom();
    // 0 for LHS and 1,...,N for the rest of operands in the RHS
    DenseMap<unsigned, ConstantRange> argMap;

    Optional<var_t> LHSVar = crabCfgBuilder->getCrabVariable(*I);
    if (LHSVar.hasValue()) {
      argMap.insert({0, getRange(LHSVar.getValue(), absVal, is_bottom, bitwidth)});
    }
    for (unsigned i=0, e=I->getNumOperands();i!=e;++i) {
      Value *argOp = I->getOperand(i);
      Optional<var_t> RHSVar = crabCfgBuilder->getCrabVariable(*argOp);
      if (RHSVar.hasValue()) {
	argMap.insert({i+1, getRange(RHSVar.getValue(), absVal, is_bottom, bitwidth)});
      } else {
	argMap.insert({i+1, getFullRange(bitwidth)});
      }
    } // end for
    m_range_inst_cache.insert(std::make_pair(I, std::move(argMap)));
  } // end foor
}
  
ConstantRange
ClamQueryCache::range(const Instruction &Inst, unsigned i, 
                      Optional<clam_abstract_domain> invAtEntry) const {

  if (i >= Inst.getNumOperands()) {
    CLAM_ERROR("range method is accessing out of bounds!");
  }

  const ConstantRange* cachedInterval = getCachedRange(Inst, i);
  if (cachedInterval != nullptr) {
    return *cachedInterval;
  }

  if (invAtEntry.hasValue()) {
    const BasicBlock &BB = *(Inst.getParent());
    if (m_crab_builder_man.hasCfg(*(BB.getParent()))) {
      populateInstCache(BB, invAtEntry.getValue());
    }
    const ConstantRange* interval = getCachedRange(Inst, i);
    if (interval != nullptr) {
      return *interval;
    }          
  }

  return getFullRange(getBitWidth(Inst));  
}
  

ConstantRange ClamQueryCache::range(const BasicBlock &BB, const Value &V,
				    Optional<clam_abstract_domain> invAtEntry) const {
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
	auto crabInterval = invAtEntry.getValue().at(crabVar.getValue());
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

// Populate m_tag_inst_cache for all instructions located at I's parent block.
void ClamQueryCache::populateTagCache(const BasicBlock &BB, clam_abstract_domain invAtEntry) const {
  const Function &fParent = *(BB.getParent());
  assert(m_crab_builder_man.hasCfg(fParent));
  auto &crabCfg = m_crab_builder_man.getCfg(fParent);
  auto crabCfgBuilder = m_crab_builder_man.getCfgBuilder(fParent);
  assert(crabCfgBuilder);
  auto &crabBB = crabCfg.get_node(crabCfgBuilder->getCrabBasicBlock(&BB));
  // Forward propagation through the basic block but ignoring
  // callsites. This might be imprecise if the analysis was
  // inter-procedural because we cannot reconstruct all the context
  // that the inter-procedural analysis had during the analysis.
  using abs_tr_t = crab::analyzer::intra_abs_transformer<basic_block_t,
							 clam_abstract_domain>;
  abs_tr_t vis(invAtEntry);
  for (auto &crabStmt : crabBB) {
    crabStmt.accept(&vis); // propagate the invariant one statement forward
    const Instruction *I = getInstruction(crabStmt);
    if (I == nullptr) {
      continue;
    }
    clam_abstract_domain &absVal = vis.get_abs_value();
    Optional<var_t> LHSRefVar = crabCfgBuilder->getCrabVariable(*I);
    Optional<var_t> LHSRgnVar = crabCfgBuilder->getCrabRegionVariable(fParent, *I);
    if (LHSRefVar.hasValue() && LHSRgnVar.hasValue()) {
      std::vector<uint64_t> tags;
      bool known = absVal.get_tags(LHSRgnVar.getValue(), LHSRefVar.getValue(), tags);
      if (known) {
	m_tag_inst_cache.insert({I, std::move(tags)});
      }
    }
  }
}

  
Optional<ClamQueryAPI::TagVector>
ClamQueryCache::tags(const Instruction &I,
		     Optional<clam_abstract_domain> invAtEntry) const {

  auto it = m_tag_inst_cache.find(&I);
  if (it != m_tag_inst_cache.end()) {
    return it->second;
  }
  
  if (invAtEntry.hasValue()) {
    const BasicBlock &BB = *(I.getParent());
    if (m_crab_builder_man.hasCfg(*(BB.getParent()))) {
      populateTagCache(BB, invAtEntry.getValue());
    }
    auto it = m_tag_inst_cache.find(&I);
    if (it != m_tag_inst_cache.end()) {
      return it->second;
    }
  }
  
  return None;
}

Optional<ClamQueryAPI::TagVector>
ClamQueryCache::tags(const BasicBlock &BB, const Value &V,
		     Optional<clam_abstract_domain> invAtEntry) const {

  auto it = m_tag_value_cache.find({&BB, &V});
  if (it != m_tag_value_cache.end()) {
    return it->second;
  }
  
  if (invAtEntry.hasValue()) {
    const Function &F = *(BB.getParent());
    if (CfgBuilder *cfgBuilder =  m_crab_builder_man.getCfgBuilder(F)) {
      Optional<var_t> LHSRgnVar = cfgBuilder->getCrabRegionVariable(F, V);
      Optional<var_t> LHSRefVar = cfgBuilder->getCrabVariable(V);
      if (LHSRgnVar.hasValue() && LHSRefVar.hasValue()) {
	ClamQueryAPI::TagVector tags;
	bool known = invAtEntry.getValue().get_tags(LHSRgnVar.getValue(),
						    LHSRefVar.getValue(), tags);
	if (known) {
	  m_tag_value_cache.insert({{&BB, &V}, tags});
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
