#pragma once

#include <clam/Clam.hh>
#include <clam/ClamQueryAPI.hh>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/Optional.h>
#include <llvm/IR/ConstantRange.h>

namespace llvm {
class Instruction;
class BasicBlock;
class Value;
} // end namespace llvm

namespace clam {
class CrabBuilderManager;

class ClamQueryCache {
  using TagVector = typename ClamQueryAPI::TagVector;
  
  CrabBuilderManager &m_crab_builder_man;
  
  mutable llvm::DenseMap<const llvm::Instruction *, llvm::DenseMap<unsigned, llvm::ConstantRange>> m_range_inst_cache;
  mutable llvm::DenseMap<std::pair<const llvm::BasicBlock *, const llvm::Value *>,
                 llvm::ConstantRange> m_range_value_cache;
  mutable llvm::DenseMap<const llvm::Instruction *, TagVector> m_tag_inst_cache;
  mutable llvm::DenseMap<std::pair<const llvm::BasicBlock *, const llvm::Value *>,
                 TagVector> m_tag_value_cache;

  // return nullptr if not found in the instruction cache
  const llvm::ConstantRange* getCachedRange(const llvm::Instruction &Inst, unsigned i) const;
  void populateInstCache(const llvm::BasicBlock &BB, clam_abstract_domain invAtEntry) const;
  void populateTagCache(const llvm::BasicBlock &BB, clam_abstract_domain invAtEntry) const;  
  
public:
  ClamQueryCache(CrabBuilderManager &man);
  // Not implemented: return always MayAlias
  llvm::AliasResult alias(const llvm::MemoryLocation &loc1,
                          const llvm::MemoryLocation &loc2,
                          llvm::AAQueryInfo &AAQI) const;
  llvm::ConstantRange range(const llvm::Instruction &I,
			    llvm::Optional<clam_abstract_domain> invAtEntry) const {
    return range(I, 0, invAtEntry);
  }
  llvm::ConstantRange range(const llvm::Instruction &Inst, unsigned i,
			    llvm::Optional<clam_abstract_domain> invAtEntry) const;
  llvm::ConstantRange range(const llvm::BasicBlock &B, const llvm::Value &V,
			    llvm::Optional<clam_abstract_domain> invAtEntry) const ;
  llvm::Optional<TagVector> tags(const llvm::Instruction &I,
				 llvm::Optional<clam_abstract_domain> invAtEntry) const;
  llvm::Optional<TagVector> tags(const llvm::BasicBlock &B, const llvm::Value &V,
				 llvm::Optional<clam_abstract_domain> invAtEntry) const;
  
};
} // end namespace clam
