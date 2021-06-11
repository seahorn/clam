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
  
  llvm::DenseMap<
      std::pair<const llvm::MemoryLocation *, const llvm::MemoryLocation *>,
      llvm::AliasResult> m_alias_cache;
  llvm::DenseMap<const llvm::Instruction *, llvm::ConstantRange> m_range_inst_cache;
  llvm::DenseMap<std::pair<const llvm::BasicBlock *, const llvm::Value *>,
                 llvm::ConstantRange> m_range_value_cache;
  llvm::DenseMap<const llvm::Instruction *, TagVector> m_tag_inst_cache;
  llvm::DenseMap<std::pair<const llvm::BasicBlock *, const llvm::Value *>,
                 TagVector> m_tag_value_cache;
  
public:
  ClamQueryCache(CrabBuilderManager &man);
  llvm::AliasResult alias(const llvm::MemoryLocation &loc1,
                          const llvm::MemoryLocation &loc2,
                          llvm::AAQueryInfo &AAQI);
  llvm::ConstantRange range(const llvm::Instruction &I,
			    llvm::Optional<clam_abstract_domain> invAtEntry);
  llvm::ConstantRange range(const llvm::BasicBlock &B, const llvm::Value &V,
			    llvm::Optional<clam_abstract_domain> invAtEntry);
  llvm::Optional<TagVector> tags(const llvm::Instruction &I,
				 llvm::Optional<clam_abstract_domain> invAtEntry);
  llvm::Optional<TagVector> tags(const llvm::BasicBlock &B, const llvm::Value &V,
				 llvm::Optional<clam_abstract_domain> invAtEntry);
  
};
} // end namespace clam
