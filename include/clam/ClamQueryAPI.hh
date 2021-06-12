#pragma once

#include "llvm/ADT/Optional.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/IR/ConstantRange.h"
#include <limits>
#include <vector>

namespace llvm {
  class BasicBlock;
  class Instruction;
  class Value;
}

namespace clam {
  
class ClamQueryAPI {
public:  
  /* a tag is just an unsigned integer */
  using TagVector = std::vector<uint64_t>;
  
  virtual llvm::AliasResult alias(const llvm::MemoryLocation &Loc1,
				  const llvm::MemoryLocation &Loc2,
				  llvm::AAQueryInfo &AAQI) = 0;
  
  virtual llvm::ConstantRange range(const llvm::Instruction &I) = 0;
  
  virtual llvm::ConstantRange range(const llvm::BasicBlock &B, const llvm::Value &V) = 0;

  virtual llvm::Optional<TagVector> tags(const llvm::Instruction &I) = 0;
  
  virtual llvm::Optional<TagVector> tags(const llvm::BasicBlock &B,
					 const llvm::Value &V) = 0;
  
};
} // end namespace clam
