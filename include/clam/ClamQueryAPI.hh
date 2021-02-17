#pragma once

#include "llvm/Analysis/AliasAnalysis.h"
#include <limits>

namespace llvm {
  class BasicBlock;
  class Instruction;
  class Value;
}

namespace clam {
  
class ClamQueryAPI {
public:
  // TODO: use llvm::ConstantRange
  using Range = std::pair<int64_t, int64_t>;
  virtual llvm::AliasResult alias(const llvm::MemoryLocation &Loc1,
				  const llvm::MemoryLocation &Loc2,
				  llvm::AAQueryInfo &AAQI) = 0;
  
  virtual Range range(const llvm::Instruction &I) = 0;
  
  virtual Range range(const llvm::BasicBlock &B, const llvm::Value &V) = 0;
};
} // end namespace clam
