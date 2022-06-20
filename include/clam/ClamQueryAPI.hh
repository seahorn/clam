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

  virtual ~ClamQueryAPI() {}
  
  virtual llvm::AliasResult alias(const llvm::MemoryLocation &Loc1,
				  const llvm::MemoryLocation &Loc2,
				  llvm::AAQueryInfo &AAQI) = 0;

  
  // Return the range for the left-hand side of I before the execution
  // of I. The type of I should be either integer or pointer,
  // otherwise an error will be raised. If the returned range is the
  // empty constant range then the instruction is unreachable.
  virtual llvm::ConstantRange range(const llvm::Instruction &I) = 0;

  // Return the range for V that holds at the entry of B.  The type of
  // V should be either integer or pointer, otherwise an error will be
  // raised. If the returned range is the empty constant range then
  // the basic block is unreachable.
  virtual llvm::ConstantRange range(const llvm::BasicBlock &B, const llvm::Value &V) = 0;

  // Return the tags associated to the left-hand side of I before the
  // execution of I.  If the type of I is not a pointer then it
  // returns None.
  virtual llvm::Optional<TagVector> tags(const llvm::Instruction &I) = 0;

  // Return the tags associated to V that hold at the entry of B. If
  // the type of V is not a pointer than it returns None.
  virtual llvm::Optional<TagVector> tags(const llvm::BasicBlock &B,
					 const llvm::Value &V) = 0;
  
};
} // end namespace clam
