#pragma once

#include "clam/config.h"
#include "llvm/Pass.h"

namespace clam {
  // Preprocessor passes
  llvm::Pass *createLowerCstExprPass();
  llvm::Pass *createLowerSelectPass();
  llvm::Pass *createLowerUnsignedICmpPass();
  llvm::Pass *createMarkInternalInlinePass();
  llvm::Pass *createRemoveUnreachableBlocksPass();
  llvm::Pass *createSimplifyAssumePass();
  llvm::Pass *createDevirtualizeFunctionsPass();
  llvm::Pass *createExternalizeAddressTakenFunctionsPass();
  llvm::Pass *createPromoteMallocPass();
  llvm::Pass *createPromoteAssumePass();
  llvm::Pass *createRenameNondetPass();
  llvm::Pass *createNondetInitPass();
  llvm::Pass *createDeadNondetElimPass();
} // namespace clam


#ifdef HAVE_LLVM_SEAHORN
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm_seahorn/Transforms/InstCombine/SeaInstCombine.h"

llvm::FunctionPass *
createSeaInstructionCombiningPass(bool ExpensiveCombines = true);

namespace clam {
inline llvm::FunctionPass *createInstCombine(bool ExpensiveCombines = true) {
  return createSeaInstructionCombiningPass(ExpensiveCombines);
}
} // namespace clam
#else
namespace clam {
inline llvm::FunctionPass *createInstCombine(bool ExpensiveCombines = true) {
  return llvm::createInstructionCombiningPass(ExpensiveCombines);
}
} // namespace clam
#endif

