#pragma once

#include "clam/config.h"
#include "llvm/Pass.h"

namespace clam {

  // Preprocessor passes
  llvm::Pass* createLowerCstExprPass ();
  llvm::Pass* createLowerSelectPass ();
  llvm::Pass* createLowerUnsignedICmpPass ();
// LLVM 8.0 has already this pass
// llvm::Pass* createScalarizerPass();
  llvm::Pass* createMarkInternalInlinePass ();
  llvm::Pass* createRemoveUnreachableBlocksPass ();
  llvm::Pass* createSimplifyAssumePass ();
  llvm::Pass* createDevirtualizeFunctionsPass();
  llvm::Pass* createExternalizeAddressTakenFunctionsPass ();
  llvm::Pass* createPromoteMallocPass ();
  llvm::Pass* createPromoteAssumePass ();
} // namespace clam

