#pragma once

#include "clam/config.h"
#include "llvm/Pass.h"

namespace clam {

  // Preprocessor passes
  llvm::Pass* createLowerCstExprPass ();
  llvm::Pass* createLowerGvInitializersPass ();
  llvm::Pass* createLowerSelectPass ();
  llvm::Pass* createLowerUnsignedICmpPass ();
  llvm::Pass* createScalarizerPass();
  llvm::Pass* createMarkInternalInlinePass ();
  llvm::Pass* createRemoveUnreachableBlocksPass ();
  llvm::Pass* createSimplifyAssumePass ();
  llvm::Pass* createDevirtualizeFunctionsPass();
  llvm::Pass* createExternalizeAddressTakenFunctionsPass ();
  llvm::Pass* createPromoteMallocPass ();
  llvm::Pass* createPromoteAssumePass ();
}

