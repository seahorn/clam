#ifndef CRABLLVM_PASSES__HH_
#define CRABLLVM_PASSES__HH_

#include "crab_llvm/config.h"
#include "llvm/Pass.h"

namespace crab_llvm {

  // Preprocessor passes
  llvm::Pass* createLowerCstExprPass ();
  llvm::Pass* createLowerGvInitializersPass ();
  llvm::Pass* createLowerSelectPass ();
  llvm::Pass* createLowerUnsignedICmpPass ();
  llvm::Pass* createMarkInternalInlinePass ();
  llvm::Pass* createRemoveUnreachableBlocksPass ();
  llvm::Pass* createSimplifyAssumePass ();
  llvm::Pass* createDevirtualizeFunctionsPass ();
  llvm::Pass* createExternalizeAddressTakenFunctionsPass ();
  llvm::Pass* createPromoteMallocPass ();
}

#endif /* CRABLLVM_PASSES__HH_ */
