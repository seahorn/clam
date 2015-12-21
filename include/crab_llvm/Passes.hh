#ifndef CRABLLVM_PASSES__HH_
#define CRABLLVM_PASSES__HH_

#include "crab_llvm/config.h"
#include "llvm/Pass.h"

namespace crab_llvm
{
  llvm::Pass* createLowerCstExprPass ();
  llvm::Pass* createLowerGvInitializersPass ();
  llvm::Pass* createLowerSelectPass ();
  llvm::Pass* createMarkInternalInlinePass ();
  llvm::Pass* createRemoveUnreachableBlocksPass ();
  llvm::Pass* createSimplifyAssumePass ();
  llvm::Pass* createDevirtualizeFunctionsPass ();
}

#endif /* CRABLLVM_PASSES__HH_ */
