#pragma once

#include "clam/config.h"
#include "llvm/Pass.h"

namespace clam {
class ClamGlobalAnalysis;
} // end namespace

namespace clam {
// Preprocessor passes
llvm::Pass *createInsertEntryPointPass();  
llvm::Pass *createLowerCstExprPass();
llvm::Pass *createLowerSelectPass();
llvm::Pass *createLowerUnsignedICmpPass();
llvm::Pass *createMarkInternalInlinePass();
llvm::Pass *createRemoveUnreachableBlocksPass();
llvm::Pass *createSimplifyAssumePass();
llvm::Pass *createDevirtualizeFunctionsPass();
llvm::Pass *createExternalizeAddressTakenFunctionsPass();
llvm::Pass *createExternalizeFunctionsPass();
llvm::Pass *createPromoteMallocPass();
llvm::Pass *createPromoteAssumePass();
llvm::Pass *createRenameNondetPass();
llvm::Pass *createNondetInitPass();
llvm::Pass *createDeadNondetElimPass();
llvm::Pass *createLoopPeelerPass(unsigned Num);
// Visualization passes
llvm::Pass *createAnnotatedCFGPrinterPass();
// Property instrumentation passes
llvm::Pass *createNullCheckPass();
llvm::Pass *createUseAfterFreeCheckPass();
// Postprocessing passes
llvm::Pass *createOptimizerPass(ClamGlobalAnalysis *clam = nullptr);  
} // namespace clam

// No createInstCombine() here anymore: llvm-seahorn ships SeaInstCombine as a
// new-PM pass only, so the pipelines build it directly (see the drivers in
// tools/). NB llvm-seahorn's SeaInstCombine.h reuses LLVM's own
// LLVM_TRANSFORMS_INSTCOMBINE_INSTCOMBINE_H include guard, so including it
// after llvm/Transforms/InstCombine/InstCombine.h silently yields nothing.
