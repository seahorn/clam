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

#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Transforms/InstCombine/SeaInstCombine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"

llvm::FunctionPass *
createSeaInstructionCombiningPass(unsigned MaxIterations, bool AvoidBv,
                                  bool AvoidUnsignedICmp, bool AvoidIntToPtr,
                                  bool AvoidAliasing, bool AvoidDisequalities);

namespace clam {
inline llvm::FunctionPass *createInstCombine() {
  const unsigned MaxIterations = 1000; /*same value used by LLVM*/
  const bool AvoidBv = true;
  const bool AvoidUnsignedICmp = true;
  const bool AvoidIntToPtr = true;
  const bool AvoidAliasing = true;
  const bool AvoidDisequalities = true;
  return createSeaInstructionCombiningPass(
      MaxIterations, AvoidBv, AvoidUnsignedICmp,
      AvoidIntToPtr, AvoidAliasing, AvoidDisequalities);
}
} // namespace clam
#else
namespace clam {
inline llvm::FunctionPass *createInstCombine() {
  return llvm::createInstructionCombiningPass();
}
} // namespace clam
#endif
