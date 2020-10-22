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
llvm::Pass *createLoopPeelerPass(unsigned Num);
// Visualization passes
llvm::Pass *createAnnotatedCFGPrinterPass();
} // namespace clam

#ifdef HAVE_LLVM_SEAHORN
#include "llvm_seahorn/Transforms/InstCombine/SeaInstCombine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"

llvm::FunctionPass *createSeaInstructionCombiningPass(bool ExpensiveCombines,
						      unsigned MaxIterations,
						      bool AvoidBv,
						      bool AvoidUnsignedICmp,
						      bool AvoidIntToPtr,
						      bool AvoidAliasing,
						      bool AvoidDisequalities);

namespace clam {
inline llvm::FunctionPass *createInstCombine(bool ExpensiveCombines = true) {
  const unsigned MaxIterations = 1000; /*same value used by LLVM*/
  const bool AvoidBv = true;
  const bool AvoidUnsignedICmp = true;
  const bool AvoidIntToPtr = true;
  const bool AvoidAliasing = true;
  const bool AvoidDisequalities = true;
  return createSeaInstructionCombiningPass(ExpensiveCombines,
					   MaxIterations,
					   AvoidBv,
					   AvoidUnsignedICmp,
					   AvoidIntToPtr,
					   AvoidAliasing,
					   AvoidDisequalities);
}
} // namespace clam
#else
namespace clam {
inline llvm::FunctionPass *createInstCombine(bool ExpensiveCombines = true) {
  return llvm::createInstructionCombiningPass(ExpensiveCombines);
}
} // namespace clam
#endif
