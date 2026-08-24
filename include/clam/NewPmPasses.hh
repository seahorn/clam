#pragma once

/**
 * New-pass-manager versions of the Clam passes run by the clam and clam-pp
 * pipelines.
 *
 * Both drivers build their pipelines with PassBuilder, so every pass they run
 * has to be a new-PM pass. The legacy passes declared in Passes.hh are still
 * around for the parts of Clam that have not moved yet (ClamPass, the
 * invariant-based Optimizer and the passes that surround them in clam.cc);
 * where both exist they share one implementation.
 */

#include "clam/config.h"

// Defines LoopAnalysisManager and LoopStandardAnalysisResults, which
// LoopPeelerPass::run takes by reference.
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/IR/PassManager.h"

namespace llvm {
class Loop;
class LPMUpdater;
} // namespace llvm

namespace clam {

/** Create an entry point if main does not exist. */
class InsertEntryPointPass : public llvm::PassInfoMixin<InsertEntryPointPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Promote top-level mallocs to allocas. */
class PromoteMallocPass : public llvm::PassInfoMixin<PromoteMallocPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Insert tag-analysis (taint) intrinsics per the user taint policy. */
class InsertTaintIntrinsicPass
    : public llvm::PassInfoMixin<InsertTaintIntrinsicPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Replace integer operands that LazyValueInfo proves constant. */
class LazyValueConstPass : public llvm::PassInfoMixin<LazyValueConstPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Promote whole-object memcpys to aggregate loads and stores. */
class PromoteMemcpyPass : public llvm::PassInfoMixin<PromoteMemcpyPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/**
 * Resolve indirect calls.
 *
 * With --devirt-functions=sea-dsa this needs sea-dsa's complete call graph,
 * which it builds from the cached AllocWrapInfo/DsaLibFuncInfo module
 * analyses. sea-dsa's RemovePtrToInt has to have run over the module first;
 * the pipelines schedule it themselves rather than requiring it here.
 */
class DevirtualizeFunctionsPass
    : public llvm::PassInfoMixin<DevirtualizeFunctionsPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Externalize user-selected functions. */
class ExternalizeFunctionsPass
    : public llvm::PassInfoMixin<ExternalizeFunctionsPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Externalize uses of address-taken functions. */
class ExternalizeAddressTakenFunctionsPass
    : public llvm::PassInfoMixin<ExternalizeAddressTakenFunctionsPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Mark internal functions with the AlwaysInline attribute. */
class MarkInternalInlinePass
    : public llvm::PassInfoMixin<MarkInternalInlinePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Remove unreachable blocks. */
class RemoveUnreachableBlocksPass
    : public llvm::PassInfoMixin<RemoveUnreachableBlocksPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Turn undefined behaviour into non-determinism. */
class NondetInitPass : public llvm::PassInfoMixin<NondetInitPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Remove calls to nondet functions whose result is unused. */
class DeadNondetElimPass : public llvm::PassInfoMixin<DeadNondetElimPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Lower constant expressions to instructions. */
class LowerCstExprPass : public llvm::PassInfoMixin<LowerCstExprPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

/** Lower all select instructions. */
class LowerSelectPass : public llvm::PassInfoMixin<LowerSelectPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Lower umax/umin/smax/smin intrinsics to icmp + select. */
class LowerMinMaxIntrinsicsPass
    : public llvm::PassInfoMixin<LowerMinMaxIntrinsicsPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Lower ULT and ULE instructions. */
class LowerUnsignedICmpPass
    : public llvm::PassInfoMixin<LowerUnsignedICmpPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Promote verifier.assume calls to llvm.assume intrinsics. */
class PromoteAssumePass : public llvm::PassInfoMixin<PromoteAssumePass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

/** Peel the first Num iterations of a loop. */
class LoopPeelerPass : public llvm::PassInfoMixin<LoopPeelerPass> {
  unsigned m_Num;

public:
  explicit LoopPeelerPass(unsigned Num) : m_Num(Num) {}
  llvm::PreservedAnalyses run(llvm::Loop &L, llvm::LoopAnalysisManager &LAM,
                              llvm::LoopStandardAnalysisResults &AR,
                              llvm::LPMUpdater &U);
};

} // namespace clam
