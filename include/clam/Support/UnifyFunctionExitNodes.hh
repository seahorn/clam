#pragma once

/**
 * Legacy-pass-manager wrapper around llvm::UnifyFunctionExitNodesPass.
 *
 * CfgBuilder requires each function to have at most one return instruction
 * (it reports CLAM_ERROR otherwise), and the analyses that consume it are
 * still legacy passes, so they declare that requirement with
 * AU.addRequired<>. LLVM provided such a wrapper until LLVM 17; LLVM 18
 * removed UnifyFunctionExitNodesLegacyPass and createUnifyFunctionExitNodesPass
 * and kept only the new-PM UnifyFunctionExitNodesPass, which a legacy manager
 * cannot schedule. This wrapper restores that dependency for clam's own legacy
 * passes; the transform itself is still LLVM's.
 **/

#include "llvm/IR/Function.h"
#include "llvm/Pass.h"

namespace clam {

class UnifyFunctionExitNodesLegacyPass : public llvm::FunctionPass {
public:
  static char ID;

  UnifyFunctionExitNodesLegacyPass() : llvm::FunctionPass(ID) {}

  virtual bool runOnFunction(llvm::Function &F) override;

  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

  virtual llvm::StringRef getPassName() const override {
    return "Clam: Unify function exit nodes";
  }
};
} // namespace clam
