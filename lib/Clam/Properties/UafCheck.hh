#include "../CfgBuilderLit.hh"
#include "clam/CrabIREmitter.hh"

#include <memory>

namespace clam {

class EmitUafChecksImpl;

/* Emit CrabIR statements to perform check for use-after-free errors */
class EmitUafChecks : public CrabIREmitter {
  std::unique_ptr<EmitUafChecksImpl> m_impl;

public:
  EmitUafChecks(const CrabBuilderParams &params, crabLitFactory &lfac,
                uint32_t &assertionId);
  virtual ~EmitUafChecks() override;

  virtual void visitBeforeBasicBlock(llvm::BasicBlock &BB) override;
  virtual void visitAfterBasicBlock(llvm::BasicBlock &BB) override;
  virtual void visitBeforeAlloc(llvm::Instruction &I,
                                const llvm::TargetLibraryInfo &tli,
                                CrabMakeRefOps &s) override;
  virtual void visitAfterAlloc(llvm::Instruction &I,
                               const llvm::TargetLibraryInfo &tli,
                               CrabMakeRefOps &s) override;
  virtual void visitBeforeFree(llvm::Instruction &I,
                               const llvm::TargetLibraryInfo &tli,
                               CrabRemoveRefOps &s) override;
  virtual void visitAfterFree(llvm::Instruction &I,
                              const llvm::TargetLibraryInfo &tli,
                              CrabRemoveRefOps &s) override;
  virtual void visitBeforeGep(llvm::Instruction &I, CrabGepRefOps &s) override;
  virtual void visitAfterGep(llvm::Instruction &I, CrabGepRefOps &s) override;
  virtual void visitBeforeStore(llvm::StoreInst &I,
                                CrabStoreRefOps &s) override;
  virtual void visitAfterStore(llvm::StoreInst &I, CrabStoreRefOps &s) override;
  virtual void visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) override;
  virtual void visitAfterLoad(llvm::LoadInst &I, CrabLoadRefOps &s) override;
  virtual void visitBeforeMemset(llvm::MemSetInst &I,
                                 CrabMemsetOps &s) override;
  virtual void visitAfterMemset(llvm::MemSetInst &I, CrabMemsetOps &s) override;
  virtual void visitBeforeMemTransfer(llvm::MemTransferInst &I,
                                      CrabMemTransferOps &s) override;
  virtual void visitAfterMemTransfer(llvm::MemTransferInst &I,
                                     CrabMemTransferOps &s) override;
  virtual void visitBeforeRefSelect(llvm::SelectInst &I,
                                    CrabSelectRefOps &s) override;
  virtual void visitAfterRefSelect(llvm::SelectInst &I,
                                   CrabSelectRefOps &s) override;
  virtual void visitBeforeIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) override;
  virtual void visitAfterIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) override;  
  
};

} // end namespace clam
