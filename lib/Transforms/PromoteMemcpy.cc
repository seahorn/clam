/**  The transformation we do here looks roughly like this:
        memcpy(Dst, Source, sizeof(BufferTy))
          goes to
      for each field_id in fields(BufferTy):
        *GEP(Dst, field_id) = *GEP(Src, field_id)
*/
#include "clam/NewPmPasses.hh"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BuildLibCalls.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/SimplifyLibCalls.h"

using namespace llvm;

namespace {
class PromoteMemcpy : public FunctionPass {
public:
  static char ID;

  PromoteMemcpy() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override;
  bool runImpl(Function &F, DominatorTree *DT, AssumptionCache *AC);

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addRequired<llvm::DominatorTreeWrapperPass>();
    AU.addRequired<AssumptionCacheTracker>();
  }

  StringRef getPassName() const override { return "PromoteMemcpy"; }

private:
  Module *m_M = nullptr;
  LLVMContext *m_Ctx = nullptr;
  const DataLayout *m_DL = nullptr;
  DominatorTree *m_DT = nullptr;
  AssumptionCache *m_AC = nullptr;

  bool simplifyMemCpy(MemCpyInst *MCpy);
  Type *getOriginalType(Value *V, Value *&SrcPtr);
  bool isFirstClassMemcpy(MemCpyInst *MI, Type *&BufferTy, Value *&SrcPtr,
                          Value *&DstPtr);
};

char PromoteMemcpy::ID = 0;

// Helper to try to get the original type from a value. Under opaque pointers
// the pointee type is gone, so the type comes from the underlying object:
// alloca allocated type, global value type, or a GEP's result element type.
Type *PromoteMemcpy::getOriginalType(Value *V, Value *&SrcPtr) {
  V = V->stripPointerCasts();
  SrcPtr = V;

  if (auto *Alloca = dyn_cast<AllocaInst>(V))
    return Alloca->getAllocatedType();

  if (auto *GV = dyn_cast<GlobalVariable>(V))
    return GV->getValueType();

  // Struct member access: the copied object is the GEP's element.
  if (auto *GEP = dyn_cast<GetElementPtrInst>(V))
    return GEP->getResultElementType();

  return nullptr;
}

// Check if the memcpy corresponds to a copy for the first class type
bool PromoteMemcpy::isFirstClassMemcpy(MemCpyInst *MI, Type *&BufferTy,
                                       Value *&SrcPtr, Value *&DstPtr) {
  // Get source and destination pointers
  SrcPtr = MI->getSource();
  DstPtr = MI->getDest();
  ConstantInt *MemOpLength = dyn_cast<ConstantInt>(MI->getLength());

  // Check if pointers have the same address space
  unsigned SrcAddrSp = cast<PointerType>(SrcPtr->getType())->getAddressSpace();
  unsigned DstAddrSp = cast<PointerType>(DstPtr->getType())->getAddressSpace();
  if (SrcAddrSp != DstAddrSp) {
    llvm_unreachable("unexpected");
    return false;
  }

  // Opaque pointers carry no pointee type, so both the former "direct struct
  // pointer" and "bitcast to i8*" cases reduce to recovering the copied type
  // from the underlying objects and checking it spans the whole memcpy.
  auto *RawSrcPtr = MI->getRawSource();
  auto *RawDstPtr = MI->getRawDest();

  Type *SrcDataTy = getOriginalType(RawSrcPtr, SrcPtr);
  Type *DstDataTy = getOriginalType(RawDstPtr, DstPtr);

  if (SrcDataTy && DstDataTy && SrcDataTy->isFirstClassType() &&
      SrcDataTy == DstDataTy) {

    // Verify size matches memcpy length
    if (MemOpLength) {
      uint64_t Size = MemOpLength->getLimitedValue();
      uint64_t TypeSize = m_DL->getTypeStoreSize(SrcDataTy);

      if (TypeSize == Size) {
        BufferTy = SrcDataTy;
        return true;
      }
    }
  }

  return false;
}

bool PromoteMemcpy::simplifyMemCpy(MemCpyInst *MI) {
  assert(MI);
#if 0

  auto DstAlign = getKnownAlignment(MI->getDest(), *m_DL, MI, m_AC, m_DT);
  auto SrcAlign = getKnownAlignment(MI->getSource(), *m_DL, MI, m_AC, m_DT);

  // -- alignment on memcpy should be trusted, alignment on arguments is not important
  // -- at most should check that alignment of src and dst is the same
  if (MI->getSourceAlignment() != SrcAlign) {
    return false;
  }
  else if (MI->getDestAlignment() != DstAlign) {
    return false;
  }
#endif

  // skip non-constant length memcpy()
  ConstantInt *MemOpLength = dyn_cast<ConstantInt>(MI->getLength());
  if (!MemOpLength) {
    return false;
  }

  // Source and destination pointer types are always "i8*" for intrinsic.  See
  // if the size is something we can handle with a single primitive load/store.
  // A single load+store correctly handles overlapping memory in the memmove
  // case.
  uint64_t Size = MemOpLength->getLimitedValue();
  if (Size == 0) {
    return false;
  }

  Type *BufferTy = nullptr;
  Value *SrcPtr = nullptr, *DstPtr = nullptr;

  IRBuilder<> Builder(MI);
  auto *I64Ty = IntegerType::getInt64Ty(*m_Ctx);
  auto *NullInt = Constant::getNullValue(I64Ty);
  auto *I32Ty = IntegerType::getInt32Ty(*m_Ctx);

  // Check if memcpy can be achieved
  if (!isFirstClassMemcpy(MI, BufferTy, SrcPtr, DstPtr)) {
    return false;
  }

  if (m_DL->getTypeStoreSize(BufferTy) != Size) {
    return false;
  }

  // Perform field-wise copy. Note that this doesn't recurse and only explores
  // the immediately visible fields.
  //
  // The transformation we do here looks roughly like this:
  //   memcpy(Dst, Source, sizeof(BufferTy))
  //    ||
  //    V
  // for each field_id in fields(BufferTy):
  //   *GEP(Dst, field_id) = *GEP(Src, field_id)
  //
  // Under typed pointers the field-wise worklist below this comment was dead
  // code (its struct test ran on the *pointer* type), so every promoted
  // memcpy was emitted as one whole-aggregate load/store. Keep exactly that
  // behavior; BufferTy now supplies the type the pointee used to.
  Builder.CreateStore(
      Builder.CreateLoad(BufferTy, SrcPtr, SrcPtr->getName() + ".pmcpy"),
      DstPtr);
  (void)NullInt;
  (void)I32Ty;
  return true;
}

bool PromoteMemcpy::runImpl(Function &F, DominatorTree *DT,
                            AssumptionCache *AC) {
  if (F.empty())
    return false;

  m_DT = DT;
  m_M = F.getParent();
  m_Ctx = &m_M->getContext();
  m_DL = &m_M->getDataLayout();
  m_AC = AC;

  bool Changed = false;
  SmallVector<MemCpyInst *, 8> ToDeleteQueue;

  for (auto &BB : F)
    for (auto &I : BB)
      if (auto *MCpy = dyn_cast<MemCpyInst>(&I)) {

        if (!simplifyMemCpy(MCpy))
          continue;

        ToDeleteQueue.push_back(MCpy);
        Changed = true;
      }

  for (auto *MCpy : ToDeleteQueue) {

    // Using getArgOperand API to avoid looking through casts.
    auto *SrcPtr = dyn_cast<BitCastInst>(MCpy->getArgOperand(1));
    auto *DstPtr = dyn_cast<BitCastInst>(MCpy->getArgOperand(0));

    MCpy->eraseFromParent();
    if (SrcPtr && SrcPtr->hasNUses(0)) {
      SrcPtr->eraseFromParent();
    }
    if (DstPtr && DstPtr->hasNUses(0)) {
      DstPtr->eraseFromParent();
    }
  }

  return Changed;
}

bool PromoteMemcpy::runOnFunction(Function &F) {
  if (F.empty())
    return false;
  auto *DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  auto *AC = &getAnalysis<AssumptionCacheTracker>().getAssumptionCache(F);
  return runImpl(F, DT, AC);
}

} // namespace

namespace clam {
llvm::FunctionPass *createPromoteMemcpyPass() { return new PromoteMemcpy(); }

PreservedAnalyses PromoteMemcpyPass::run(Function &F,
                                         FunctionAnalysisManager &FAM) {
  auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
  auto &AC = FAM.getResult<AssumptionAnalysis>(F);
  PromoteMemcpy P;
  if (!P.runImpl(F, &DT, &AC)) {
    return PreservedAnalyses::all();
  }
  // memcpys are replaced by loads/stores; the CFG is untouched.
  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
} // namespace clam

static llvm::RegisterPass<PromoteMemcpy>
    X("promote-memcpy", "Promote memcpy to field-wise stores");