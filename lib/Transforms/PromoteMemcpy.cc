/**  The transformation we do here looks roughly like this:
        memcpy(Dst, Source, sizeof(BufferTy))
          goes to
      for each field_id in fields(BufferTy):
        *GEP(Dst, field_id) = *GEP(Src, field_id)
*/
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

// Helper to try to get the original type from a value
Type *PromoteMemcpy::getOriginalType(Value *V, Value *&SrcPtr) {
  // Handle direct struct pointers
  if (auto *PtrTy = dyn_cast<PointerType>(V->getType())) {
    if (PtrTy->getPointerElementType()->isStructTy()) {
      return PtrTy->getPointerElementType();
    }
  }

  // Handle bitcast instructions
  if (auto *BC = dyn_cast<BitCastInst>(V)) {
    Value *Source = BC->getOperand(0);
    SrcPtr = Source;
    if (auto *SrcPtrTy = dyn_cast<PointerType>(Source->getType())) {
      if (SrcPtrTy->getPointerElementType()->isStructTy()) {
        return SrcPtrTy->getPointerElementType();
      }
    }
    // Recursively look through nested bitcasts
    return getOriginalType(Source, SrcPtr);
  }

  // Handle GEP instructions (struct member access)
  if (auto *GEP = dyn_cast<GetElementPtrInst>(V)) {
    if (auto *SrcTy = GEP->getSourceElementType()) {
      if (SrcTy->isStructTy()) {
        return SrcTy;
      }
    }
  }

  // Handle alloca instructions
  if (auto *Alloca = dyn_cast<AllocaInst>(V)) {
    Type *AllocatedTy = Alloca->getAllocatedType();
    if (AllocatedTy->isStructTy()) {
      return AllocatedTy;
    }
  }

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

  auto *SrcPtrTy = cast<PointerType>(SrcPtr->getType());
  auto *DstPtrTy = cast<PointerType>(DstPtr->getType());

  // Method 1: Direct struct pointer check
  if (SrcPtrTy->getPointerElementType()->isFirstClassType() &&
      DstPtrTy->getPointerElementType()->isFirstClassType()) {

    auto *SrcDataTy = SrcPtrTy->getPointerElementType();
    auto *DstDataTy = DstPtrTy->getPointerElementType();

    // Ensure both are the same type
    if (SrcDataTy != DstDataTy) {
      return false;
    }

    if (MemOpLength &&
        m_DL->getTypeStoreSize(SrcDataTy) == MemOpLength->getLimitedValue()) {
      BufferTy = SrcDataTy;
      return true;
    }
  }

  // Method 2: Handle bitcast to i8* (common pattern)
  if (SrcPtrTy->getPointerElementType()->isIntegerTy(8) &&
      DstPtrTy->getPointerElementType()->isIntegerTy(8)) {

    // Look through bitcasts to find original struct types
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
  using Transfer = std::pair<Value *, Value *>;
  SmallVector<Transfer, 4> ToLower = {std::make_pair(SrcPtr, DstPtr)};
  while (!ToLower.empty()) {
    Value *TrSrc, *TrDst;
    std::tie(TrSrc, TrDst) = ToLower.pop_back_val();
    auto *Ty = TrSrc->getType();
    assert(Ty == TrDst->getType());

    if (!Ty->isStructTy()) {
      assert(TrSrc->getType()->isPointerTy());
      auto *TrSrcPtr = cast<PointerType>(TrSrc->getType());
      auto *LoadedTy = TrSrcPtr->getPointerElementType();
      auto *NewLoad =
          Builder.CreateLoad(LoadedTy, TrSrc, SrcPtr->getName() + ".pmcpy");
      auto *NewStore = Builder.CreateStore(NewLoad, TrDst);
      continue;
    }

    SmallVector<Transfer, 8> TmpBuff;
    for (unsigned i = 0, e = Ty->getStructNumElements(); i != e; ++i) {
      auto *Idx = Constant::getIntegerValue(I32Ty, APInt(32, i));
      auto *SrcGEP = Builder.CreateInBoundsGEP(nullptr, SrcPtr, {NullInt, Idx},
                                               "src.gep.pmcpy");
      auto *DstGEP = Builder.CreateInBoundsGEP(nullptr, DstPtr, {NullInt, Idx},
                                               "buffer.gep.pmcpy");
      TmpBuff.push_back({SrcGEP, DstGEP});
    }

    for (auto &P : llvm::reverse(TmpBuff)) {
      ToLower.push_back(P);
    }
  }
  return true;
}

bool PromoteMemcpy::runOnFunction(Function &F) {
  if (F.empty())
    return false;

  m_DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  m_M = F.getParent();
  m_Ctx = &m_M->getContext();
  m_DL = &m_M->getDataLayout();
  m_AC = &getAnalysis<AssumptionCacheTracker>().getAssumptionCache(F);

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

} // namespace

namespace clam {
llvm::FunctionPass *createPromoteMemcpyPass() { return new PromoteMemcpy(); }
} // namespace clam

static llvm::RegisterPass<PromoteMemcpy>
    X("promote-memcpy", "Promote memcpy to field-wise stores");