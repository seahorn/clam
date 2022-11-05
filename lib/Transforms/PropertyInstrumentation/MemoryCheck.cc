#include "./MemoryCheck.hh"

#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"

namespace clam {
using namespace llvm;
namespace property_instrumentation {
DerefPointer getBasePtr(Value *V, SmallPtrSet<Instruction *, 8> &SeenInsts) {
  V = V->stripPointerCasts();
  if (Instruction *I = dyn_cast<Instruction>(V)) {
    // If we have already seen this instruction, bail
    // out. Cycles can happen in unreachable code after constant
    // propagation.
    if (!SeenInsts.insert(I).second)
      return DerefPointer(nullptr, 0);

    if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(V)) {
      if (GEP->isInBounds() && GEP->getPointerAddressSpace() == 0) {
        // if the pointer is the base of some gep that is directly
        // read from memory we give up.
        if (isa<LoadInst>(GEP->getPointerOperand()))
          return DerefPointer(GEP->getPointerOperand(), 0);
        else
          return getBasePtr(GEP->getPointerOperand(), SeenInsts);
      } else
        return DerefPointer(nullptr, 0);
    }
    if (isa<AllocaInst>(V))
      return DerefPointer(V, 1);
    // if (LoadInst *LI = dyn_cast<LoadInst>(V))
    //   return getBasePtr(LI->getPointerOperand(), SeenInsts);
    // if (StoreInst *SI = dyn_cast<StoreInst>(V))
    //   return getBasePtr(SI->getPointerOperand(), SeenInsts);
    if (SelectInst *SI = dyn_cast<SelectInst>(V)) {
      // searching for this pattern:
      // %_4 = gep(%b1, ...)
      // %_5 = gep(%b2, ...)
      // _call2 = select i1 %.b, i8* %_4, i8* %_5,
      DerefPointer TrueOp = getBasePtr(SI->getTrueValue(), SeenInsts);
      if (TrueOp.getInt() == 1) {
        DerefPointer FalseOp = getBasePtr(SI->getFalseValue(), SeenInsts);
        if (FalseOp.getInt() == 1) {
          // We use the base pointer to minimize the number of the
          // checks so it's fine to return either TrueOp or FalseOp.
          return TrueOp;
        }
      }
    }
    return DerefPointer(nullptr, 0);
  }

  if (Argument *A = dyn_cast<Argument>(V)) {
    // dereferenceable(<n>) does imply nonnull in addrspace(0) (which
    // is the default address space), except if the
    // null_pointer_is_valid function attribute is present.
    return DerefPointer(V, A->getDereferenceableBytes() > 0 ||
                               A->hasNonNullAttr());
  }
  if (isa<ConstantPointerNull>(V) || isa<UndefValue>(V))
    return DerefPointer(V, 0);
  if (isa<GlobalAlias>(V) || isa<GlobalVariable>(V))
    return DerefPointer(V, 1);
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(V)) {
    if (CE->getOpcode() == Instruction::GetElementPtr)
      return getBasePtr(cast<GEPOperator>(*CE).getPointerOperand(), SeenInsts);
  }
  return DerefPointer(nullptr, 0);
}

DerefPointer getBasePtr(Value *V) {
  SmallPtrSet<Instruction *, 8> SeenInsts;
  return getBasePtr(V, SeenInsts);
}
} // end namespace property_instrumentation
} // end namespace clam
