#include "CfgBuilderUtils.hh"

#include "llvm/ADT/APInt.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

#include <cstdint>

namespace clam {

using namespace ikos;
using namespace llvm;

// Any integer that cannot be represented by 64 bits is considered a bignum.
bool isSignedBigNum(const APInt &v) {
  unsigned b = v.getBitWidth();
  if (b <= 64) {
    return false;
  } else {
    // if bitwidth > 64 then we check the actual value
    APInt max(b, APInt::getSignedMaxValue(64).getSExtValue(), true);
    APInt min(b, APInt::getSignedMinValue(64).getSExtValue(), true);
    return (v.sgt(max) || v.slt(min));
  }
}

bool isBool(const Type *t) { return (t->isIntegerTy(1)); }

bool isBool(const Value &v) { return isBool(v.getType()); }

bool isInteger(const Type *t) { return (t->isIntegerTy() && !isBool(t)); }

bool isInteger(const Value &v) { return isInteger(v.getType()); }

bool isReference(const Type *t, const CrabBuilderParams &params) {
  return (t->isPointerTy() && params.trackMemory());
}

bool isReference(const Value &v, const CrabBuilderParams &params) {
  return isReference(v.getType(), params);
}

z_number toZNumber(const APInt &v, const CrabBuilderParams &params,
                   bool &is_bignum) {
  is_bignum = false;
  if (!params.enable_bignums) {
    is_bignum = isSignedBigNum(v);
  }
#if 0
  // Convert to strings is not ideal but it shouldn't be a big
  // bottleneck.
  std::string val = v.toString(10,true /*is signed*/);
  return z_number(val);
#else
  // Based on:
  // https://llvm.org/svn/llvm-project/polly/trunk/lib/Support/GICHelper.cpp
  APInt abs;
  abs = v.isNegative() ? v.abs() : v;
  const uint64_t *rawdata = abs.getRawData();
  unsigned numWords = abs.getNumWords();

  ikos::z_number res;
  mpz_import(res.get_mpz_t(), numWords, -1, sizeof(uint64_t), 0, 0, rawdata);
  return v.isNegative() ? -res : res;
#endif
}

z_number getIntConstant(const ConstantInt *CI, const CrabBuilderParams &params,
                        bool &is_bignum) {
  is_bignum = false;
  if (CI->getType()->isIntegerTy(1)) {
    return z_number((int64_t)CI->getZExtValue());
  } else {
    return toZNumber(CI->getValue(), params, is_bignum);
  }
}

bool isTrackedType(const Type &ty, const CrabBuilderParams &params) {
  return isReference(&ty, params) || ty.isIntegerTy();
}

bool isTracked(const Value &v, const CrabBuilderParams &params) {
  // -- ignore any shadow variable created by seahorn
  // if (v.getName().startswith("shadow.mem"))
  // return false;

  return isTrackedType(*v.getType(), params);
}

bool ShouldCallSiteReturn(CallInst &I, const CrabBuilderParams &params) {
  CallSite CS(&I);
  if (Function *Callee =
          dyn_cast<Function>(CS.getCalledValue()->stripPointerCasts())) {
    Type *RT = Callee->getReturnType();
    return (!(RT->isVoidTy()) && isTrackedType(*RT, params));
  }
  return false;
}

bool DoesCallSiteReturn(CallInst &I, const CrabBuilderParams &params) {
  return (!I.getType()->isVoidTy() && isTracked(I, params));
}

bool hasDebugLoc(const Instruction *inst) {
  if (!inst)
    return false;
  const DebugLoc &dloc = inst->getDebugLoc();
  return dloc;
}

crab::cfg::debug_info getDebugLoc(const Instruction *I) {
  if (hasDebugLoc(I)) {
    const DebugLoc &dloc = I->getDebugLoc();
    unsigned Line = dloc.getLine();
    unsigned Col = dloc.getCol();
    std::string File = (*dloc).getFilename();
    int64_t Id = getAssertIdFromMetadata(I->getMetadata("clam-assertion"));
    return crab::cfg::debug_info(File, Line, Col, Id);
  } else {
    int64_t Id = getAssertIdFromMetadata(I->getMetadata("clam-assertion"));
    return crab::cfg::debug_info(Id);
  } 
}

uint64_t storageSize(const Type *t, const DataLayout &dl) {
  return dl.getTypeStoreSize(const_cast<Type *>(t));
}

void normalizeCmpInst(CmpInst &I) {
  switch (I.getPredicate()) {
  case ICmpInst::ICMP_UGT:
  case ICmpInst::ICMP_SGT:
    I.swapOperands();
    break;
  case ICmpInst::ICMP_UGE:
  case ICmpInst::ICMP_SGE:
    I.swapOperands();
    break;
  default:;
  }
}

bool isIntToBool(const CastInst &I) {
  return (isa<TruncInst>(I) && I.getDestTy()->isIntegerTy(1));
}

bool isBoolToInt(const CastInst &I) {
  return ((isa<ZExtInst>(I) || isa<SExtInst>(I)) &&
          I.getSrcTy()->isIntegerTy(1));
}

bool isBoolArray(const Type &T) {
  return (T.isArrayTy() && T.getArrayElementType()->isIntegerTy(1));
}

bool isIntArray(const Type &T) {
  return (T.isArrayTy() && T.getArrayElementType()->isIntegerTy() &&
          !(T.getArrayElementType()->isIntegerTy(1)));
}

// bool isPointerArray(const Type &T) {
//   return (T.isArrayTy() && T.getArrayElementType()->isPointerTy());
// }

bool isAssertFn(const Function &F) {
  return (F.getName().equals("verifier.assert") ||
          F.getName().equals("crab.assert") ||
          F.getName().equals("__CRAB_assert"));
}

bool isSeaHornFail(const Function &F) {
  return (F.getName().equals("seahorn.fail"));
}

bool isErrorFn(const Function &F) {
  return (F.getName().equals("seahorn.error") ||
          F.getName().equals("verifier.error") ||
          F.getName().equals("__VERIFIER_error") ||
          F.getName().equals("__SEAHORN_error"));
}

bool isAssumeFn(const Function &F) {
  return (F.getName().equals("verifier.assume") ||
          F.getName().equals("__VERIFIER_assume") ||
          F.getName().equals("__CRAB_assume") ||
	  F.getName().equals("llvm.assume"));
}

bool isNotAssumeFn(const Function &F) {
  return (F.getName().equals("verifier.assume.not") ||
          F.getName().equals("__VERIFIER_assume_not") ||
          F.getName().equals("__CRAB_assume_not"));
}

bool isVerifierCall(const Function &F) {
  return (isAssertFn(F) || isErrorFn(F) || isAssumeFn(F) || isNotAssumeFn(F) ||
          isSeaHornFail(F));
}

bool isCrabIntrinsic(const Function &F) {
  return (F.getName().startswith("__CRAB_intrinsic_"));
}

std::string getCrabIntrinsicName(const Function &F) {
  assert(isCrabIntrinsic(F));
  StringRef res = F.getName().split("__CRAB_intrinsic_").second;
  return res.str();
}

bool isZeroInitializer(const Function &F) {
  return F.getName().startswith("verifier.zero_initializer");
}

bool isZeroInitializer(const CallInst &CI) {
  ImmutableCallSite CS(&CI);
  const Value *calleeV = CS.getCalledValue();
  if (const Function *callee =
          dyn_cast<Function>(calleeV->stripPointerCasts())) {
    return isZeroInitializer(*callee);
  }
  return false;
}

bool isIntInitializer(const Function &F) {
  return F.getName().startswith("verifier.int_initializer");
}

bool isIntInitializer(const CallInst &CI) {
  ImmutableCallSite CS(&CI);
  const Value *calleeV = CS.getCalledValue();
  if (const Function *callee =
          dyn_cast<Function>(calleeV->stripPointerCasts())) {
    return isIntInitializer(*callee);
  }
  return false;
}

int64_t getAssertIdFromMetadata(MDNode *MDN) {
  // assume MDN is the metadata associate to getMetadata("clam-assertion")
  int64_t id = -1; // no id found
  if (MDN) {
    if (MDTuple *t = dyn_cast<MDTuple>(MDN->getOperand(0))) {
      if (MDString *s = dyn_cast<MDString>(t->getOperand(1))) {
	StringRef checkId = s->getString();
	unsigned long long n;
	if (!(getAsUnsignedInteger(checkId, 10, n) || (n > INT64_MAX))) {
	  id = (int64_t) n;
	}
      }
    }
  }
  return id;
}
  
std::string getAssertKindFromMetadata(MDNode *MDN) {
  // assume MDN is the metadata associate to getMetadata("clam-assertion")
  if (MDN) {
    if (MDTuple *t = dyn_cast<MDTuple>(MDN->getOperand(0))) {
      std::string checkName = cast<MDString>(t->getOperand(0))->getString();
      return checkName;
    } 
  }
  return "";
}

// Return true if all uses are BranchInst's
bool AllUsesAreBrInst(Value &V) {
  // XXX: do not strip pointers here
  for (auto &U : V.uses())
    if (!isa<BranchInst>(U.getUser()))
      return false;
  return true;
}

// Return true if all uses are BranchInst's or Select's
bool AllUsesAreBrOrIntSelectCondInst(Value &V,
                                     const CrabBuilderParams &params) {
  // XXX: do not strip pointers here
  for (auto &U : V.uses()) {
    if ((!isa<BranchInst>(U.getUser())) && (!isa<SelectInst>(U.getUser())))
      return false;
    if (SelectInst *SI = dyn_cast<SelectInst>(U.getUser())) {
      if (isBool(*SI) || SI->getCondition() != &V || isReference(*SI, params)) {
        // if the operands are bool or V is not the condition
        return false;
      }
    }
  }
  return true;
}

// Return true if all uses are the callee at callsites
bool AllUsesAreIndirectCalls(Value &V) {
  // XXX: do not strip pointers here
  for (auto &U : V.uses()) {
    if (CallInst *CI = dyn_cast<CallInst>(U.getUser())) {
      CallSite CS(CI);
      const Value *callee = CS.getCalledValue();
      if (callee == &V)
        continue;
    }
    return false;
  }
  return true;
}

// Return true if all uses are verifier calls (assume/assert)
bool AllUsesAreVerifierCalls(Value &V, bool goThroughIntegerCasts,
                             bool nonBoolCond,
                             SmallVector<CallInst *, 4> &verifierCalls) {
  for (auto &U : V.uses()) {
    Value *User = U.getUser();
    if (goThroughIntegerCasts) {
      if (isa<ZExtInst>(User) || isa<SExtInst>(User)) {
        return AllUsesAreVerifierCalls(*User, goThroughIntegerCasts,
                                       nonBoolCond, verifierCalls);
      }
    }

    if (CallInst *CI = dyn_cast<CallInst>(User)) {
      CallSite CS(CI);
      const Value *calleeV = CS.getCalledValue();
      const Function *callee = dyn_cast<Function>(calleeV->stripPointerCasts());
      if (callee && (isAssertFn(*callee) || isAssumeFn(*callee) ||
                     isNotAssumeFn(*callee))) {
        if (nonBoolCond) {
          FunctionType *FTy = callee->getFunctionType();
          if (!FTy->isVarArg() && FTy->getReturnType()->isVoidTy() &&
              FTy->getNumParams() == 1 && !isBool(FTy->getParamType(0))) {
            verifierCalls.push_back(CI);
            continue;
          }
        } else {
          verifierCalls.push_back(CI);
          continue;
        }
      }
    }
    verifierCalls.clear();
    return false;
  }
  return true;
}

bool AllUsesAreVerifierCalls(Value &V) {
  SmallVector<CallInst *, 4> verifierCalls /*unused*/;
  return AllUsesAreVerifierCalls(V, false, false, verifierCalls);
}

// Return true if all uses are GEPs
bool AllUsesAreGEP(Value &V) {
  for (auto &U : V.uses())
    if (!isa<GetElementPtrInst>(U.getUser()))
      return false;
  return true;
}

bool AllUsesAreIgnoredInst(llvm::Value &V) {
  for (auto &U : V.uses()) {
    if (CallInst *CI = dyn_cast<CallInst>(U.getUser())) {
      if (Function *CalledF = dyn_cast<Function>(CI->getCalledOperand())) {
        if (CalledF->getName().startswith("llvm.dbg.value") ||
            CalledF->getName().startswith("llvm.lifetime")) {
          continue;
        }
      }
    }
    return false;
  }
  return true;
}

} // namespace clam
