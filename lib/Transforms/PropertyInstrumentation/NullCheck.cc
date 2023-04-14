/*
 * Instrument a program to add null dereference checks to all memory
 * accesses.
 */

#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "./MemoryCheck.hh"

static llvm::cl::opt<bool>
OptimizeChecks("clam-null-check-optimize",
	       llvm::cl::desc("Minimize the number of instrumented checks"),
	       llvm::cl::Hidden,
	       llvm::cl::init(true));

static llvm::cl::opt<bool>
AddMemSafety("clam-null-check-mem-safety",
	     llvm::cl::desc("Add safety assumptions such as "
			    "successful load/store imply validity of their arguments"),
	     llvm::cl::Hidden,
	     llvm::cl::init(false));

static llvm::cl::opt<bool>
IgnoreMemoryInstrinsics("clam-null-ignore-memintrinsics",
	       llvm::cl::desc("Skip checks on memory intrinsics"),
	       llvm::cl::Hidden,
	       llvm::cl::init(false));

#define NC_LOG(...)
//#define NC_LOG(...) llvm::errs() << "NullCheck: "; __VA_ARGS__

namespace clam {

using namespace llvm;

// static Value *getCastedInt8PtrValue(IRBuilder<> &B, Value *Ptr) {
//   auto *PT = cast<PointerType>(Ptr->getType());
//   if (PT->getElementType()->isIntegerTy(8))
//     return Ptr;
//   return B.CreateBitCast(Ptr, Type::getInt8PtrTy(B.getContext()));
// }

class NullCheck : public llvm::ModulePass {
public:
  static char ID;

private:
  unsigned ChecksAdded;
  unsigned TrivialChecks;
  Function *AssertFn;
  Function *AssumeFn;
  CallGraph *CG;

  void insertNullCheck(Value *Ptr, IRBuilder<> &B, Instruction *I);

public:
  NullCheck()
      : llvm::ModulePass(ID), ChecksAdded(0), TrivialChecks(0),
        AssertFn(nullptr), AssumeFn(nullptr), CG(nullptr) {}

  virtual bool runOnModule(llvm::Module &M) override;
  bool runOnFunction(Function &F);
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  virtual StringRef getPassName() const override { return "NullCheck"; }
};

void NullCheck::insertNullCheck(Value *Ptr, IRBuilder<> &B, Instruction *I) {
/*
  Before:
       I;
  After:
       %b = Ptr != null;
       clam_assert(%b);  
       I;
*/  
  static unsigned id = 0;
  B.SetInsertPoint(I);
  Value *GtNull = B.CreateICmpNE /*SGT*/ (
      Ptr, Constant::getNullValue(Ptr->getType()), "null_check");

  if (Constant *C = dyn_cast<Constant>(GtNull)) {
    if (ConstantInt *CI = dyn_cast<ConstantInt>(C)) {
      if (CI == ConstantInt::getTrue(B.getContext())) {
        NC_LOG(errs() << "Memory access is trivially safe " << *I << "\n";);
        TrivialChecks++;
        return;
      } else if (CI == ConstantInt::getFalse(B.getContext())) {
        NC_LOG(errs() << "Memory access is trivially unsafe " << *I << "\n";);
        TrivialChecks++;
        return;
      }
    }
  }

  ChecksAdded++;
  auto &ctx = B.getContext();

  if (isa<ConstantExpr>(GtNull)) {
    // The IRBuilder will not insert any instruction if GtNull is a constant.
    // We force to add the instruction here.
    Instruction *Cond =
        new ICmpInst(ICmpInst::ICMP_NE /*SGT*/, Ptr,
                     Constant::getNullValue(Ptr->getType()), "null_check");
    auto InsertPt = B.GetInsertPoint();
    B.GetInsertBlock()->getInstList().insert(InsertPt, Cond);
    GtNull = Cond;
  }

  // add special assertion instruction
  CallInst *CI = B.CreateCall(
      AssertFn, B.CreateZExtOrTrunc(GtNull, Type::getInt32Ty(ctx)));

  // add some metadata
  CI->setMetadata(
      "clam-assertion",
      MDNode::get(ctx,
		  MDTuple::get(ctx,
			       {MDString::get(ctx, "nullity"),
				MDString::get(ctx, std::to_string(id++))})));
  

  // update call graph
  if (CG) {
    Function *Fn = I->getParent()->getParent();
    auto f1 = CG->getOrInsertFunction(Fn);
    auto f2 = CG->getOrInsertFunction(AssertFn);
    f1->addCalledFunction(CI, f2);
  }
}

bool NullCheck::runOnFunction(Function &F) {
  // Cache to avoid checking the same thing
  SmallSet<Value *, 16> TempsToInstrument;
  // Contain a pair of a pointer to be checked and its user.
  std::vector<std::pair<Value*, Instruction *>> Worklist;
  
  if (F.empty()) {
    return false;
  }

  // Decide if the check is relevant
  auto isRelevantCheck = [](Value *Ptr, SmallSet<Value*, 16> &seen) {
       auto BasePair = property_instrumentation::getBasePtr(Ptr);
       if (Value *BasePtr = BasePair.getPointer()) {
	 if (BasePair.getInt() == 1) {
	   NC_LOG(errs() << "Skipped " << *BasePtr
		         << " because it is dereferenceable!\n";);
	   return false;
	 }
	 // We've checked BasePtr in the current BB.
	 if (!seen.insert(BasePtr).second) {
	   NC_LOG(errs() << "Skipped " << *BasePtr
		         << " because already checked!\n";);
	   return false;
	 }
       }
       return true;
  };

  // Extract pointer operand
  auto getPtrFromUnaryMemOp = [](Instruction &I) -> Value* {
	  if (isa<LoadInst>(I)) {
	    return I.getOperand(0);
	  } else if (isa<StoreInst>(I)) {
	    return I.getOperand(1);
	  } else if (!IgnoreMemoryInstrinsics && isa<MemSetInst>(I)) {
	    return I.getOperand(0);
	  } else {
	    return nullptr;
	  }
  };

  // Extract pointer operands
  auto getPtrFromBinaryMemOp = [](Instruction &I) -> std::pair<Value*,Value*> {
      if (!IgnoreMemoryInstrinsics && (isa<MemCpyInst>(I) || isa<MemMoveInst>(I))) {
	return {I.getOperand(0), I.getOperand(1)};
      } else {
	return {nullptr, nullptr};
      }
  };
  
  
  for (auto &BB : F) {
    TempsToInstrument.clear();
    for (auto &i : BB) {
      Instruction &I = *&i;
      if (Value *Ptr = getPtrFromUnaryMemOp(I)) {
	if (!isRelevantCheck(Ptr, TempsToInstrument)) {
	  TrivialChecks++;
	} else {
	  Worklist.push_back({Ptr, &I});
	}
      } else {
	std::pair<Value*,Value*> PtrPair = getPtrFromBinaryMemOp(I);
	if (PtrPair.first && PtrPair.second) {
	  if (!isRelevantCheck(PtrPair.first, TempsToInstrument)) {
	    TrivialChecks++;
	  } else {
	    Worklist.push_back({PtrPair.first, &I});
	  }
	  if (!isRelevantCheck(PtrPair.second, TempsToInstrument)) {
	    TrivialChecks++;
	  } else {
	    Worklist.push_back({PtrPair.second, &I});
	  }
	}
      }
    }
  }

  LLVMContext &ctx = F.getContext();
  IRBuilder<> B(ctx);
  bool change = false;
  for (auto p : Worklist) {
    Value *Ptr = p.first;
    Instruction *PtrUser = p.second;
    insertNullCheck(Ptr, B, PtrUser);
    
    if (AddMemSafety) {
      // -- Add extra memory safety assumption: successful load/store
      //    implies validity of their arguments.
      if (GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(Ptr)) {
        if (gep->isInBounds() && gep->getPointerAddressSpace() == 0) {
          Value *base = gep->getPointerOperand();
          B.SetInsertPoint(PtrUser);
          auto It = B.GetInsertPoint();
          ++It;
          B.SetInsertPoint(PtrUser->getParent(), It);
          CallInst *CI = B.CreateCall(AssumeFn, B.CreateIsNotNull(base));
          CI->setDebugLoc(PtrUser->getDebugLoc());

          NC_LOG(errs() << "Added memory safety assumption for " << *base
                        << "\n";);
          // update call graph
          if (CG) {
            auto f1 = CG->getOrInsertFunction(&F);
            auto f2 = CG->getOrInsertFunction(AssumeFn);
            f1->addCalledFunction(CI, f2);
          }
        }
      }
    }
    change = true;
  }
  return change;
}

bool NullCheck::runOnModule(llvm::Module &M) {

  if (M.begin() == M.end())
    return false;

  // Get call graph
  CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass>();
  if (cgwp) {
    CG = &cgwp->getCallGraph();
  }

  LLVMContext &ctx = M.getContext();

  {
    AttrBuilder B(ctx);
    // Function does not access memory
    B.addAttribute(Attribute::ReadNone);
    AttributeList as = AttributeList::get(ctx, AttributeList::FunctionIndex, B);
    AssumeFn = dyn_cast<Function>(M.getOrInsertFunction("__CRAB_assume", as,
                                                        Type::getVoidTy(ctx),
                                                        Type::getInt32Ty(ctx))
                                      .getCallee());
  }

  {
    AttrBuilder B(ctx);
    // Function does not access memory
    B.addAttribute(Attribute::ReadNone);
    B.addAttribute(Attribute::NoReturn);
    B.addAttribute(Attribute::ReadNone);
    AttributeList as = AttributeList::get(ctx, AttributeList::FunctionIndex, B);
    AssertFn = dyn_cast<Function>(M.getOrInsertFunction("__CRAB_assert", as,
                                                        Type::getVoidTy(ctx),
                                                        Type::getInt32Ty(ctx))
                                      .getCallee());
  }

  bool change = false;
  for (Function &F : M) {
    if (!F.empty()) {
      change |= runOnFunction(F);
    }
  }

  errs() << "-- Inserted " << ChecksAdded;
  errs() << " null dereference checks ";
  errs() << " (skipped " << TrivialChecks << " trivial checks). "
         << "Considering Load/Store/MemSet/MemCpy/MemMove instructions.\n";

  return change;
}

void NullCheck::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
  AU.setPreservesAll();
  AU.addRequired<CallGraphWrapperPass>();
  AU.addPreserved<CallGraphWrapperPass>();
  // TODO: probably also seadsa CallGraph
}

char NullCheck::ID = 0;

Pass *createNullCheckPass() { return new NullCheck(); }

} // end namespace clam

static llvm::RegisterPass<clam::NullCheck>
    X("clam-null-check", "Insert null dereference checks", false, false);
