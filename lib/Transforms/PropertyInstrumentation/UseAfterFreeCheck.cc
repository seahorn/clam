/*
 * Instrument checks for use-after-free errors in Load and Store
 * instructions. The pass only instrument dangling pointers to the
 * heap.
 * TODO: other memory instructions.
 */

#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "./MemoryCheck.hh"

static llvm::cl::opt<bool>
    OptimizeChecks("clam-uaf-check-optimize",
                   llvm::cl::desc("Minimize the number of instrumented checks"),
		   llvm::cl::Hidden,
                   llvm::cl::init(true));

#define UAF_LOG(...)
//#define UAF_LOG(...) llvm::errs() << "UAFCheck: "; __VA_ARGS__

namespace clam {

using namespace llvm;

static Value *getCastedInt8PtrValue(IRBuilder<> &B, Value *Ptr) {
  auto *PT = cast<PointerType>(Ptr->getType());
  if (PT->getPointerElementType()->isIntegerTy(8))
    return Ptr;
  return B.CreateBitCast(Ptr, Type::getInt8PtrTy(B.getContext()));
}

class UseAfterFreeCheck : public llvm::ModulePass {
public:
  static char ID;

private:
  unsigned ChecksAdded;
  unsigned TrivialChecks;
  Function *AssertFn;
  Function *AssumeFn;
  Function *NotDanglingFn;
  CallGraph *CG;

  void insertNonDanglingCheck(Value *Ptr, IRBuilder<> &B, Instruction *I);

public:
  UseAfterFreeCheck()
      : llvm::ModulePass(ID), ChecksAdded(0), TrivialChecks(0),
        AssertFn(nullptr), AssumeFn(nullptr), NotDanglingFn(nullptr),
        CG(nullptr) {}

  virtual bool runOnModule(llvm::Module &M) override;
  bool runOnFunction(Function &F);
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  virtual StringRef getPassName() const override { return "UseAfterFreeCheck"; }
};

void UseAfterFreeCheck::insertNonDanglingCheck(Value *Ptr, IRBuilder<> &B,
                                               Instruction *I) {
  static unsigned id = 0;
  ChecksAdded++;
  B.SetInsertPoint(I);
  auto &ctx = B.getContext();
  CallInst *IntrinsicCI =
      B.CreateCall(NotDanglingFn, getCastedInt8PtrValue(B, Ptr));
  CallInst *AssertCI = B.CreateCall(
      AssertFn, B.CreateZExtOrTrunc(IntrinsicCI, Type::getInt32Ty(ctx)));
  AssertCI->setMetadata(
      "clam-assertion",
      MDNode::get(ctx,
		  MDTuple::get(ctx,
			       {MDString::get(ctx, "not_dangling"),
				MDString::get(ctx, std::to_string(id++))})));
  if (CG) {
    Function *Fn = I->getParent()->getParent();
    auto f1 = CG->getOrInsertFunction(Fn);
    auto f2 = CG->getOrInsertFunction(NotDanglingFn);
    auto f3 = CG->getOrInsertFunction(AssertFn);
    f1->addCalledFunction(IntrinsicCI, f2);
    f1->addCalledFunction(AssertCI, f3);
  }
}

bool UseAfterFreeCheck::runOnFunction(Function &F) {
  if (F.empty()) {
    return false;
  }

  SmallSet<Value *, 16> TempsToInstrument;
  std::vector<Instruction *> Worklist;
  for (auto &BB : F) {
    TempsToInstrument.clear();
    for (auto &i : BB) {
      Instruction *I = &i;
      if (isa<LoadInst>(I) || isa<StoreInst>(I)) {
        if (OptimizeChecks) {
	  Value *Ptr = (isa<LoadInst>(I) ? I->getOperand(0) : I->getOperand(1));
          auto BasePair = property_instrumentation::getBasePtr(Ptr);
          if (Value *BasePtr = BasePair.getPointer()) {
            if (isa<GlobalValue>(BasePtr) || isa<AllocaInst>(BasePtr)) {
              // Note that the fact that the base is dereferenceable
              // doesn't avoid use-after-free errors because it can be
              // freed in between.
              UAF_LOG(errs() << "Skipped " << *BasePtr
                             << " because it points to a global or alloca!\n";);
              TrivialChecks++;
              continue;
            }

            // We've checked BasePtr in the current BB.
            if (!TempsToInstrument.insert(BasePtr).second) {
              UAF_LOG(errs() << "Skipped " << *BasePtr
                             << " because already checked!\n";);
              TrivialChecks++;
              continue;
            }
          }
        }
        Worklist.push_back(I);
      } else {
        // TODO memory intrinsics
      }
    }
  }

  LLVMContext &ctx = F.getContext();
  IRBuilder<> B(ctx);
  bool change = false;
  for (auto I : Worklist) {
    Value *Ptr = nullptr;
    if (auto *LI = dyn_cast<LoadInst>(I)) {
      Ptr = LI->getPointerOperand();
    } else if (auto *SI = dyn_cast<StoreInst>(I)) {
      Ptr = SI->getPointerOperand();
    } else {
      errs() << "ERROR: unknown instruction " << *I << "\n";
      continue;
    }
    // property_instrumentation::DerefPointer BasePair;
    // if (OptimizeChecks) {
    //   BasePair = property_instrumentation::getBasePtr(Ptr);
    // }
    // insertNonDanglingCheck(BasePair.getPointer() ? BasePair.getPointer() :
    // Ptr, B, I);

    insertNonDanglingCheck(Ptr, B, I);
    change = true;
  }
  return change;
}

bool UseAfterFreeCheck::runOnModule(llvm::Module &M) {

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

  {
    AttrBuilder B(ctx);
    // Function does not access memory
    B.addAttribute(Attribute::ReadNone);
    AttributeList as = AttributeList::get(ctx, AttributeList::FunctionIndex, B);
    NotDanglingFn = dyn_cast<Function>(
        M.getOrInsertFunction("__CRAB_intrinsic_is_unfreed_or_null", as,
                              Type::getInt1Ty(ctx), Type::getInt8PtrTy(ctx))
            .getCallee());
  }

  bool change = false;
  for (Function &F : M) {
    if (!F.empty()) {
      change |= runOnFunction(F);
    }
  }

  errs() << "-- Inserted " << ChecksAdded;
  errs() << " use-after-free pointer checks ";
  errs() << " (skipped " << TrivialChecks << " trivial checks). "
         << "Considering only Load and Store instructions.\n";

  return change;
}

void UseAfterFreeCheck::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
  AU.setPreservesAll();
  AU.addRequired<CallGraphWrapperPass>();
  AU.addPreserved<CallGraphWrapperPass>();
}

char UseAfterFreeCheck::ID = 0;

Pass *createUseAfterFreeCheckPass() { return new UseAfterFreeCheck(); }

} // end namespace clam

static llvm::RegisterPass<clam::UseAfterFreeCheck>
    X("clam-uaf-check", "Insert use-after-free checks", false, false);
