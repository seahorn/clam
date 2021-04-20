/*
 * Instrument a program to add null dereference checks for Load and
 * Store instructions.
 * TODO: other memory operations
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
    OptimizeChecks("clam-null-check-optimize",
                   llvm::cl::desc("Minimize the number of instrumented checks"),
                   llvm::cl::init(true));

static llvm::cl::opt<bool> AddMemSafety(
    "clam-null-check-mem-safety",
    llvm::cl::desc("Add safety assumptions such as "
                   "successful load/store imply validity of their arguments"),
    llvm::cl::init(false), llvm::cl::Hidden);

#define NC_LOG(...)
//#define NC_LOG(...) llvm::errs() << "NullCheck: "; __VA_ARGS__

namespace clam {

using namespace llvm;

static Value *getCastedInt8PtrValue(IRBuilder<> B, Value *Ptr) {
  auto *PT = cast<PointerType>(Ptr->getType());
  if (PT->getElementType()->isIntegerTy(8))
    return Ptr;
  return B.CreateBitCast(Ptr, Type::getInt8PtrTy(B.getContext()));
}

class NullCheck : public llvm::ModulePass {
public:
  static char ID;

private:
  unsigned ChecksAdded;
  unsigned TrivialChecks;
  Function *AssertFn;
  Function *AssumeFn;
  CallGraph *CG;

  void insertNullCheck(Value *Ptr, IRBuilder<> B, Instruction *I);

public:
  NullCheck()
      : llvm::ModulePass(ID), ChecksAdded(0), TrivialChecks(0),
        AssertFn(nullptr), AssumeFn(nullptr), CG(nullptr) {}

  virtual bool runOnModule(llvm::Module &M) override;
  bool runOnFunction(Function &F);
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  virtual StringRef getPassName() const override { return "NullCheck"; }
};

void NullCheck::insertNullCheck(Value *Ptr, IRBuilder<> B, Instruction *I) {
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
      MDNode::get(ctx, MDString::get(ctx, "nullity_" + std::to_string(id++))));

  // update call graph
  if (CG) {
    Function *Fn = I->getParent()->getParent();
    auto f1 = CG->getOrInsertFunction(Fn);
    auto f2 = CG->getOrInsertFunction(AssertFn);
    f1->addCalledFunction(CI, f2);
  }
}

bool NullCheck::runOnFunction(Function &F) {
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
            if (BasePair.getInt() == 1) {
              NC_LOG(errs() << "Skipped " << *BasePtr
                            << " because it is dereferenceable!\n";);
              TrivialChecks++;
              continue;
            }
            // We've checked BasePtr in the current BB.
            if (!TempsToInstrument.insert(BasePtr).second) {
              NC_LOG(errs() << "Skipped " << *BasePtr
                            << " because already checked!\n";);
              TrivialChecks++;
              continue;
            }
          }
        }
        Worklist.push_back(I);
      } else {
        // TODO: memory intrinsics
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
    // insertNullCheck(BasePair.getPointer() ? BasePair.getPointer() : Ptr, B,
    // I);

    insertNullCheck(Ptr, B, I);

    if (AddMemSafety) {
      // -- Add extra memory safety assumption: successful load/store
      //    implies validity of their arguments.
      if (GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(Ptr)) {
        if (gep->isInBounds() && gep->getPointerAddressSpace() == 0) {
          Value *base = gep->getPointerOperand();
          B.SetInsertPoint(I);
          auto It = B.GetInsertPoint();
          ++It;
          B.SetInsertPoint(I->getParent(), It);
          CallInst *CI = B.CreateCall(AssumeFn, B.CreateIsNotNull(base));
          CI->setDebugLoc(I->getDebugLoc());

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
    AttrBuilder B;
    // Function does not access memory
    B.addAttribute(Attribute::ReadNone);
    AttributeList as = AttributeList::get(ctx, AttributeList::FunctionIndex, B);
    AssumeFn = dyn_cast<Function>(M.getOrInsertFunction("__CRAB_assume", as,
                                                        Type::getVoidTy(ctx),
                                                        Type::getInt32Ty(ctx))
                                      .getCallee());
  }

  {
    AttrBuilder B;
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
         << "Considering only Load and Store instructions.\n";

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
