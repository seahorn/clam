/** Insert dummy main function if one does not exist */

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

static llvm::cl::opt<std::string>
    EntryPoint("entry-point",
               llvm::cl::desc("Entry point if main does not exist"),
               llvm::cl::init(""));

namespace clam {

class InsertEntryPoint : public ModulePass {
  DenseMap<const Type *, FunctionCallee> m_ndfn;

  FunctionCallee getNondetFn(Type *type, Module &M) {
    auto it = m_ndfn.find(type);
    if (it != m_ndfn.end()) {
      return it->second;
    }
    
    FunctionCallee res =
      M.getOrInsertFunction("verifier.nondet." + std::to_string(m_ndfn.size()), type);
    // -- say that f does not access memory will make llvm
    // -- assume that all calls to it return the same value
    // if (Function *f = dyn_cast<Function>(res))
    // {
    //   // f->setDoesNotAccessMemory (true);
    //   // f->setDoesNotAlias (0);
    // }    
    m_ndfn[type] = res;
    return res;
  }

public:
  static char ID;

  InsertEntryPoint() : ModulePass(ID) {}

  virtual bool runOnModule(Module &M) override {

    if (M.getFunction("main")) {
      return false;
    }

    Function *Entry = nullptr;
    if (EntryPoint != "") {
      Entry = M.getFunction(EntryPoint);
    }

    if (!Entry) {
      return false;
    }

    // --- Create main
    LLVMContext &ctx = M.getContext();
    Type *intTy = Type::getInt32Ty(ctx);

    ArrayRef<Type *> params;
    Function *main = Function::Create(
        FunctionType::get(intTy, params, false),
        GlobalValue::LinkageTypes::ExternalLinkage, "main", &M);

    IRBuilder<> B(ctx);
    BasicBlock *BB = BasicBlock::Create(ctx, "", main);
    B.SetInsertPoint(BB, BB->begin());

    // -- create a call with non-deterministic actual parameters
    SmallVector<Value *, 16> Args;
    for (auto &A : Entry->args()) {
      FunctionCallee ndf = getNondetFn(A.getType(), M);
      Args.push_back(B.CreateCall(ndf));
    }
    B.CreateCall(Entry, Args);

    // -- return of main
    // our favourite exit code
    B.CreateRet(ConstantInt::get(intTy, 42));
    return true;
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    // AU.setPreservesAll ();
  }

  virtual StringRef getPassName() const override {
    return "Clam: insert an entry point if main does not exist";
  }
};

char InsertEntryPoint::ID = 0;
Pass *createInsertEntryPointPass() { return new InsertEntryPoint(); }
} // end namespace clam
