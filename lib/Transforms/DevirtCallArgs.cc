#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "seadsa/Global.hh"
#include "seadsa/Graph.hh"
#include "seadsa/InitializePasses.hh"

#include <vector>

#define SCFPA_LOG(...) __VA_ARGS__
//#define SCFPA_LOG(...)

namespace clam {
/**
 * Transform a call "foo(fptr, ....)" where fptr is a function pointer to:
 *     if (fptr == fn_1) {
 *        foo(fn_1, ...);
 *     } else if (fptr == fn_2) {
 *        foo(fn_2, ...);
 *     }  else {
 *        foo(fn_N, ...);
 *     }
 *
 * where fn_1,fn_2,...,fn_N are the possible function addresses to
 *     which ptr might point to.
 **/
class DevirtCallArgs {
  seadsa::GlobalAnalysis &m_seadsa;
  bool m_onlyExternalCalls;

public:
  DevirtCallArgs(seadsa::GlobalAnalysis &dsa, bool onlyExternalCalls)
      : m_seadsa(dsa), m_onlyExternalCalls(onlyExternalCalls) {}

  bool runOnModule(llvm::Module &M);
};

class DevirtCallArgsPass : public llvm::ModulePass {
public:
  static char ID;
  DevirtCallArgsPass();
  bool runOnModule(llvm::Module &M) override;
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
};

class DevirtExternCallArgsPass : public llvm::ModulePass {
public:
  static char ID;
  DevirtExternCallArgsPass();
  bool runOnModule(llvm::Module &M) override;
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
};

} // namespace clam

namespace clam {

using namespace llvm;
using namespace seadsa;

static bool isFunctionPtrType(const Type *ty) {
  if (const PointerType *PTy = dyn_cast<PointerType>(ty)) {
    return isa<FunctionType>(PTy->getElementType());
  }
  return false;
}

// Copied from llvm CallPromotionUtils
static void fixupPHINodeForNormalDest(InvokeInst *Invoke, BasicBlock *OrigBlock,
                                      BasicBlock *MergeBlock) {
  for (PHINode &Phi : Invoke->getNormalDest()->phis()) {
    int Idx = Phi.getBasicBlockIndex(OrigBlock);
    if (Idx == -1)
      continue;
    Phi.setIncomingBlock(Idx, MergeBlock);
  }
}

// Copied from llvm CallPromotionUtils
static void fixupPHINodeForUnwindDest(InvokeInst *Invoke, BasicBlock *OrigBlock,
                                      BasicBlock *ThenBlock,
                                      BasicBlock *ElseBlock) {
  for (PHINode &Phi : Invoke->getUnwindDest()->phis()) {
    int Idx = Phi.getBasicBlockIndex(OrigBlock);
    if (Idx == -1)
      continue;
    auto *V = Phi.getIncomingValue(Idx);
    Phi.setIncomingBlock(Idx, ThenBlock);
    Phi.addIncoming(V, ElseBlock);
  }
}

// Copied from llvm CallPromotionUtils
static void createRetPHINode(Instruction *OrigInst, Instruction *NewInst,
                             BasicBlock *MergeBlock, IRBuilder<> &Builder) {

  if (OrigInst->getType()->isVoidTy() || OrigInst->use_empty())
    return;

  Builder.SetInsertPoint(&MergeBlock->front());
  PHINode *Phi = Builder.CreatePHI(OrigInst->getType(), 0);
  SmallVector<User *, 16> UsersToUpdate(OrigInst->users());
  for (User *U : UsersToUpdate)
    U->replaceUsesOfWith(OrigInst, Phi);
  Phi->addIncoming(OrigInst, OrigInst->getParent());
  Phi->addIncoming(NewInst, NewInst->getParent());
}

static void specializeCallFunctionPtrArg(CallBase &CB, unsigned ArgNo,
                                         Value *Callee) {
  auto *Arg = CB.getArgOperand(ArgNo);
  if (Arg->getType() != Callee->getType()) {
    Callee = CastInst::CreateBitOrPointerCast(Callee, Arg->getType(), "", &CB);
  }
  CB.setArgOperand(ArgNo, Callee);
}

// Code very similar to CallPromotionUtils::versionCallSite. The
// difference is that we don't split on CB.getCalledOperand() but
// instead on CB.getArgOperand(ArgNo).
static Instruction *versionCallSite(CallBase &CB, unsigned ArgNo, Value *Callee,
                                    MDNode *BranchWeights) {
  IRBuilder<> Builder(&CB);
  Instruction *OrigInst = &CB;
  BasicBlock *OrigBlock = OrigInst->getParent();

  // Create the compare. The CB.getArgOperand(ArgNo) value and callee must
  // have the same type to be compared.
  if (CB.getArgOperand(ArgNo)->getType() != Callee->getType())
    Callee = Builder.CreateBitCast(Callee, CB.getArgOperand(ArgNo)->getType());
  auto *Cond = Builder.CreateICmpEQ(CB.getArgOperand(ArgNo), Callee);

  // Create an if-then-else structure. The original instruction is moved into
  // the "else" block, and a clone of the original instruction is placed in the
  // "then" block.
  Instruction *ThenTerm = nullptr;
  Instruction *ElseTerm = nullptr;
  SplitBlockAndInsertIfThenElse(Cond, &CB, &ThenTerm, &ElseTerm, BranchWeights);
  BasicBlock *ThenBlock = ThenTerm->getParent();
  BasicBlock *ElseBlock = ElseTerm->getParent();
  BasicBlock *MergeBlock = OrigInst->getParent();

  ThenBlock->setName("if.true.specialize_funcptr");
  ElseBlock->setName("if.false.specialize_funcptr");
  MergeBlock->setName("if.end.specialize_funcptr");

  Instruction *NewInst = OrigInst->clone();
  OrigInst->moveBefore(ElseTerm);
  NewInst->insertBefore(ThenTerm);

  // If the original call site is an invoke instruction, we have extra work to
  // do since invoke instructions are terminating. We have to fix-up phi nodes
  // in the invoke's normal and unwind destinations.
  if (auto *OrigInvoke = dyn_cast<InvokeInst>(OrigInst)) {
    auto *NewInvoke = cast<InvokeInst>(NewInst);

    // Invoke instructions are terminating, so we don't need the terminator
    // instructions that were just created.
    ThenTerm->eraseFromParent();
    ElseTerm->eraseFromParent();

    // Branch from the "merge" block to the original normal destination.
    Builder.SetInsertPoint(MergeBlock);
    Builder.CreateBr(OrigInvoke->getNormalDest());

    // Fix-up phi nodes in the original invoke's normal and unwind destinations.
    fixupPHINodeForNormalDest(OrigInvoke, OrigBlock, MergeBlock);
    fixupPHINodeForUnwindDest(OrigInvoke, MergeBlock, ThenBlock, ElseBlock);

    // Now set the normal destinations of the invoke instructions to be the
    // "merge" block.
    OrigInvoke->setNormalDest(MergeBlock);
    NewInvoke->setNormalDest(MergeBlock);
  }

  // Create a phi node for the returned value of the call site.
  createRetPHINode(OrigInst, NewInst, MergeBlock, Builder);

  return NewInst;
}

static void
specializeCallFunctionPtrArg(CallBase &CB, unsigned ArgNo,
                             const SmallVector<Function *, 8> &Callees) {
  for (unsigned i = 0, numCallees = Callees.size(); i < numCallees; ++i) {
    // The last callee does not create an "else" block
    // If there is only one callee we don't create an "else" block either.
    if (i == numCallees - 1) {
      specializeCallFunctionPtrArg(CB, ArgNo, Callees[i]);
    } else {
      Function *ArgV = Callees[i];
      versionCallSite(CB, ArgNo, ArgV, nullptr);
      specializeCallFunctionPtrArg(CB, ArgNo, ArgV);
    }
  }
}

bool DevirtCallArgs::runOnModule(Module &M) {
  // When this pass is called, devirtualization has been already
  // called.

  if (M.empty()) {
    return false;
  }

  SCFPA_LOG(errs() << "=== Begin DevirtCallArgs === \n";)

  // The worklist contains pairs of callsites and the index of
  // the argument whose type is a function pointer.
  std::vector<std::pair<CallBase *, unsigned>> worklist;

  // Add in the worklist any callsite whose parameter is a function
  // pointer.
  for (auto &F : M) {
    for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
      if (CallBase *CB = dyn_cast<CallBase>(&*I)) {
        if (!CB->isIndirectCall()) {
          if (m_onlyExternalCalls) {
            if (Function *calleeF = CB->getCalledFunction()) {
              if (!calleeF->isDeclaration() || calleeF->isIntrinsic()) {
                continue;
              }
            }
          }

          for (unsigned i = 0, num_args = CB->arg_size(); i < num_args; ++i) {
            if (isFunctionPtrType(CB->getArgOperand(i)->getType())) {
              if (!isa<Function>(CB->getArgOperand(i))) {
                SCFPA_LOG(errs() << "[DevirtCallArgs] Added " << *CB
                                 << " to the worklist\n";);
                worklist.push_back({CB, i});
              } else {
                // if already a known function we skip it.
                SCFPA_LOG(errs() << " [DevirtCallArgs] argument " << i << " of "
                                 << *CB << " already known\n";);
              }
            }
          }
        }
      }
    }
  }

  // For stats only
  unsigned numCandidates = worklist.size();
  unsigned numResolved = 0;
  // Process the worklist
  bool Change = false;
  while (!worklist.empty()) {
    auto p = worklist.back();
    worklist.pop_back();
    assert(p.first);
    CallBase &CB = *(p.first);
    Value *ArgV = CB.getArgOperand(p.second);

    // Ask m_seadsa the possible allocation sites associated to the
    // function pointer
    Function &FParent = *(p.first->getParent()->getParent());
    if (m_seadsa.hasGraph(FParent)) {
      seadsa::Graph &G = m_seadsa.getGraph(FParent);
      if (G.hasCell(*ArgV)) {
        const seadsa::Cell &C = G.getCell(*ArgV);
        if (!C.getNode()->isExternal()) {
          SmallVector<Function *, 8> Callees;
          for (const Value *AS : C.getNode()->getAllocSites()) {
            if (const Function *F = dyn_cast<const Function>(AS)) {
              // We need to remove constness
              Callees.push_back(const_cast<Function *>(F));
            }
          }
          SCFPA_LOG(errs() << "[DevirtCallArgs] Callsite= " << CB
                           << ". Replaced argument=" << p.second
                           << " with values={";
                    for (unsigned i = 0, sz = Callees.size(); i < sz;) {
                      errs() << Callees[i]->getName();
                      ++i;
                      if (i < sz) {
                        errs() << ",";
                      }
                    } errs()
                    << "}\n";);
          specializeCallFunctionPtrArg(CB, p.second, Callees);
          numResolved++;
          Change = true;
        }
      }
    }
  }

  SCFPA_LOG(
      errs() << "Number of calls with function pointer parameters: "
             << numCandidates << "\n";
      errs() << "Number of *resolved* calls with function pointer parameters: "
             << numResolved << "\n";
      errs() << "=== End DevirtCallArgs === \n";)
  return Change;
}

/** Begin DevirtCallArgsPass **/
void DevirtCallArgsPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<seadsa::BottomUpTopDownGlobalPass>();
}

bool DevirtCallArgsPass::runOnModule(Module &M) {
  auto &seadsa = getAnalysis<BottomUpTopDownGlobalPass>().getGlobalAnalysis();
  DevirtCallArgs SCFA(seadsa, false /* specialize all calls */);
  return SCFA.runOnModule(M);
}

DevirtCallArgsPass::DevirtCallArgsPass() : ModulePass(ID) {
  auto &Registry = *llvm::PassRegistry::getPassRegistry();
  llvm::initializeDsaAnalysisPass(Registry);
  llvm::initializeCompleteCallGraphPass(Registry);
}

char DevirtCallArgsPass::ID = 0;

/** End DevirtCallArgsPass **/

/** Begin DevirtExternCallArgsPass **/
void DevirtExternCallArgsPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<seadsa::BottomUpTopDownGlobalPass>();
}

bool DevirtExternCallArgsPass::runOnModule(Module &M) {
  auto &seadsa = getAnalysis<BottomUpTopDownGlobalPass>().getGlobalAnalysis();
  DevirtCallArgs SCFA(seadsa, true /* specialize only external calls */);
  return SCFA.runOnModule(M);
}

DevirtExternCallArgsPass::DevirtExternCallArgsPass() : ModulePass(ID) {
  auto &Registry = *llvm::PassRegistry::getPassRegistry();
  llvm::initializeDsaAnalysisPass(Registry);
  llvm::initializeCompleteCallGraphPass(Registry);
}

char DevirtExternCallArgsPass::ID = 0;

/** End DevirtExternCallArgsPass **/

Pass *createDevirtualizeCallArgsPass(bool onlyExternCalls) {
  if (onlyExternCalls) {
    return new DevirtExternCallArgsPass();
  } else {
    return new DevirtCallArgsPass();
  }
}

} // namespace clam
