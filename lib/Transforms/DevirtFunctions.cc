#include "clam/Transforms/DevirtFunctions.hh"
#include "clam/config.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/CallPromotionUtils.h"

#include <algorithm>
#include <set>

using namespace llvm;

//#define DEBUG_DEVIRT

#ifdef DEBUG_DEVIRT
#define DEVIRT_LOG(...) __VA_ARGS__
#define DEVIRT_WARNING(...) __VA_ARGS__
#else
#define DEVIRT_LOG(...)
#define DEVIRT_WARNING(...)
#endif

namespace clam {

static bool isIndirectCall(CallBase &CB) {
  Value *v = CB.getCalledOperand();
  if (!v)
    return false;

  v = v->stripPointerCastsAndAliases();
  return !isa<Function>(v);
}

#ifdef USE_BOUNCE_FUNCTIONS  
static PointerType *getVoidPtrType(LLVMContext &C) {
  Type *Int8Type = IntegerType::getInt8Ty(C);
  return PointerType::getUnqual(Int8Type);
}

static Value *castTo(Value *V, Type *Ty, std::string Name,
                     Instruction *InsertPt) {
  // Don't bother creating a cast if it's already the correct type.
  if (V->getType() == Ty)
    return V;

  // If it's a constant, just create a constant expression.
  if (Constant *C = dyn_cast<Constant>(V)) {
    Constant *CE = ConstantExpr::getZExtOrBitCast(C, Ty);
    return CE;
  }

  // Otherwise, insert a cast instruction.
  return CastInst::CreateZExtOrBitCast(V, Ty, Name, InsertPt);
}
#endif
  
static void removeBlock(BasicBlock *BB, LLVMContext &ctx) {
  auto *BBTerm = BB->getTerminator();
  // Loop through all of our successors and make sure they know that one
  // of their predecessors is going away.
  for (unsigned i = 0, e = BBTerm->getNumSuccessors(); i != e; ++i) {
    BBTerm->getSuccessor(i)->removePredecessor(BB);
  }
  // Zap all the instructions in the block.
  while (!BB->empty()) {
    Instruction &I = BB->back();
    // If this instruction is used, replace uses with an arbitrary value.
    // Because control flow can't get here, we don't care what we replace the
    // value with.  Note that since this block is unreachable, and all values
    // contained within it must dominate their uses, that all uses will
    // eventually be removed (they are themselves dead).
    if (!I.use_empty()) {
      I.replaceAllUsesWith(UndefValue::get(I.getType()));
    }
    BB->getInstList().pop_back();
  }
  // Add unreachable terminator
  BB->getInstList().push_back(new UnreachableInst(ctx));
}   
///
/// Create a sequence of if-then-else statements at the location of
/// the callsite.  The "if" condition compares the callsite's called
/// value with a function f from Callees.  The direct call to f is
/// moved to the "then" block. The "else" block contains the next
/// "if". For the last callsite's called value we don't create an
/// "else" block if keepOriginalCallSite is false. Otherwise, the last
/// "else" block contains the original call site.
///
/// For example, the call instruction below:
///
///   orig_bb:
///     %t0 = call i32 %ptr()  with callees = {foo, bar}
///     ...
///
/// Is replaced by the following:
///
///   orig_bb:
///     %cond = icmp eq i32 ()* %ptr, @foo
///     br i1 %cond, %then_bb, %else_bb
///
///   then_bb:
///     %t1 = call i32 @foo()
///     br merge_bb
///
///   else_bb:
///     %t0 = call i32 %bar()
///     br merge_bb
///
///   merge_bb:
///     ; Uses of the original call instruction are replaced by uses of the phi
///     ; node.
///     %t2 = phi i32 [ %t0, %else_bb ], [ %t1, %then_bb ]
///     ...
///
static void promoteIndirectCall(CallBase &CB,
                                const std::vector<Function *> &Callees,
                                bool keepOriginal) {
#if 0
  for (unsigned i = 0, numCallees = Callees.size(); i < numCallees; ++i) {
    if (i == numCallees - 1 && !keepOriginal) {
      llvm::promoteCall(CB, Callees[i]);
    } else {
      llvm::promoteCallWithIfThenElse(CB, Callees[i]);
    }
  }
#else  
  for (unsigned i = 0, numCallees = Callees.size(); i < numCallees; ++i) {
    llvm::promoteCallWithIfThenElse(CB, Callees[i]);
  }
  // We create an "else" block with the original call and then remove
  // the block. This seems unnecessary but this avoids having as the
  // "else" block the last candidate callee.
  //                                                      
  // We want each possible call be guarded by an explicit comparison
  // instruction. This can allow other transformations to optimize
  // code if something is known about the comparison operands.
  removeBlock(CB.getParent(),
              CB.getParent()->getParent()->getContext());
#endif   
}

namespace devirt_impl {
AliasSetId typeAliasId(CallBase &CB, bool LookThroughCast) {
  assert(isIndirectCall(CB) && "Not an indirect call");
  PointerType *pTy = nullptr;

  if (LookThroughCast) {
    /*
        %390 = load void (i8*, i32*, i32*, i64, i32)*,
                          void (i8*, i32*, i32*, i64, i32)**
                          bitcast (i64 (i8*, i32*, i32*, i64, i32)** @listdir to
                                   void (i8*, i32*, i32*, i64, i32)**)
        call void %390(i8* %385, i32* %1, i32* %2, i64 %139, i32 %26)
    */
    if (LoadInst *LI = dyn_cast<LoadInst>(CB.getCalledOperand())) {
      if (Constant *C = dyn_cast<Constant>(LI->getPointerOperand())) {
        if (ConstantExpr *CE = dyn_cast<ConstantExpr>(C)) {
          if (CE->getOpcode() == Instruction::BitCast) {
            if (PointerType *ppTy =
                    dyn_cast<PointerType>(CE->getOperand(0)->getType())) {
              pTy = dyn_cast<PointerType>(ppTy->getPointerElementType());
              if (pTy) {
                assert(
                    isa<FunctionType>(pTy->getPointerElementType()) &&
                    "The type of called value is not a pointer to a function");
              }
            }
          }
        }
      }
    }
  }

  if (pTy) {
    return pTy;
  }

  pTy = dyn_cast<PointerType>(CB.getCalledOperand()->getType());
  assert(pTy && "Unexpected call not through a pointer");
  assert(isa<FunctionType>(pTy->getPointerElementType()) &&
         "The type of called value is not a pointer to a function");
  return pTy;
}

AliasSetId typeAliasId(const Function &F) {
  return F.getFunctionType()->getPointerTo();
}
} // namespace devirt_impl

/***
 * Begin specific callsites resolvers
 ***/

CallSiteResolverByTypes::CallSiteResolverByTypes(Module &M, DevirtStats &stats)
    : CallSiteResolver(RESOLVER_TYPES, stats), m_M(M) {
  populateTypeAliasSets();
}

CallSiteResolverByTypes::~CallSiteResolverByTypes() = default;

void CallSiteResolverByTypes::populateTypeAliasSets() {
  // -- Create type-based alias sets
  for (auto const &F : m_M) {
    // -- intrinsics are never called indirectly
    if (F.isIntrinsic())
      continue;

    // -- local functions whose address is not taken cannot be
    // -- resolved by a function pointer
    if (F.hasLocalLinkage() && !F.hasAddressTaken())
      continue;

    // -- skip calls to declarations, these are resolved implicitly
    // -- by calling through the function pointer argument in the
    // -- default case of bounce function
    if (F.isDeclaration())
      continue;

    // -- skip seahorn and verifier specific intrinsics
    if (F.getName().startswith("seahorn."))
      continue;
    if (F.getName().startswith("verifier."))
      continue;
    // -- assume entry point is never called indirectly
    if (F.getName().equals("main"))
      continue;

    // -- add F to its corresponding alias set (keep sorted the Targets)
    AliasSet &Targets = m_targets_map[devirt_impl::typeAliasId(F)];
    // XXX: ordered by pointer addresses. Ideally we should use
    // something more deterministic.
    auto it = std::upper_bound(Targets.begin(), Targets.end(), &F);
    Targets.insert(it, &F);
  }
}

const CallSiteResolverByTypes::AliasSet *
CallSiteResolverByTypes::getTargets(CallBase &CB) {
  AliasSetId id = devirt_impl::typeAliasId(CB, true);
  auto it = m_targets_map.find(id);
  if (it != m_targets_map.end()) {
    return &(it->second);
  }
  return nullptr;
}

#ifdef USE_BOUNCE_FUNCTIONS
Function *CallSiteResolverByTypes::getBounceFunction(CallBase &CB) {
  AliasSetId id = devirt_impl::typeAliasId(CB, false);
  auto it = m_bounce_map.find(id);
  if (it != m_bounce_map.end()) {
    return it->second;
  } else {
    return nullptr;
  }
}

void CallSiteResolverByTypes::cacheBounceFunction(CallBase &CB,
                                                  Function *bounce) {
  AliasSetId id = devirt_impl::typeAliasId(CB, false);
  m_bounce_map.insert({id, bounce});
}
#endif

template <typename Dsa>
CallSiteResolverByDsa<Dsa>::CallSiteResolverByDsa(Module &M, Dsa &dsa,
                                                  bool incomplete,
                                                  unsigned max_num_targets,
                                                  DevirtStats &stats)
    : CallSiteResolverByTypes(M, stats), m_M(M), m_dsa(dsa),
      m_allow_incomplete(incomplete), m_max_num_targets(max_num_targets) {

  CallSiteResolver::m_kind = RESOLVER_SEA_DSA;
  
  /*
    Assume that Dsa provides these methods:
     - bool isComplete(CallBase&)
     - iterator begin(CallBase&)
     - iterator end(CallBase&)
     where each element of iterator is of type Function*
  */

  // build the target map
  for (auto &F : m_M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        Instruction *CI = nullptr;
        if (isa<CallInst>(&I)) {
          CI = &I;
        }
        if (!CI && isa<InvokeInst>(&I)) {
          CI = &I;
        }
        if (CI) {
	  CallBase &CB = *dyn_cast<CallBase>(CI);	  
          if (isIndirectCall(CB)) {
            if (m_allow_incomplete || m_dsa.isComplete(CB)) {
              AliasSet dsa_targets;
              dsa_targets.append(m_dsa.begin(CB), m_dsa.end(CB));
              if (dsa_targets.empty()) {
                m_stats.m_num_unresolved++;
                DEVIRT_WARNING(
                    errs()
                        << "WARNING Devirt (dsa): does not have any target for "
                        << *CI << "\n";);
                continue;
              }
	      
              // sort dsa_targets
              // XXX: ordered by pointer addresses. Ideally we should use
              // something more deterministic.
              std::sort(dsa_targets.begin(), dsa_targets.end());

	      if (dsa_targets.size() <= m_max_num_targets) {
		m_targets_map.insert({CI, dsa_targets});
		DEVIRT_LOG(errs() << "Devirt (dsa): resolved the " << *CI
			         << " with targets: ";
			   for (auto F : dsa_targets) {
			     errs() << "\t" << F->getName()
				    << "::" << *(F->getType()) << "\n";
			   });
		
	      } else {
		m_stats.m_num_unresolved++;
		DEVIRT_WARNING(
                        errs()
                            << "WARNING Devirt (dsa): unresolve "
                            << *CI
                            << " because the number of targets is greater than "
                            << m_max_num_targets << "\n";);
	      }
	    } else {
              m_stats.m_num_unresolved++;
              DEVIRT_WARNING(errs() << "WARNING Devirt (dsa): cannot resolve "
                                    << *CI
                                    << " because the corresponding dsa node is "
                                       "not complete\n";);
            }
          }
        }
      }
    }
  }
}

template <typename Dsa> CallSiteResolverByDsa<Dsa>::~CallSiteResolverByDsa() {
  m_targets_map.clear();
#ifdef USE_BOUNCE_FUNCTIONS
  m_bounce_map.clear();
#endif
}

template <typename Dsa>
const typename CallSiteResolverByDsa<Dsa>::AliasSet *
CallSiteResolverByDsa<Dsa>::getTargets(CallBase &CB) {
  auto it = m_targets_map.find(&CB);
  if (it != m_targets_map.end()) {
    return &(it->second);
  }
  return nullptr;
}

#ifdef USE_BOUNCE_FUNCTIONS
template <typename Dsa>
Function *CallSiteResolverByDsa<Dsa>::getBounceFunction(CallBase &CB) {
  AliasSetId id = devirt_impl::typeAliasId(CB, false);
  auto it = m_bounce_map.find(id);
  if (it != m_bounce_map.end()) {
    const AliasSet *cachedTargets = it->second.first;
    const AliasSet *Targets = getTargets(CB);
    if (cachedTargets && Targets) {
      if (std::equal(cachedTargets->begin(), cachedTargets->end(),
                     Targets->begin())) {
        return it->second.second;
      }
    }
  }
  return nullptr;
}

template <typename Dsa>
void CallSiteResolverByDsa<Dsa>::cacheBounceFunction(CallBase &CB,
                                                     Function *bounce) {
  if (const AliasSet *targets = getTargets(CB)) {
    AliasSetId id = devirt_impl::typeAliasId(CB, false);
    m_bounce_map.insert({id, {targets, bounce}});
  }
}
#endif

/***
 * End specific callsites resolver
 ***/

DevirtualizeFunctions::DevirtualizeFunctions(llvm::CallGraph * /*cg*/,
                                             bool allowIndirectCalls)
    : // m_cg(nullptr)
      m_allowIndirectCalls(allowIndirectCalls) {}

DevirtualizeFunctions::~DevirtualizeFunctions() { m_stats.dump(); }

#ifdef USE_BOUNCE_FUNCTIONS
/**
 * Creates a bounce function that calls functions in an alias set directly
 * All the work happens here.
 */
Function *DevirtualizeFunctions::mkBounceFn(CallBase &CB,
                                            CallSiteResolver *CSR) {
  assert(isIndirectCall(CB) && "Not an indirect call");

  // We don't create a bounce function if the function has a
  // variable number of arguments.
  if (CB.getFunctionType()->isVarArg()) {
    return nullptr;
  }

  if (Function *bounce = CSR->getBounceFunction(CB)) {
    DEVIRT_LOG(errs() << "Reusing bounce function for "
                      << CB << "\n\t" << bounce->getName()
                      << "::" << *(bounce->getType()) << "\n";);
    return bounce;
  }

  const AliasSet *Targets = CSR->getTargets(CB);
  if (!Targets || Targets->empty()) {
    return nullptr;
  }

  DEVIRT_LOG(errs() << CB << "\n";
             errs() << "Possible targets:\n"; for (const Function *F
                                                   : *Targets) {
               errs() << "\t" << F->getName() << ":: " << *(F->getType())
                      << "\n";
             })

  // Create a bounce function that has a function signature almost
  // identical to the function being called.  The only difference is
  // that it will have an additional pointer argument at the
  // beginning of its argument list that will be the function to
  // call.
  Value *ptr = CB.getCalledOperand();
  SmallVector<Type *, 8> TP;
  TP.push_back(ptr->getType());
  for (auto i = CB.arg_begin(), e = CB.arg_end(); i != e; ++i)
    TP.push_back((*i)->getType());

  FunctionType *NewTy = FunctionType::get(CB.getType(), TP, false);
  Module *M = CB.getParent()->getParent()->getParent();
  assert(M);
  Function *F = Function::Create(NewTy, GlobalValue::InternalLinkage,
                                 "seahorn.bounce", M);

  // Set the names of the arguments.  Also, record the arguments in a vector
  // for subsequence access.
  F->arg_begin()->setName("funcPtr");
  SmallVector<Value *, 8> fargs;
  auto ai = F->arg_begin();
  ++ai;
  for (auto ae = F->arg_end(); ai != ae; ++ai) {
    fargs.push_back(&*ai);
    ai->setName("arg");
  }

  // Create an entry basic block for the function.  All it should do is perform
  // some cast instructions and branch to the first comparison basic block.
  BasicBlock *entryBB = BasicBlock::Create(M->getContext(), "entry", F);

  // For each function target, create a basic block that will call that
  // function directly.
  DenseMap<const Function *, BasicBlock *> targets;
  for (const Function *FL : *Targets) {
    // Create the basic block for doing the direct call
    BasicBlock *BL = BasicBlock::Create(M->getContext(), FL->getName(), F);
    targets[FL] = BL;
    // Create the direct function call
    CallingConv::ID cc = FL->getCallingConv();
    CallInst *directCall =
        CallInst::Create(const_cast<Function *>(FL), fargs, "", BL);
    directCall->setCallingConv(cc);
    // TODO: update call graph
    // if (m_cg) {
    //   auto fl_cg = m_cg->getOrInsertFunction (const_cast<Function*> (FL));
    //   auto cf_cg = m_cg->getOrInsertFunction (directCall->getCalledFunction
    //   ()); fl_cg->addCalledFunction (CallSite (directCall), cf_cg);
    // }

    // Add the return instruction for the basic block
    if (CB.getType()->isVoidTy())
      ReturnInst::Create(M->getContext(), BL);
    else
      ReturnInst::Create(M->getContext(), directCall, BL);
  }

  BasicBlock *defaultBB = nullptr;
  if (m_allowIndirectCalls) {
    // Create a default basic block having the original indirect call
    defaultBB = BasicBlock::Create(M->getContext(), "default", F);
    if (CB.getType()->isVoidTy()) {
      ReturnInst::Create(M->getContext(), defaultBB);
    } else {
      CallInst *defaultRet =
          CallInst::Create(&*(F->arg_begin()), fargs, "", defaultBB);
      ReturnInst::Create(M->getContext(), defaultRet, defaultBB);
    }
  } else {
    // Create a failure basic block.  This basic block should simply be an
    // unreachable instruction.
    defaultBB = BasicBlock::Create(M->getContext(), "fail", F);
    new UnreachableInst(M->getContext(), defaultBB);
  }

  // Setup the entry basic block.  For now, just have it call the default
  // basic block.  We'll change the basic block to which it branches later.
  BranchInst *InsertPt = BranchInst::Create(defaultBB, entryBB);

  // Create basic blocks which will test the value of the incoming function
  // pointer and branch to the appropriate basic block to call the function.
  Type *VoidPtrType = getVoidPtrType(M->getContext());
  Value *FArg = castTo(&*(F->arg_begin()), VoidPtrType, "", InsertPt);
  BasicBlock *tailBB = defaultBB;
  for (const Function *FL : *Targets) {
    // Cast the function pointer to an integer.  This can go in the entry
    // block.
    Value *TargetInt =
        castTo(const_cast<Function *>(FL), VoidPtrType, "", InsertPt);

    // Create a new basic block that compares the function pointer to the
    // function target.  If the function pointer matches, we'll branch to the
    // basic block performing the direct call for that function; otherwise,
    // we'll branch to the next function call target.
    BasicBlock *TB = targets[FL];
    BasicBlock *newB =
        BasicBlock::Create(M->getContext(), "test." + FL->getName(), F);
    CmpInst *setcc = CmpInst::Create(Instruction::ICmp, CmpInst::ICMP_EQ,
                                     TargetInt, FArg, "sc", newB);
    BranchInst::Create(TB, tailBB, setcc, newB);

    // Make this newly created basic block the next block that will be reached
    // when the next comparison will need to be done.
    tailBB = newB;
  }

  // Make the entry basic block branch to the first comparison basic block.
  InsertPt->setSuccessor(0, tailBB);

  // -- cache the newly created function
  CSR->cacheBounceFunction(CB, F);

  // Return the newly created bounce function.
  return F;
}
#endif

void DevirtualizeFunctions::mkDirectCall(CallBase &CB, CallSiteResolver *CSR) {
  m_stats.m_num_indirect_calls++;
#ifndef USE_BOUNCE_FUNCTIONS
  const AliasSet *Targets = CSR->getTargets(CB);
  if (!Targets || Targets->empty()) {
    // cannot resolve the indirect call
    return;
  }
  m_stats.m_num_resolved_calls++;
  
  // HACK: remove constness
  std::vector<Function *> Callees;
  Callees.resize(Targets->size());
  std::transform(Targets->begin(), Targets->end(), Callees.begin(),
                 [](const Function *fn) { return const_cast<Function *>(fn); });
  // promote indirect call to a bunch of direct calls
  promoteIndirectCall(CB, Callees, m_allowIndirectCalls);
#else
  const Function *bounceFn = mkBounceFn(CB, CSR);
  // -- something failed
  if (!bounceFn)
    return;

  m_stats.m_num_resolved_calls++;

  DEVIRT_LOG(errs() << "Callsite: " << CB << "\n";
             errs() << "Bounce function: " << bounceFn->getName()
                    << ":: " << *(bounceFn->getType()) << "\n";)

  // Replace the original call with a call to the bounce function.
  if (CallInst *CI = dyn_cast<CallInst>(&CB)) {
    // The last operand in the op list is the callee
    SmallVector<Value *, 8> Params;
    Params.reserve(std::distance(CI->op_begin(), CI->op_end()));
    Params.push_back(*(CI->op_end() - 1));
    Params.insert(Params.end(), CI->op_begin(), (CI->op_end() - 1));
    std::string name = CI->hasName() ? CI->getName().str() + ".dv" : "";
    CallInst *CN =
        CallInst::Create(const_cast<Function *>(bounceFn), Params, name, CI);
    // TODO: update call graph
    // if (m_cg) {
    //   m_cg->getOrInsertFunction (const_cast<Function*> (bounceFn));
    //   (*m_cg)[CI->getParent ()->getParent ()]->addCalledFunction
    //     (CallSite (CN), (*m_cg)[CN->getCalledFunction ()]);
    // }

    CN->setDebugLoc(CI->getDebugLoc());
    CI->replaceAllUsesWith(CN);
    CI->eraseFromParent();
  } else if (InvokeInst *CI = dyn_cast<InvokeInst>(&CB)) {
    SmallVector<Value *, 8> Params;
    Params.reserve(
        std::distance(CI->arg_operands().begin(), CI->arg_operands().end()));
    // insert first the callee
    Params.push_back(CI->getCalledOperand());
    Params.insert(Params.end(), CI->arg_operands().begin(),
                  CI->arg_operands().end());

    std::string name = CI->hasName() ? CI->getName().str() + ".dv" : "";
    InvokeInst *CN = InvokeInst::Create(const_cast<Function *>(bounceFn),
                                        CI->getNormalDest(),
                                        CI->getUnwindDest(), Params, name, CI);

    // TODO: update call graph
    // if (m_cg) {
    //   m_cg->getOrInsertFunction (const_cast<Function*> (bounceFn));
    //   (*m_cg)[CI->getParent ()->getParent ()]->addCalledFunction
    //     (CallSite (CN), (*m_cg)[CN->getCalledFunction ()]);
    // }

    CN->setDebugLoc(CI->getDebugLoc());
    CI->replaceAllUsesWith(CN);
    CI->eraseFromParent();
  }
#endif
}

void DevirtualizeFunctions::visitCallBase(CallBase &CB) {
  // -- skip direct calls
  if (!isIndirectCall(CB))
    return;

  // This is an indirect call site.  Put it in the worklist of call
  // sites to transforms.
  m_worklist.push_back(&CB);
  return;
}

void DevirtualizeFunctions::visitCallInst(CallInst &CI) {
  // we cannot take the address of an inline asm
  if (CI.isInlineAsm()){
    m_stats.m_num_asm_calls++;
    return;
  }
  visitCallBase(CI);
}

void DevirtualizeFunctions::visitInvokeInst(InvokeInst &II) {
  visitCallBase(II);
}

bool DevirtualizeFunctions::resolveCallSites(Module &M, CallSiteResolver *CSR) {
  // -- Visit all of the call instructions in this function and
  // -- record those that are indirect function calls.
  visit(M);

  // -- Now go through and transform all of the indirect calls that
  // -- we found that need transforming.
  bool Changed = !m_worklist.empty();
  while (!m_worklist.empty()) {
    auto* w = m_worklist.back();
    m_worklist.pop_back();
    CallBase &CB = *dyn_cast<CallBase>(w);
    mkDirectCall(CB, CSR);
  }
  // -- Conservatively assume that we've changed one or more call
  // -- sites.
  return Changed;
}

void DevirtStats::dump() const {
  errs() << "=== Resolution of indirect calls ===\n";
  errs() << "BRUNCH_STAT INDIRECT CALLS "    << m_num_indirect_calls << "\n";
  errs() << "BRUNCH_STAT RESOLVED CALLS "    << m_num_resolved_calls << "\n";
  errs() << "BRUNCH_STAT UNRESOLVED CALLS "  << m_num_unresolved - m_num_asm_calls << "\n";
  errs() << "BRUNCH_STAT IGNORED ASM CALLS " << m_num_asm_calls << "\n";
}
} // namespace clam

/* Template instantiations */
// sea-dsa
#include "seadsa/CompleteCallGraph.hh"
namespace clam {
template class CallSiteResolverByDsa<seadsa::CompleteCallGraph>;
} // namespace clam
