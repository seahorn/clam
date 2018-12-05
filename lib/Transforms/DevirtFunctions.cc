#include "crab_llvm/Transforms/DevirtFunctions.hh"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

#include<set>
#include<algorithm>

using namespace llvm;

namespace crab_llvm {

  static bool isIndirectCall(CallSite &CS) {
    Value *v = CS.getCalledValue ();
    if (!v) return false;
    
    v = v->stripPointerCasts ();
    return !isa<Function> (v);
  }

  static PointerType * getVoidPtrType(LLVMContext & C) {
    Type * Int8Type  = IntegerType::getInt8Ty(C);
    return PointerType::getUnqual(Int8Type);
  }

  static Value * castTo(Value * V, Type * Ty, std::string Name, Instruction * InsertPt) {
    // Don't bother creating a cast if it's already the correct type.
    if (V->getType() == Ty) return V;
    
    // If it's a constant, just create a constant expression.
    if (Constant * C = dyn_cast<Constant>(V))
    {
      Constant * CE = ConstantExpr::getZExtOrBitCast (C, Ty);
      return CE;
    }
    
    // Otherwise, insert a cast instruction.
    return CastInst::CreateZExtOrBitCast (V, Ty, Name, InsertPt);
  }
  
  Function* DevirtualizeFunctions::mkBounceFn(CallSite &CS, CallSiteResolver* CSR) {
    assert (isIndirectCall (CS) && "Not an indirect call");
    
    AliasSetId id = typeAliasId (CS);
    if (!CSR->useAliasing()) {
      // -- If we just use types then we can reuse the same bounce
      // -- function to avoid many duplicates.
      // 
      // -- If we use Dsa we don't want to use the same bounce for the
      // -- same callee's type since the targets returned by Dsa can
      // -- be different from one callsite to another.
      auto it = m_bounceMap.find (id);
      if (it != m_bounceMap.end ()) return it->second;
    }
    
    // -- no direct calls in this alias set, nothing to construct
    if (m_typeAliasSets.count (id) <= 0) return nullptr;

    // -- the final targets to build the bounce function
    AliasSet Targets;
    // -- all possible candidate targets whose type signatures matche
    AliasSet& TypesTargets = m_typeAliasSets [id];
    // -- targets provided by an external pointer analysis (optional)

    if (CSR->hasTargets(CS.getInstruction())) {
      // --- We filter out those targets whose signature do not match.
      AliasSet& AliasTargets = CSR->getTargets(CS.getInstruction());
      std::sort(TypesTargets.begin(), TypesTargets.end());
      std::sort(AliasTargets.begin(), AliasTargets.end());
      std::set_intersection(AliasTargets.begin(), AliasTargets.end(),
			    TypesTargets.begin(), TypesTargets.end(),
			    std::back_inserter(Targets));
    } else {
      // -- We did not have aliasing information so we just use types
      Targets = std::move(TypesTargets);
    }

    if (Targets.empty()) {
      // -- it's possible to be here if we had aliasing information
      // -- but it was not type consistent. If here, we won't able to
      // -- resolve the indirect call.
      return nullptr;
    }

    // Create a bounce function that has a function signature almost
    // identical to the function being called.  The only difference is
    // that it will have an additional pointer argument at the
    // beginning of its argument list that will be the function to
    // call.
    Value* ptr = CS.getCalledValue();
    SmallVector<Type*, 8> TP;
    TP.push_back (ptr->getType ());
    for (auto i = CS.arg_begin(), e = CS.arg_end (); i != e; ++i) 
      TP.push_back ((*i)->getType());
    
    FunctionType* NewTy = FunctionType::get (CS.getType(), TP, false);
    Module * M = CS.getInstruction()->getParent()->getParent()->getParent();
    assert (M);
    Function* F = Function::Create (NewTy,
                                    GlobalValue::InternalLinkage,
                                    "seahorn.bounce",
                                    M);
    
    // Set the names of the arguments.  Also, record the arguments in a vector
    // for subsequence access.
    F->arg_begin()->setName("funcPtr");
    SmallVector<Value*, 8> fargs;
    for(auto ai = ++F->arg_begin(), ae = F->arg_end(); ai != ae; ++ai) {
      fargs.push_back(&*ai);
      ai->setName("arg");
    }
          
    // Create an entry basic block for the function.  All it should do is perform
    // some cast instructions and branch to the first comparison basic block.
    BasicBlock* entryBB = BasicBlock::Create (M->getContext(), "entry", F);
    
    // For each function target, create a basic block that will call that
    // function directly.
    DenseMap<const Function*, BasicBlock*> targets;
    for (const Function *FL : Targets) {
      // Create the basic block for doing the direct call
      BasicBlock* BL = BasicBlock::Create (M->getContext(), FL->getName(), F);
      targets[FL] = BL;
      // Create the direct function call
      CallInst* directCall = CallInst::Create (const_cast<Function*>(FL),
                                               fargs, "", BL);
      // update call graph
      if (m_cg) {
        auto fl_cg = m_cg->getOrInsertFunction (const_cast<Function*> (FL));
        auto cf_cg = m_cg->getOrInsertFunction (directCall->getCalledFunction ());
        fl_cg->addCalledFunction (CallSite (directCall), cf_cg);
      }
      
      // Add the return instruction for the basic block
      if (CS.getType()->isVoidTy())
        ReturnInst::Create (M->getContext(), BL);
      else
        ReturnInst::Create (M->getContext(), directCall, BL);
    }



    BasicBlock * defaultBB = nullptr;
    if (m_allowIndirectCalls) {
      // Create a default basic block having the original indirect call
      defaultBB = BasicBlock::Create (M->getContext(), "default", F);
      if (CS.getType()->isVoidTy()) {
	ReturnInst::Create (M->getContext(), defaultBB);
      } else {
	CallInst *defaultRet = CallInst::Create(&*(F->arg_begin()), fargs, "", defaultBB);
	ReturnInst::Create (M->getContext(), defaultRet, defaultBB);
      }
    } else {
      // Create a failure basic block.  This basic block should simply be an
      // unreachable instruction.
      defaultBB = BasicBlock::Create (M->getContext(), "fail", F);
      new UnreachableInst (M->getContext(), defaultBB);
    }
                             
    // Setup the entry basic block.  For now, just have it call the default
    // basic block.  We'll change the basic block to which it branches later.
    BranchInst * InsertPt = BranchInst::Create (defaultBB, entryBB);
    
    // Create basic blocks which will test the value of the incoming function
    // pointer and branch to the appropriate basic block to call the function.
    Type * VoidPtrType = getVoidPtrType (M->getContext());
    Value * FArg = castTo (&*(F->arg_begin()), VoidPtrType, "", InsertPt);
    BasicBlock * tailBB = defaultBB;
    for (const Function *FL : Targets)
    {
      
      // Cast the function pointer to an integer.  This can go in the entry
      // block.
      Value * TargetInt = castTo (const_cast<Function*>(FL),
                                  VoidPtrType,
                                  "",
                                  InsertPt);
      
      // Create a new basic block that compares the function pointer to the
      // function target.  If the function pointer matches, we'll branch to the
      // basic block performing the direct call for that function; otherwise,
      // we'll branch to the next function call target.
      BasicBlock* TB = targets [FL];
      BasicBlock* newB = BasicBlock::Create (M->getContext(),
                                             "test." + FL->getName(),
                                             F);
      CmpInst * setcc = CmpInst::Create (Instruction::ICmp,
                                         CmpInst::ICMP_EQ,
                                         TargetInt,
                                         FArg,
                                         "sc",
                                         newB);
      BranchInst::Create (TB, tailBB, setcc, newB);
      
      // Make this newly created basic block the next block that will be reached
      // when the next comparison will need to be done.
      tailBB = newB;
    }
    
    // Make the entry basic block branch to the first comparison basic block.
    InsertPt->setSuccessor(0, tailBB);

    // -- log the newly created function
    m_bounceMap.insert (std::make_pair (id, F));
    
    // Return the newly created bounce function.
    return F;
  }


  void DevirtualizeFunctions::mkDirectCall(CallSite CS, CallSiteResolver* CSR) {
    const Function *bounceFn = mkBounceFn(CS, CSR);
    // -- something failed
    if (!bounceFn) return;
    
    // Replace the original call with a call to the bounce function.
    if (CallInst* CI = dyn_cast<CallInst>(CS.getInstruction())) {
      // The last operand in the op list is the callee
      SmallVector<Value*, 8> Params;
      Params.reserve (std::distance (CI->op_begin(), CI->op_end()));
      Params.push_back (*(CI->op_end () - 1));
      Params.insert (Params.end (), CI->op_begin(), (CI->op_end() - 1));
      std::string name = CI->hasName() ? CI->getName().str() + ".dv" : "";
      CallInst* CN = CallInst::Create (const_cast<Function*>(bounceFn),
                                       Params,
                                       name,
                                       CI);

      // update call graph
      if (m_cg) {
        m_cg->getOrInsertFunction (const_cast<Function*> (bounceFn));
        (*m_cg)[CI->getParent ()->getParent ()]->addCalledFunction
          (CallSite (CN), (*m_cg)[CN->getCalledFunction ()]);
      }

      CN->setDebugLoc (CI->getDebugLoc ());
      CI->replaceAllUsesWith(CN);
      CI->eraseFromParent();
    }
    else if (InvokeInst* CI = dyn_cast<InvokeInst>(CS.getInstruction())) {
      SmallVector<Value*, 8> Params;
      Params.reserve (std::distance (CI->arg_operands().begin (),
                                     CI->arg_operands().end ()));
      // insert first the callee 
      Params.push_back (CI->getCalledValue ());
      Params.insert (Params.end (), 
                     CI->arg_operands().begin (), 
                     CI->arg_operands().end());

      std::string name = CI->hasName() ? CI->getName().str() + ".dv" : "";
      InvokeInst* CN = InvokeInst::Create (const_cast<Function*>(bounceFn),
                                           CI->getNormalDest(),
                                           CI->getUnwindDest(),
                                           Params,
                                           name,
                                           CI);

      // update call graph
      if (m_cg) {
        m_cg->getOrInsertFunction (const_cast<Function*> (bounceFn));
        (*m_cg)[CI->getParent ()->getParent ()]->addCalledFunction
          (CallSite (CN), (*m_cg)[CN->getCalledFunction ()]);
      }

      CN->setDebugLoc (CI->getDebugLoc ());
      CI->replaceAllUsesWith(CN);
      CI->eraseFromParent();
    }
    return;
  }
  
  void DevirtualizeFunctions::visitCallSite (CallSite &CS) {
    // -- skip direct calls
    if (!isIndirectCall (CS)) return;
    
    // This is an indirect call site.  Put it in the worklist of call
    // sites to transforms.
    m_worklist.push_back (CS.getInstruction());
    return;
  }

  void DevirtualizeFunctions::visitCallInst(CallInst &CI) {
    // we cannot take the address of an inline asm
    if (CI.isInlineAsm ()) return;
    
    CallSite CS(&CI);
    visitCallSite(CS);
  }
  
  void DevirtualizeFunctions::visitInvokeInst(InvokeInst &II) {
    CallSite CS(&II);
    visitCallSite(CS);
  }

  DevirtualizeFunctions::AliasSetId DevirtualizeFunctions::typeAliasId (CallSite &CS) {
    assert (isIndirectCall (CS) && "Not an indirect call");
    PointerType *pTy = dyn_cast<PointerType> (CS.getCalledValue ()->getType ());
    assert (pTy && "Unexpected call not through a pointer");
    assert (isa<FunctionType> (pTy->getElementType ())
	    && "The type of called value is not a pointer to a function");
    return pTy;
  }
  
  /// returns an id of an alias set to which this function belongs
  DevirtualizeFunctions::AliasSetId DevirtualizeFunctions::typeAliasId(const Function &F) {
    return F.getFunctionType ()->getPointerTo ();
  }

  DevirtualizeFunctions::DevirtualizeFunctions(llvm::CallGraph* cg,
					       bool allowIndirectCalls)
    : m_cg(cg)
    , m_allowIndirectCalls(allowIndirectCalls)
  {}
      
  void DevirtualizeFunctions::computeTypeAliasSets(Module& M) {
    // -- Create type-based alias sets
    for (auto const &F: M) {
      // -- intrinsics are never called indirectly
      if (F.isIntrinsic ()) continue;
      
      // -- local functions whose address is not taken cannot be
      // -- resolved by a function pointer
      if (F.hasLocalLinkage () && !F.hasAddressTaken ()) continue;
      
      // -- skip calls to declarations, these are resolved implicitly
      // -- by calling through the function pointer argument in the
      // -- default case of bounce function
      if (F.isDeclaration ()) continue;
      
      // -- skip seahorn and verifier specific intrinsics
      if (F.getName().startswith ("seahorn.")) continue;
      if (F.getName().startswith ("verifier.")) continue;
      // -- assume entry point is never called indirectly
      if (F.getName ().equals ("main")) continue;
      
      // -- add F to its corresponding alias set
      m_typeAliasSets[DevirtualizeFunctions::typeAliasId(F)].push_back(&F);
    }
  }
  
  bool DevirtualizeFunctions::resolveCallSites(Module & M, CallSiteResolver* CSR) {
    // -- Compute type alias sets if not computed already
    if (m_typeAliasSets.empty()) {
      computeTypeAliasSets(M);
    }
    
    // -- Visit all of the call instructions in this function and
    // -- record those that are indirect function calls.
    visit(M);
    
    // -- Now go through and transform all of the indirect calls that
    // -- we found that need transforming.
    bool Changed = !m_worklist.empty ();
    for (auto I : m_worklist) {
      CallSite CS(I);
      mkDirectCall(CS, CSR);
    }
    // -- Conservatively assume that we've changed one or more call
    // -- sites.
    return Changed;
  }

} // end namespace
