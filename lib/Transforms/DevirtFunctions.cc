#include "crab_llvm/config.h"
#include "crab_llvm/Transforms/DevirtFunctions.hh"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Analysis/CallGraph.h"

#include <set>
#include <algorithm>

using namespace llvm;

//#define DEVIRT_LOG(...) __VA_ARGS__
//#define DEVIRT_WARNING(...) __VA_ARGS__

#define DEVIRT_LOG(...)
#define DEVIRT_WARNING(...)

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
    if (Constant * C = dyn_cast<Constant>(V)) {
      Constant * CE = ConstantExpr::getZExtOrBitCast (C, Ty);
      return CE;
    }
    
    // Otherwise, insert a cast instruction.
    return CastInst::CreateZExtOrBitCast (V, Ty, Name, InsertPt);
  }

  namespace devirt_impl {
    AliasSetId typeAliasId(CallSite &CS, bool LookThroughCast) {
      assert (isIndirectCall (CS) && "Not an indirect call");
      PointerType *pTy = nullptr;

      if (LookThroughCast) {
	/*
            %390 = load void (i8*, i32*, i32*, i64, i32)*, 
                              void (i8*, i32*, i32*, i64, i32)** 
	      		      bitcast (i64 (i8*, i32*, i32*, i64, i32)** @listdir to 
                                       void (i8*, i32*, i32*, i64, i32)**)
            call void %390(i8* %385, i32* %1, i32* %2, i64 %139, i32 %26) 
	*/
	if (LoadInst* LI = dyn_cast<LoadInst>(CS.getCalledValue())) {
	  if (Constant* C = dyn_cast<Constant>(LI->getPointerOperand())) {
	    if (ConstantExpr* CE = dyn_cast<ConstantExpr>(C)) {
	      if (CE->getOpcode() == Instruction::BitCast) {
		if (PointerType *ppTy = dyn_cast<PointerType>(CE->getOperand(0)->getType())) {
		  pTy = dyn_cast<PointerType>(ppTy->getElementType());
		  if (pTy) {
		    assert (isa<FunctionType>(pTy->getElementType())
			    && "The type of called value is not a pointer to a function");
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
      
      pTy = dyn_cast<PointerType> (CS.getCalledValue()->getType ());
      assert (pTy && "Unexpected call not through a pointer");
      assert (isa<FunctionType> (pTy->getElementType ())
	      && "The type of called value is not a pointer to a function");
      return pTy;
    }

    AliasSetId typeAliasId(const Function &F) {
      return F.getFunctionType()->getPointerTo();
    }
  } // namespace devirt_impl

  struct FunctionCompare {

    // TODO: we should compare two functions in a deterministic way
    // across multiple executions.
    bool operator()(const Function *F1, const Function *F2) {
      // if (F1->hasName() && F2->hasName()) {
      // 	if (F1->getName () != F2->getName()) {
      // 	  return F1->getName() < F2->getName();
      // 	}
      // } 
      return F1 < F2;
    }
  };
    
  /***
   * Begin specific callsites resolvers
   ***/
  
  CallSiteResolverByTypes::CallSiteResolverByTypes(Module &M)
    : CallSiteResolver(RESOLVER_TYPES)
    , m_M(M) {
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
      AliasSet& Targets = m_targets_map[devirt_impl::typeAliasId(F)];
      FunctionCompare cmp;
      auto it = std::upper_bound(Targets.begin(), Targets.end(), &F, cmp);
      Targets.insert(it, &F);
    }      
  }

  const CallSiteResolverByTypes::AliasSet* CallSiteResolverByTypes::getTargets(CallSite &CS) {
    AliasSetId id = devirt_impl::typeAliasId(CS, true);
    auto it = m_targets_map.find(id);
    if (it != m_targets_map.end()) {
      return &(it->second);
    }
    return nullptr;
  }

  Function* CallSiteResolverByTypes::getBounceFunction(CallSite& CS) {
    AliasSetId id = devirt_impl::typeAliasId(CS, false);
    auto it = m_bounce_map.find(id);
    if (it != m_bounce_map.end()) {
      return it->second;
    } else {
      return nullptr;
    }
  }
  
  void CallSiteResolverByTypes::cacheBounceFunction(CallSite&CS, Function* bounce) {
    AliasSetId id = devirt_impl::typeAliasId(CS, false);    
    m_bounce_map.insert({id, bounce});
  }
  
  template<typename Dsa>
  CallSiteResolverByDsa<Dsa>::CallSiteResolverByDsa(Module& M, Dsa& dsa,
						    bool incomplete, unsigned max_num_targets)
    : CallSiteResolverByTypes(M)
    , m_M(M)
    , m_dsa(dsa)
    , m_allow_incomplete(incomplete)
    , m_max_num_targets(max_num_targets) {
    
    CallSiteResolver::m_kind = RESOLVER_DSA;
    
    /*
      Assume that Dsa provides these methods:
       - bool isComplete(CallSite&)
       - iterator begin(CallSite&)
       - iterator end(CallSite&) 
       where each element of iterator is of type Function*
    */
    
    // build the target map
    unsigned num_indirect_calls = 0;
    unsigned num_complete_calls = 0;
    unsigned num_resolved_calls = 0;    
    for (auto &F: m_M) {
      for (auto &BB: F) {
	for (auto &I: BB) {
	  Instruction *CI= nullptr;
	  if (isa<CallInst>(&I)) {
	    CI = &I;
	  }
	  if (!CI && isa<InvokeInst>(&I)) {
	    CI = &I;
	  }
	  if (CI) {
	    CallSite CS(CI);
	    if (isIndirectCall(CS)) {
	      num_indirect_calls++;
	      if (m_allow_incomplete || m_dsa.isComplete(CS)) {
		num_complete_calls++;
		AliasSet dsa_targets;
		dsa_targets.append(m_dsa.begin(CS), m_dsa.end(CS));
		if (dsa_targets.empty()) {
		  DEVIRT_WARNING(errs() << "WARNING Devirt (dsa): does not have any target for "
				        << *(CS.getInstruction()) << "\n";);
		  continue;
		}
		// sort dsa_targets
		FunctionCompare cmp;
		std::sort(dsa_targets.begin(), dsa_targets.end(), cmp);
		
		DEVIRT_LOG(errs() << "\nDsa-based targets: \n";
			   for(auto F: dsa_targets) {
			     errs() << "\t" << F->getName() << "::" << *(F->getType()) << "\n";
			   });
		
		if (const AliasSet* types_targets = CallSiteResolverByTypes::getTargets(CS)) {

		  DEVIRT_LOG(errs() << "Type-based targets: \n";
			     for(auto F: *types_targets) {
			       errs() << "\t" << F->getName() << "::" << *(F->getType()) << "\n";
			     });
		  
		  // --- We filter out those dsa targets whose signature do not match.
		  AliasSet refined_dsa_targets;
		  // assert(is_sorted(types_targets))
		  // assert(is_sorted(dsa_targets))
		  std::set_intersection(dsa_targets.begin(), dsa_targets.end(),
					types_targets->begin(), types_targets->end(),
					std::back_inserter(refined_dsa_targets));
		  if (refined_dsa_targets.empty()) {
		    DEVIRT_WARNING(errs() << "WARNING Devirt (dsa): cannot resolve "
				          << *(CS.getInstruction())
				          << " after refining dsa targets with calsite type\n";);
		  } else {
		    if (refined_dsa_targets.size() <= m_max_num_targets) {
		      num_resolved_calls++;
		      m_targets_map.insert({CS.getInstruction(), refined_dsa_targets});
		      DEVIRT_LOG(errs() << "Devirt (dsa) resolved " << *(CS.getInstruction())
				        << " with targets=";
				 for(auto F: refined_dsa_targets) {
				   errs() << "\t" << F->getName() << "::" << *(F->getType()) << "\n";				   
				 });
		    } else {
		      DEVIRT_WARNING(errs() << "WARNING Devirt (dsa): unresolve "
				            << *(CS.getInstruction())
				            << " because the number of targets is greater than "
				            << m_max_num_targets << "\n";);
		    } 
		  }
		} else {
		  DEVIRT_WARNING(errs() << "WARNING Devirt (dsa): cannot resolve "
				        << *(CS.getInstruction())
				        << " because there is no internal function with same callsite type\n";);
		}
	      } else {
		DEVIRT_WARNING(errs() << "WARNING Devirt (dsa): cannot resolve "
			              << *(CS.getInstruction())
			              << " because the corresponding dsa node is not complete\n";);
		
		DEVIRT_LOG(AliasSet targets;
			   targets.append(m_dsa.begin(CS), m_dsa.end(CS));
			   errs() << "Dsa-based targets: \n";
			   for(auto F: targets) {
			     errs() << "\t" << F->getName() << "::" << *(F->getType()) << "\n";
			   };)
	      }
	    }
	  }
	}
      }
    }
    // errs() << "=== DEVIRT (Dsa) stats===\n";
    // errs() << "BRUNCH_STAT INDIRECT CALLS " << num_indirect_calls << "\n";
    // errs() << "BRUNCH_STAT COMPLETE CALLS " << num_complete_calls << "\n";
    // errs() << "BRUNCH_STAT RESOLVED CALLS " << num_resolved_calls << "\n";
  }

  template<typename Dsa>
  CallSiteResolverByDsa<Dsa>::~CallSiteResolverByDsa(){
    m_targets_map.clear();    
    m_bounce_map.clear();
  }
  
  template<typename Dsa>  
  const typename CallSiteResolverByDsa<Dsa>::AliasSet*
  CallSiteResolverByDsa<Dsa>::getTargets(CallSite& CS) {
    auto it = m_targets_map.find(CS.getInstruction());
    if (it != m_targets_map.end()) {
      return &(it->second);
    }
    return nullptr;
  }
  
  template<typename Dsa>
  Function* CallSiteResolverByDsa<Dsa>::getBounceFunction(CallSite&CS) {
    AliasSetId id = devirt_impl::typeAliasId(CS, false);
    auto it = m_bounce_map.find(id);
    if (it != m_bounce_map.end()) {
      const AliasSet* cachedTargets = it->second.first;
      const AliasSet* Targets = getTargets(CS);
      if (cachedTargets && Targets) {
	if (std::equal(cachedTargets->begin(), cachedTargets->end(),
		       Targets->begin())) {
	  return it->second.second;
	}
      }
    }
    return nullptr;
  }
  
  template<typename Dsa>
  void CallSiteResolverByDsa<Dsa>::cacheBounceFunction(CallSite& CS, Function* bounce) {
    if (const AliasSet* targets = getTargets(CS)) {
      AliasSetId id = devirt_impl::typeAliasId(CS, false);      
      m_bounce_map.insert({id, {targets, bounce}});
    }
      
  }

  /***
   * End specific callsites resolver
   ***/
  

  DevirtualizeFunctions::DevirtualizeFunctions(llvm::CallGraph* /*cg*/,
					       bool allowIndirectCalls)
    : //m_cg(nullptr) 
      m_allowIndirectCalls(allowIndirectCalls)
      , m_num_indirect_calls(0)
      , m_num_resolved_calls(0) { }

  DevirtualizeFunctions::~DevirtualizeFunctions() {
    errs() << "=== Devirtualization stats===\n";
    errs() << "BRUNCH_STAT INDIRECT CALLS " << m_num_indirect_calls << "\n";
    errs() << "BRUNCH_STAT RESOLVED CALLS " << m_num_resolved_calls << "\n";
  }
  
  Function* DevirtualizeFunctions::mkBounceFn(CallSite &CS, CallSiteResolver* CSR) {
    assert (isIndirectCall (CS) && "Not an indirect call");

    if (Function* bounce = CSR->getBounceFunction(CS)) {
      DEVIRT_LOG(errs() << "Reusing bounce function for " << *(CS.getInstruction()) 
		 << "\n\t" << bounce->getName() << "::" << *(bounce->getType()) << "\n";);
      return bounce;
    }
    
    const AliasSet* Targets = CSR->getTargets(CS);
    if (!Targets || Targets->empty()) {
      return nullptr;
    }

    DEVIRT_LOG(errs() << *CS.getInstruction() << "\n";
	       errs() << "Possible targets:\n";
	       for(const Function* F: *Targets) {
		 errs() << "\t" << F->getName() << ":: " << *(F->getType()) << "\n";
	       })
    
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
    auto ai = F->arg_begin();
    ++ai;
    for(auto ae = F->arg_end(); ai != ae; ++ai) {
      fargs.push_back(&*ai);
      ai->setName("arg");
    }
          
    // Create an entry basic block for the function.  All it should do is perform
    // some cast instructions and branch to the first comparison basic block.
    BasicBlock* entryBB = BasicBlock::Create (M->getContext(), "entry", F);
    
    // For each function target, create a basic block that will call that
    // function directly.
    DenseMap<const Function*, BasicBlock*> targets;
    for (const Function *FL : *Targets) {
      // Create the basic block for doing the direct call
      BasicBlock* BL = BasicBlock::Create (M->getContext(), FL->getName(), F);
      targets[FL] = BL;
      // Create the direct function call
      CallInst* directCall = CallInst::Create (const_cast<Function*>(FL),
                                               fargs, "", BL);
      // TODO: update call graph
      // if (m_cg) {
      //   auto fl_cg = m_cg->getOrInsertFunction (const_cast<Function*> (FL));
      //   auto cf_cg = m_cg->getOrInsertFunction (directCall->getCalledFunction ());
      //   fl_cg->addCalledFunction (CallSite (directCall), cf_cg);
      // }
      
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
    for (const Function *FL : *Targets) {
      // Cast the function pointer to an integer.  This can go in the entry
      // block.
      Value * TargetInt =
	castTo (const_cast<Function*>(FL), VoidPtrType, "", InsertPt);
      
      // Create a new basic block that compares the function pointer to the
      // function target.  If the function pointer matches, we'll branch to the
      // basic block performing the direct call for that function; otherwise,
      // we'll branch to the next function call target.
      BasicBlock* TB = targets[FL];
      BasicBlock* newB =
	BasicBlock::Create(M->getContext(), "test." + FL->getName(), F);
      CmpInst * setcc = CmpInst::Create(Instruction::ICmp,
					CmpInst::ICMP_EQ,
					TargetInt, FArg, "sc", newB);
      BranchInst::Create (TB, tailBB, setcc, newB);
      
      // Make this newly created basic block the next block that will be reached
      // when the next comparison will need to be done.
      tailBB = newB;
    }
    
    // Make the entry basic block branch to the first comparison basic block.
    InsertPt->setSuccessor(0, tailBB);

    // -- cache the newly created function
    CSR->cacheBounceFunction(CS, F);
    
    // Return the newly created bounce function.
    return F;
  }


  void DevirtualizeFunctions::mkDirectCall(CallSite CS, CallSiteResolver* CSR) {
    m_num_indirect_calls++;
    
    const Function *bounceFn = mkBounceFn(CS, CSR);
    // -- something failed
    if (!bounceFn) return;

    m_num_resolved_calls++;
    
    DEVIRT_LOG(errs() << "Callsite: " << *(CS.getInstruction()) << "\n";
	       errs() << "Bounce function: " << bounceFn->getName() << ":: "
	       << *(bounceFn->getType()) << "\n";)
    
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
      // TODO: update call graph
      // if (m_cg) {
      //   m_cg->getOrInsertFunction (const_cast<Function*> (bounceFn));
      //   (*m_cg)[CI->getParent ()->getParent ()]->addCalledFunction
      //     (CallSite (CN), (*m_cg)[CN->getCalledFunction ()]);
      // }

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

      // TODO: update call graph
      // if (m_cg) {
      //   m_cg->getOrInsertFunction (const_cast<Function*> (bounceFn));
      //   (*m_cg)[CI->getParent ()->getParent ()]->addCalledFunction
      //     (CallSite (CN), (*m_cg)[CN->getCalledFunction ()]);
      // }

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
          
  bool DevirtualizeFunctions::resolveCallSites(Module & M, CallSiteResolver* CSR) {
    // -- Visit all of the call instructions in this function and
    // -- record those that are indirect function calls.
    visit(M);
    
    // -- Now go through and transform all of the indirect calls that
    // -- we found that need transforming.
    bool Changed = !m_worklist.empty ();
    while (!m_worklist.empty()) {
      auto I = m_worklist.back();
      m_worklist.pop_back();
      CallSite CS(I);
      mkDirectCall(CS, CSR);
    }
    // -- Conservatively assume that we've changed one or more call
    // -- sites.
    return Changed;
  }  
} // end namespace

#ifdef HAVE_DSA
/* Template instantiation */
// llvm-dsa 
#include "dsa/CallTargets.h"
namespace crab_llvm {  
template class CallSiteResolverByDsa<dsa::CallTargetFinder<EQTDDataStructures>>;
} // end namespace
#endif   
