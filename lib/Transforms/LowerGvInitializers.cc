/** 
 * Lower some global variable initializers into explicit
 * initialization code added in the entry block of main.
 *  
 * Given a global variable with initializer like @g:
 * 
 *   %struct.gstate = type { i32, i32, [10 x i32] }
 *   @g = internal global %struct.gstate zeroinitializer, align 4
 * 
 * This pass inserts in the entry block of main :
 * 
 * define i32 @main() {
 *   %_1 = getelementptr %struct.gstate* @g, i32 0, i32 0
 *   call void @verifier.zero_initializer.1(i32* %_1)
 *   %_2 = getelementptr %struct.gstate* @g, i32 0, i32 1
 *   call void @verifier.zero_initializer.1(i32* %_2)
 *   %_3 = getelementptr %struct.gstate* @g, i32 0, i32 2
 *   call void @verifier.zero_initializer.2([10 x i32]* _3)
 *   ...
 * }
 * 
 * Note: initializers can be partially lowered if they get too complex
 * for analysis.
 * 
 */

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/GlobalStatus.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/DenseMap.h"

#include "boost/range.hpp"
#include "boost/format.hpp"

using namespace llvm;

//#define DEBUG_LOWER_GV

#define DEBUG_TYPE "lower-gv"

namespace crab_llvm {

  class LowerGvInitializers : public ModulePass {
    
    static char ID;

    template<typename S> 
    SmallVector<Value*, 8> CreateGEPIndices(const S &stack){
      // The first GEP index must be of pointer type.
      // Struct fields can only be indexed by i32 integer constants.
      // Array, pointers, and vectors can by indexed by any integer constant.
      SmallVector<Value*, 8> indices;
      for(unsigned i=0; i< stack.size(); i++){
	if (i == 0)
	  indices.push_back(ConstantInt::get(m_intptrty, stack[i]));
	else
	  indices.push_back(ConstantInt::get(m_intty, stack[i]));	  
      }
      return indices;
    }
    
    Constant* getInitFn(Type *type, std::vector<Constant*> &LLVMUsed, Module &m) {
      Constant* res = m_initfn[type];
      if (res == NULL) {
	res = m.getOrInsertFunction 
	  (boost::str 
	   (boost::format ("verifier.zero_initializer.%d") % m_initfn.size ()), 
	   m_voidty, type, NULL);
	m_initfn[type] = res;

	Type *i8PTy = Type::getInt8PtrTy(m.getContext ());
	LLVMUsed.push_back(ConstantExpr::getBitCast(res, i8PTy));
      }
      return res;
    }
        
    Function* CreateZeroInitializerFunction(Value * V,
					    std::vector<Constant*> &LLVMUsed,
					    Module &M) {
      AttrBuilder AB;
      Function* fun = dyn_cast<Function>(getInitFn(V->getType(), LLVMUsed, M));
      // XXX: do not mark it as ReadNone, otherwise LLVM will optimize
      // it away.
      //fun->addFnAttr(Attribute::ReadNone);
      if (m_cg) m_cg->getOrInsertFunction(fun);
      return fun;
    }

    template<typename S>
    void CreateZeroInitializerCallSite(Value &base, const S &stack,  
				       IRBuilder<> &Builder,
				       std::vector<Constant*> &LLVMUsed,
				       Module &M) {
      Value *ptr = Builder.CreateInBoundsGEP(&base, CreateGEPIndices(stack));
      Function *f = CreateZeroInitializerFunction(ptr, LLVMUsed, M);
      Builder.CreateCall(f, ptr);
    }


    Constant* getIntInitFn(Type *ty,std::vector<Constant*> &LLVMUsed, Module &m) {
      assert(ty->isIntegerTy());
      
      Constant* res = m_initfn[ty];
      if (res == NULL) {
	res = m.getOrInsertFunction 
	  (boost::str 
	   (boost::format ("verifier.int_initializer.%d") % m_initfn.size ()), 
	   m_voidty, ty->getPointerTo(), ty, NULL);
	m_initfn[ty] = res;
	Type *i8PTy = Type::getInt8PtrTy(m.getContext ());
	LLVMUsed.push_back(ConstantExpr::getBitCast(res, i8PTy));
      }
      return res;
    }
    
    Function* CreateIntInitializerFunction(GlobalVariable &gv,
					   std::vector<Constant*> &LLVMUsed,
					   Module &M) {      
      AttrBuilder AB;
      Function* fun = dyn_cast<Function>(getIntInitFn(gv.getInitializer()->getType(),
						      LLVMUsed, M));
      // XXX: do not mark it as ReadNone, otherwise LLVM will optimize
      // it away.
      //fun->addFnAttr(Attribute::ReadNone);
      if (m_cg) m_cg->getOrInsertFunction(fun);
      return fun;
    }    
    void CreateIntInitializerCallSite(GlobalVariable &gv,
				      IRBuilder<> &Builder,				      
				      std::vector<Constant*> &LLVMUsed,
				      Module &M) {

      assert(gv.hasInitializer() && "global without initializer");
      assert(gv.getInitializer()->getType()->isIntegerTy());
      
      Function *intfn = CreateIntInitializerFunction(gv, LLVMUsed, M);
      Builder.CreateCall(intfn, {&gv, gv.getInitializer()});
    }

    template<typename S>
    bool LowerZeroInitializer(Type* T, Value &base,
			      IRBuilder<> &Builder, Module &M,
			      std::vector<Constant*> &LLVMUsed,
			      S &stack) {
      bool change = false;
      if (isa<IntegerType> (T)) {
	#ifdef DEBUG_LOWER_GV
	IntegerType * IntTy = cast<IntegerType> (T);
	llvm::errs () << "GEP " << base.getName() << " ";
	for (unsigned j=0; j<stack.size();j++)
	  llvm::errs () << stack[j] << " ";
	llvm::errs () << "\n";
	llvm::errs () << "TYPE=" << *IntTy << "\n";	  	
	#endif

	CreateZeroInitializerCallSite(base, stack, Builder, LLVMUsed, M);
	change = true;
      } else if (StructType *STy = dyn_cast<StructType> (T)) {
	for (unsigned i=0; i < STy->getNumElements(); i++) {
	  Type* ETy = STy->getElementType(i);
	  stack.push_back(i);
	  change |= LowerZeroInitializer(ETy, base, Builder, M, LLVMUsed, stack);
	}
      } else if (ArrayType *ATy = dyn_cast<ArrayType> (T)) {
	if (ATy->getElementType()->isIntegerTy()) {
          #ifdef DEBUG_LOWER_GV
	  llvm::errs () << "GEP " << base.getName() << " ";
	  for (unsigned j=0; j<stack.size();j++)
	    llvm::errs () << stack[j] << " ";
	  llvm::errs () << "\n";
	  llvm::errs () << "TYPE=" << *ATy << "\n";	  
	  #endif

	  CreateZeroInitializerCallSite(base, stack, Builder, LLVMUsed, M);	  
	  change = true;
	} else {
	  //llvm::errs () << "CRABLLVM WARNING: skipped initialization of " << *ATy << "\n";
	}
      } else if (isa<PointerType> (T)) {
        #ifdef DEBUG_LOWER_GV
	PointerType *PTy = cast<PointerType> (T);
	llvm::errs () << "GEP " << base.getName() << " ";
	for (unsigned j=0; j<stack.size();j++)
	  llvm::errs () << stack[j] << " ";
	llvm::errs () << "\n";
	llvm::errs () << "TYPE=" << *PTy << "\n";
        #endif

	// Note that we don't go recursively since it would require
	// loading the pointer before continuing calculation.	
	CreateZeroInitializerCallSite(base, stack, Builder, LLVMUsed, M);
	change = true;
      } else {
	//llvm::errs () << "CRABLLVM WARNING: skipped initialization of " << *T << "\n";
      }

      stack.pop_back();	      
      return change;
    }

    /// C may have non-instruction users. Can all of those users be turned into
    /// instructions?
    static bool allNonInstructionUsersCanBeMadeInstructions(Constant *C) {
      // We don't do this exhaustively. The most common pattern that we really need
      // to care about is a constant GEP or constant bitcast - so just looking
      // through one single ConstantExpr.
      //
      // The set of constants that this function returns true for must be able to be
      // handled by makeAllConstantUsesInstructions.
      for (auto *U : C->users()) {
	if (isa<Instruction>(U))
	continue;
	if (!isa<ConstantExpr>(U))
	  // Non instruction, non-constantexpr user; cannot convert this.
	  return false;
	for (auto *UU : U->users())
	  if (!isa<Instruction>(UU))
	    // A constantexpr used by another constant. We don't try and recurse any
	    // further but just bail out at this point.
	    return false;
      }    
      return true;
    }
    
    /// C may have non-instruction users, and
    /// allNonInstructionUsersCanBeMadeInstructions has returned true. Convert the
    /// non-instruction users to instructions.
    void makeAllConstantUsesInstructions(Constant *C) {
      SmallVector<ConstantExpr*,4> Users;
      for (auto *U : C->users()) {
	if (isa<ConstantExpr>(U))
	  Users.push_back(cast<ConstantExpr>(U));
	else
	  // We should never get here; allNonInstructionUsersCanBeMadeInstructions
	  // should not have returned true for C.
	  assert(
		 isa<Instruction>(U) &&
		 "Can't transform non-constantexpr non-instruction to instruction!");
      }
      
      SmallVector<Value*,4> UUsers;
      for (auto *U : Users) {
	UUsers.clear();
	for (auto *UU : U->users())
	  UUsers.push_back(UU);
	for (auto *UU : UUsers) {
	  Instruction *UI = cast<Instruction>(UU);
	  Instruction *NewU = U->getAsInstruction();
	  NewU->insertBefore(UI);
	  UI->replaceUsesOfWith(U, NewU);
	}
	U->dropAllReferences();
      }
    }
    
    
    /** map for initializer functions */
    DenseMap<const Type*, Constant*> m_initfn;
    /** void type **/
    Type *m_voidty;
    /** gep index types **/
    IntegerType* m_intptrty;
    IntegerType* m_intty;    
    /** callgraph **/
    CallGraph *m_cg;
    const DataLayout *m_dl;
    
  public:
    
    LowerGvInitializers () : ModulePass (ID) {}
    
    virtual bool runOnModule (Module &M) {
      m_dl = &M.getDataLayout();
      CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass> ();
      m_cg = cgwp ? &cgwp->getCallGraph () : nullptr;
      m_voidty = Type::getVoidTy(M.getContext());
      m_intptrty = cast<IntegerType>(m_dl->getIntPtrType(M.getContext(), 0));
      m_intty = IntegerType::get(M.getContext(), 32);      

      //llvm::errs () << "IntegerPtrType:" << *m_intptrty << "\n";
      //llvm::errs () << "IntegerType:" << *m_intty << "\n";      
      
      Function *f = M.getFunction ("main");
      if (!f) return false;

      std::vector<GlobalVariable*> gvs;
      for (GlobalVariable &gv : boost::make_iterator_range(M.global_begin(),
							   M.global_end())) {
        if (gv.hasInitializer () && gv.getName() != "llvm.used")
	  gvs.push_back(&gv);
      }
      
      if (gvs.empty()) return false;

      /* add our verifier.zero_initializer and
	 verifier.int_initializer functions to llvm used to avoid them
	 to be optimized away by LLVM.
      */
      GlobalVariable *LLVMUsed = M.getGlobalVariable("llvm.used");
      std::vector<Constant*> MergedVars;
      if (LLVMUsed && LLVMUsed->hasInitializer()) {
      	ConstantArray *Inits = cast<ConstantArray>(LLVMUsed->getInitializer());
      	for (unsigned I = 0, E = Inits->getNumOperands(); I != E; ++I) {
	  MergedVars.push_back(Inits->getOperand(I));
      	}
      	LLVMUsed->eraseFromParent();
      }
      
      IRBuilder<> Builder(M.getContext());
      Builder.SetInsertPoint(&f->getEntryBlock(), f->getEntryBlock().begin ());
      bool change=false;
      for (GlobalVariable *gv : gvs) {
	// -- We only lower ConstantAggregateZero and ConstantInt.
	// 
	// At some point we might want to lower also
	// ConstantDataSequential constants. const char* are usually
	// translated into that. We don't bother for now since
	// crab-llvm does not focus on strings.
	assert(gv->hasInitializer());
	if (isa<ConstantInt>(gv->getInitializer())) {
	  GlobalStatus GS;
	  bool AddressTaken = GlobalStatus::analyzeGlobal(gv, GS);
	  if (!AddressTaken &&
	      !GS.HasMultipleAccessingFunctions &&
	      GS.AccessingFunction &&
	      GS.AccessingFunction->getName() == "main" &&
	      allNonInstructionUsersCanBeMadeInstructions(gv)) {
	    Type *ElemTy = gv->getType()->getElementType();
	    AllocaInst* Alloca = Builder.CreateAlloca(ElemTy, nullptr, gv->getName());
	    Builder.CreateAlignedStore(gv->getInitializer(), Alloca,
				       m_dl->getABITypeAlignment(ElemTy));
	    makeAllConstantUsesInstructions(gv);
	    gv->replaceAllUsesWith(Alloca);
	    gv->eraseFromParent();
	  } else {
	    CreateIntInitializerCallSite(*gv, Builder, MergedVars, M);    
	  }
	  change = true;
	} else if (isa<ConstantAggregateZero>(gv->getInitializer())) {
	  std::vector<uint64_t> stack = {0};
	  change |= LowerZeroInitializer(gv->getInitializer()->getType(), *gv,
					 Builder, M, MergedVars, stack);
	}
      }


      // re-create llvm.used
      if (!MergedVars.empty()) {
	Type *i8PTy = Type::getInt8PtrTy(M.getContext ());      
	ArrayType *ATy = ArrayType::get(i8PTy, MergedVars.size());
	LLVMUsed = new llvm::GlobalVariable(M, ATy, false, llvm::GlobalValue::AppendingLinkage,
					    llvm::ConstantArray::get(ATy, MergedVars),
					    "llvm.used");
	LLVMUsed->setSection("llvm.metadata");
      }
      
      return change;
    }
    
    void getAnalysisUsage (AnalysisUsage &AU) const {
      AU.setPreservesAll ();
      AU.addRequired<llvm::CallGraphWrapperPass>();      
    }

    virtual const char * getPassName() const {
      return "LowerGvInitializers";
    }
    
  };

  char LowerGvInitializers::ID = 0;
  Pass* createLowerGvInitializersPass () { return new LowerGvInitializers (); }  

} 



