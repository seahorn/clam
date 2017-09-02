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

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/DenseMap.h"

#include "boost/range.hpp"
#include "boost/format.hpp"

using namespace llvm;

//#define DEBUG_LOWER_GV

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
    
    Constant* getInitFn(Type *type, Module &m) {
      Constant* res = m_initfn[type];
      if (res == NULL) {
	res = m.getOrInsertFunction 
	  (boost::str 
	   (boost::format ("verifier.zero_initializer.%d") % m_initfn.size ()), 
	   m_voidty, type, NULL);
	m_initfn[type] = res;
      }
      return res;
    }
    
    Function* CreateZeroInitializerFunction(Value * V, Module &M) {
      AttrBuilder AB;
      AttributeSet AS = AttributeSet::get(M.getContext(), AttributeSet::FunctionIndex, AB);
      Function* fun = dyn_cast<Function>(getInitFn(V->getType(), M));
      fun->addFnAttr(Attribute::ReadNone);
      if (m_cg) m_cg->getOrInsertFunction(fun);
      return fun;
    }

    template<typename S>
    void CreateZeroInitializerCallSite(Value &base, const S &stack,  
				       IRBuilder<> &Builder, Module &M) {
      Value *ptr = Builder.CreateInBoundsGEP(&base, CreateGEPIndices(stack));
      Function *f = CreateZeroInitializerFunction(ptr, M);
      Builder.CreateCall(f, ptr);
    }

    void LowerIntInitializer(GlobalVariable &gv, Function* intfn, IRBuilder<> &Builder) {
      assert(gv.hasInitializer() && "global without initializer");
      Builder.CreateCall2(intfn, &gv, gv.getInitializer());
    }

    template<typename S>
    bool LowerZeroInitializer(Type* T, Value &base,
			      IRBuilder<> &Builder, Module &M,
			      S &stack) {
      
      bool change = false;
      if (IntegerType * IntTy = dyn_cast<IntegerType> (T)) {
	#ifdef DEBUG_LOWER_GV
	llvm::errs () << "GEP " << base.getName() << " ";
	for (unsigned j=0; j<stack.size();j++)
	  llvm::errs () << stack[j] << " ";
	llvm::errs () << "\n";
	llvm::errs () << "TYPE=" << *IntTy << "\n";	  	
	#endif

	CreateZeroInitializerCallSite(base, stack, Builder, M);
	change = true;
      } else if (StructType *STy = dyn_cast<StructType> (T)) {
	for (unsigned i=0; i < STy->getNumElements(); i++) {
	  Type* ETy = STy->getElementType(i);
	  stack.push_back(i);
	  change |= LowerZeroInitializer(ETy, base, Builder, M, stack);
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

	  CreateZeroInitializerCallSite(base, stack, Builder, M);	  
	  change = true;
	} else {
	  llvm::errs () << "CRABLLVM WARNING: skipped initialization of " << *ATy << "\n";
	}
      } else if (PointerType *PTy = dyn_cast<PointerType> (T)) {
        #ifdef DEBUG_LOWER_GV
	llvm::errs () << "GEP " << base.getName() << " ";
	for (unsigned j=0; j<stack.size();j++)
	  llvm::errs () << stack[j] << " ";
	llvm::errs () << "\n";
	llvm::errs () << "TYPE=" << *PTy << "\n";
        #endif

	// Note that we don't go recursively since it would require
	// loading the pointer before continuing calculation.	
	CreateZeroInitializerCallSite(base, stack, Builder, M);
	change = true;
      } else {
	llvm::errs () << "CRABLLVM WARNING: skipped initialization of " << *T << "\n";
      }

      stack.pop_back();	      
      return change;
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
      m_dl = &getAnalysis<DataLayoutPass>().getDataLayout ();
      CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass> ();
      m_cg = cgwp ? &cgwp->getCallGraph () : nullptr;
      m_voidty = Type::getVoidTy(M.getContext());
      m_intptrty = cast<IntegerType>(m_dl->getIntPtrType(M.getContext(), 0));
      m_intty = IntegerType::get(M.getContext(), 32);      

      AttrBuilder ab;
      AttributeSet as = AttributeSet::get(M.getContext(), AttributeSet::FunctionIndex, ab);
      Function* intfn = dyn_cast<Function>(
			    M.getOrInsertFunction("verifier.int_initializer", as,
						  m_voidty, m_intptrty->getPointerTo(),
						  m_intty, NULL));
      intfn->addFnAttr(Attribute::ReadNone);
      if (m_cg) m_cg->getOrInsertFunction(intfn);

      //llvm::errs () << "IntegerPtrType:" << *m_intptrty << "\n";
      //llvm::errs () << "IntegerType:" << *m_intty << "\n";      
      
      Function *f = M.getFunction ("main");
      if (!f) return false;
      
      IRBuilder<> Builder (f->getContext ());
      Builder.SetInsertPoint (&f->getEntryBlock (), 
                              f->getEntryBlock ().begin ());

      bool change=false;
      for (GlobalVariable &gv :
	     boost::make_iterator_range (M.global_begin (), M.global_end ())) {
        if (!gv.hasInitializer ()) continue;
	
	// -- We only lower ConstantAggregateZero and ConstantInt.
	// 
	// At some point we might want to lower also
	// ConstantDataSequential constants. const char* are usually
	// translated into that. We don't bother for now since
	// crab-llvm does not focus on strings.
	if (isa<ConstantInt>(gv.getInitializer())) {
	  LowerIntInitializer(gv, intfn, Builder);
	  change = true;
	} else if (isa<ConstantAggregateZero>(gv.getInitializer())) {
	  std::vector<uint64_t> stack = {0};
	  change |= LowerZeroInitializer(gv.getInitializer()->getType(), gv,
					 Builder, M, stack);
	}
      }
      return change;
      
    }
    
    void getAnalysisUsage (AnalysisUsage &AU) const {
      AU.setPreservesAll ();
      AU.addRequired<llvm::DataLayoutPass>();
      AU.addRequired<llvm::CallGraphWrapperPass>();      
    }

    virtual const char * getPassName() const {
      return "LowerGvInitializers";
    }
    
  };

  char LowerGvInitializers::ID = 0;
  Pass* createLowerGvInitializersPass () { return new LowerGvInitializers (); }  

} 



