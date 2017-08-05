/** 
 * Lower some global variable initializers into explicit
 * initialization code added in the entry block of main.
 *  
 * Given a global variable with initializer like @g:
 * 
 *   %struct.gstate = type { i32, i32, [10 x i32] }
 *   @g = internal global %struct.gstate zeroinitializer, align 4
 * 
 * This pass inserts the entry block of main as follows:
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

namespace crab_llvm {
  
  class LowerGvInitializers : public ModulePass {
    
    static char ID;

    Value* CreateIntCst (int64_t val) {
      ConstantInt *cst = ConstantInt::get(m_intty, val);
      return cst;
    }
    
    SmallVector<Value*, 8> CreateGEPIndices(std::vector<unsigned> &stack){
      SmallVector<Value*, 8> indices;
      for(unsigned i=0; i< stack.size(); i++){
	indices.push_back(CreateIntCst(stack[i]));
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
      if (m_cg) m_cg->getOrInsertFunction(fun);
      return fun;
    }
    
    
    bool LowerZeroInitializer(Type* T, Value &base,
			    IRBuilder<> &Builder, Module &M,
			    std::vector<unsigned> &stack,
			    SmallSet<Type*,8> &cache) {
      
      if (cache.count(T) > 0)
	return true;

      bool change = false;
      if (IntegerType * IT = dyn_cast<IntegerType> (T)) {
	#if 0
	llvm::errs () << "GEP " << base.getName() << " ";
	for (unsigned j=0; j<stack.size();j++){
	  llvm::errs () << stack[j] << " ";
	}
	llvm::errs () << "\n";
	#endif
	
	Value *ptr =
	  Builder.CreateInBoundsGEP(&base, CreateGEPIndices(stack));
	Function *f = CreateZeroInitializerFunction(ptr, M);
	Builder.CreateCall(f, ptr);
	stack.pop_back();
	change = true;
      } else if (StructType *STy = dyn_cast<StructType> (T)) {
	for (unsigned i=0; i < STy->getNumElements(); i++) {
	  Type* ETy = STy->getElementType(i);
	  stack.push_back(i);
	  change |= LowerZeroInitializer(ETy, base, Builder, M, stack, cache);
	}
	stack.pop_back();
      } else if (ArrayType *ATy = dyn_cast<ArrayType> (T)) {
	if (ATy->getElementType()->isIntegerTy()) {
	  #if 0
	  llvm::errs () << "GEP " << base.getName() << " ";
	  for (unsigned j=0; j<stack.size();j++)
	    llvm::errs () << stack[j] << " ";
	  llvm::errs () << "\n";
	  #endif
	  
	  Value *ptr =
	    Builder.CreateInBoundsGEP(&base, CreateGEPIndices(stack));
	  Function *f = CreateZeroInitializerFunction(ptr, M);
	  Builder.CreateCall(f, ptr);
	  change = true;
	} else {
	  // TODO: skipped
	}
	stack.pop_back();
      } else if (PointerType *PTy = dyn_cast<PointerType> (T)) {
	Value *ptr =
	  Builder.CreateInBoundsGEP(&base, CreateGEPIndices(stack));
	Function *f = CreateZeroInitializerFunction(ptr, M);
	Builder.CreateCall(f, ptr);
	stack.pop_back();
      }
      return change;
    }

    /** map for initializer functions */
    DenseMap<const Type*, Constant*> m_initfn;
    /** void type **/
    Type *m_voidty;
    /** gep index type **/
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
      m_intty = cast<IntegerType>(m_dl->getIntPtrType(M.getContext(), 0));
  
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
	
	if (!(isa<ConstantAggregateZero>(gv.getInitializer()) ||
	      isa<ConstantInt>(gv.getInitializer()))) continue;
	
	std::vector<unsigned> stack = {0};
	SmallSet<Type*,8> cache;
	change |= LowerZeroInitializer(gv.getInitializer()->getType(), gv,
				       Builder, M, stack, cache);
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



