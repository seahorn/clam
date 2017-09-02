/* 
 * Translate a LLVM function to a CFG language understood by Crab.
 * 
 * The translation considers the following Crab types:
 * 
 * - booleans <--> isIntegerTy(1)
 * - integers <--> isIntegerTy() && !isIntegerTy(1)
 * - pointers <--> isPointerTy()
 * 
 * In addition, there is an array type which is a type that it doesn't
 * exist in LLVM but it does in Crab. An array is a sequence of
 * consecutive bytes for which certain guarantees hold (e.g., sequence
 * elements have compatible types and they are always accessed in the
 * same way). Arrays are identified using HeapAbstraction.
 * 
 * Known limitations of the translation:
 * 
 * - Ignore floating point instructions.
 * - Ignore ashr and shl with non-constant shifts.
 * - Ignore integer bitwidths in most operations (this can affect
 *   soundness).
 * - Comparison between pointers
 * - Pointer cast instructions
 */

#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

#include "boost/tuple/tuple.hpp"
#include "boost/range/iterator_range.hpp"
#include "boost/range/algorithm/set_algorithm.hpp"
#include "boost/optional.hpp"

#include "crab/common/debug.hpp"
#include "crab/common/stats.hpp"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/HeapAbstraction.hh"
#include "crab_llvm/Support/CFG.hh"

#include <algorithm>

using namespace llvm;
using namespace boost;
using namespace crab;
using namespace crab::cfg;
using namespace ikos;

cl::opt<bool>
CrabCFGSimplify("crab-cfg-simplify",
	 cl::desc ("Simplify Crab CFG"), 
	 cl::init (false),
	 cl::Hidden);

cl::opt<bool>
CrabPrintCFG("crab-print-cfg",
	 cl::desc ("Print Crab CFG"), 
	 cl::init (false));

cl::opt<bool>
CrabEnableUniqueScalars("crab-singleton-aliases",
	 cl::desc ("Treat singleton alias sets as scalar values"), 
	 cl::init (false));

/** 
 * Allow to track memory but ignoring pointer arithmetic. This makes
 * sense because array smashing can reason about memory without having
 * any knowledge about pointer arithmetic as long as the memory
 * disambiguation analysis ensures:
 * a) an array can be only pointed by objects of compatible types, and
 * b) every memory array accesses are aligned.
 */
cl::opt<bool>
CrabDisablePointers("crab-disable-ptr",
		    cl::desc ("Disable translation of pointers"), 
		    cl::init (false),
		    cl::Hidden);

/**
 * Reduce boolean operations to integer ones
 * It has some unsoundness with SExt and ZExt instructions.
 **/
cl::opt<bool>
CrabBoolAsInt("crab-bool-as-int",
	      cl::desc ("Treat Booleans as integers"), 
	      cl::init (false),
	      cl::Hidden);

/**
 * Since LLVM IR is in SSA form many of the havoc statements are
 * redundant since variables can be defined only once.
 */
cl::opt<bool>
CrabIncludeHavoc("crab-include-useless-havoc",
		 cl::desc ("Include all havoc statements."), 
		 cl::init (true),
		 cl::Hidden);

cl::opt<bool>
CrabArrayInit("crab-arr-init",
	cl::desc ("Initialization of arrays for weak array domains (e.g., smashing)."), 
	cl::init (true),
	cl::Hidden);

/**
 * Enable initialization of arrays that correspond to objects that may
 * contain an infinite number of memory locations.
 *
 * Initialization of allocation sites originated from calloc or memset
 * instructions may be unsound if it can be executed by more than one
 * execution.
*/
cl::opt<bool>
CrabUnsoundArrayInit("crab-arr-unsound-init",
		     cl::desc ("Unsound initialization of arrays."), 
		     cl::init (false),
		     cl::Hidden);

cl::opt<bool>
CrabDisableWarnings("crab-disable-warnings",
	      cl::desc ("Disable warning messages"), 
	      cl::init (false),
	      cl::Hidden);

namespace crab_llvm {

  #define CRABLLVM_WARNING(MSG) \
    if (!CrabDisableWarnings) \
      {llvm::errs () << "CRABLLVM WARNING: " << MSG << "\n";}
  #define GET_VAR(V,LFAC) LFAC.get_vfac()[&V]
  #define FRESH_VAR(LFAC) LFAC.get_vfac().get()
  #define GET_VAR_FROM_REGION(R,LFAC) LFAC.get_vfac().get(R.get_id())
  // Note: getRegion only returns regions containing integers
  #define GET_REGION(I,V) m_mem.getRegion(*(I.getParent()->getParent()),V)
  #define CST_TRUE z_lin_cst_t::get_true()
  #define CST_FALSE z_lin_cst_t::get_false()

  // Factory to create crab literals (variables or constants)
  // A literal can be of type: boolean, integer or pointer.
  //
  // XXX: we do not create a datatype for literals (via inheritance)
  // because literals are very simple so we prefer to save allocation
  // of pointers and casts across subclasses.
  struct crabLitFactory;

  class crabBoolLit {
    // A boolean literal is either a variable or constants true and false.
    friend class crabLitFactory;
    
    // invariant class: m_cst ^ m_var
    boost::optional<bool> m_cst;
    boost::optional<varname_t> m_var;

    crabBoolLit(bool cst): m_cst(cst) {}
    crabBoolLit(varname_t var): m_var(var) {}

  public:
    
    bool isConst() const { return (m_cst ? true: false); }
    bool isVar() const { return (m_var ? true: false); }
    varname_t getVar() const {
      assert (isVar());
      return *m_var;
    }
    bool isTrue () const {
      if (!isConst()) return false;
      return *m_cst;
    }
    bool isFalse () const {
      if (!isConst()) return false;
      return !(*m_cst); 
    }
  };

  class crabPtrLit {

    // A pointer literal is either a variable or constant null
    friend class crabLitFactory;
    
    boost::optional<varname_t> m_lit;

    crabPtrLit() {} // null
    crabPtrLit(varname_t v): m_lit(v) {}

  public:
    
    bool isNull() const { return (m_lit ? false : true); }
    bool isVar() const { return !isNull(); }
    varname_t getVar() const {
      assert (isVar());
      return *(m_lit);
    }
  };

  class crabNumLit {

    // A numerical literal is either a number or variable
    friend class crabLitFactory;

    // invariant class: m_num ^ m_var     
    boost::optional<ikos::z_number> m_num;
    boost::optional<varname_t> m_var;

    crabNumLit(ikos::z_number n): m_num(n) {}
    crabNumLit(varname_t v): m_var(v) {}

  public:
    
    bool isNum() const { return (m_num ? true : false); }
    bool isVar() const { return (m_var ? true : false); }
    ikos::z_number getNum() const {
      assert (isNum());
      return *(m_num);
    }
    varname_t getVar() const {
      assert (isVar());
      return *(m_var);
    }
    z_lin_exp_t getExp () const {
      if (isNum())
	return z_lin_exp_t(getNum());
      else {
	assert (isVar());
	return z_lin_exp_t(getVar());
      }
    }
    
  };

  static bool isBool(const llvm::Type *t){
    return (t->isIntegerTy(1) && !CrabBoolAsInt);
  }
  
  static bool isBool(const llvm::Value&v) {
    return isBool(v.getType());
  }

  static bool isInteger(const llvm::Type *t) {
    return (t->isIntegerTy() && !isBool(t));
  }
  
  static bool isInteger(const llvm::Value&v) {
    return isInteger(v.getType());
  }
  
  static bool isPointer(const llvm::Value&v,
			const crab::cfg::tracked_precision tracklev) {
    return (v.getType()->isPointerTy() && tracklev >= PTR && !CrabDisablePointers);
  }

  /** Converts v to mpz_class. Assumes that v is signed */
  static mpz_class toMpz (const llvm::APInt &v) {
    // Based on:
    // https://llvm.org/svn/llvm-project/polly/trunk/lib/Support/GICHelper.cpp
    // return v.getSExtValue ();
    
    llvm::APInt abs;
    abs = v.isNegative () ? v.abs () : v;
    
    const uint64_t *rawdata = abs.getRawData ();
    unsigned numWords = abs.getNumWords ();
    
    mpz_class res;
    mpz_import(res.get_mpz_t (), numWords, 1, 
	       sizeof (uint64_t), 0, 0, rawdata);
    
    return v.isNegative () ? mpz_class(-res) : res;
  }
  
  static boost::optional<int64_t> getIntConstant(const ConstantInt* CI){
    if (CI->getType ()->isIntegerTy(1)) {
      return (int64_t) CI->getZExtValue();
    }
    else if (CI->getValue().getMinSignedBits() <= 64) {
      return CI->getSExtValue();
    }
    else {
      CRABLLVM_WARNING("Warning: " <<
		       toMpz(CI->getValue()).get_str() <<   
		       " does not fit in int64_t.");
      return boost::optional<int64_t>();
    }
  }

  static bool isTracked (const llvm::Value &v,
			 const crab::cfg::tracked_precision tracklev) {
    // -- ignore any shadow variable created by seahorn
    if (v.getName().startswith ("shadow.mem")) 
      return false;
    
    // -- a pointer
    if (v.getType ()->isPointerTy ()) 
      return (tracklev >= crab::cfg::PTR && !CrabDisablePointers); 
    
    // -- always track integer and boolean registers
    return v.getType ()->isIntegerTy ();
  }

  // Convert a llvm::Value to either a boolean, pointer or numerical
  // literal.
  class crabLitFactory {

    llvm_variable_factory &m_vfac;
    crab::cfg::tracked_precision m_tracklev;

  public:

    crabLitFactory(llvm_variable_factory &vfac,
		   crab::cfg::tracked_precision tracklev)
      : m_vfac (vfac), m_tracklev(tracklev) {}


    llvm_variable_factory& get_vfac() { return m_vfac; }
    crab::cfg::tracked_precision get_track() const { return m_tracklev;}

    // Note that getBoolLit, getPtrLit and getNumLit are not aware of
    // which types are tracked or not. They only use type information
    // and not the track level.
    boost::optional<crabBoolLit> getBoolLit (const llvm::Value &v){
      if (const llvm::ConstantInt *c =
	  llvm::dyn_cast<const llvm::ConstantInt>(&v)) {
	// -- constant boolean
	if (boost::optional<int64_t> n = getIntConstant(c))
	  return crabBoolLit( (*n) > 0 ? true : false);
      } else if (isBool(v) && !llvm::isa<llvm::ConstantExpr>(v)
		 /*&& isTracked(v, m_tracklev)*/) {
	// -- boolean variable 
	return crabBoolLit(m_vfac[&v]);
      }
      return boost::optional<crabBoolLit>();

    }

    boost::optional<crabPtrLit> getPtrLit (const llvm::Value &v){
      if (llvm::isa<llvm::ConstantPointerNull> (&v)) {
	// -- constant null
	return crabPtrLit();
      } else if (v.getType()->isPointerTy() && !llvm::isa<llvm::ConstantExpr>(v)
		 /*&& isTracked(v, m_tracklev)*/) {
	// -- pointer variable 
	return crabPtrLit(m_vfac[&v]);
      }
      return boost::optional<crabPtrLit>();
    }

    boost::optional<crabNumLit> getNumLit (const llvm::Value &v){
      if (const llvm::ConstantInt *c =
	  llvm::dyn_cast<const llvm::ConstantInt>(&v)) {
	// -- constant integer
	if (boost::optional<int64_t> n = getIntConstant(c))
	  return crabNumLit(*n);
      } else if (isInteger(v) && !llvm::isa<llvm::ConstantExpr>(v) 
		 /*&& isTracked(v, m_tracklev)*/) {
	// -- integer variable 
	return crabNumLit(m_vfac[&v]);
      }
      return boost::optional<crabNumLit>();
    }
  };
    
  static bool hasDebugLoc (const Instruction *inst) {
    if (!inst) return false;
    const DebugLoc &dloc = inst->getDebugLoc ();
    return (!(dloc.isUnknown ()));
  }

  static crab::cfg::debug_info getDebugLoc (const Instruction *inst) {
    if (!hasDebugLoc(inst))
      return crab::cfg::debug_info();

    const DebugLoc &dloc = inst->getDebugLoc ();
    unsigned Line = dloc.getLine ();
    unsigned Col = dloc.getCol ();
    std::string File; 
    DIScope Scope (dloc.getScope ());
    if (Scope) File = Scope.getFilename ();
    else File = "unknown file";
    return crab::cfg::debug_info(File, Line, Col);
  }

  static uint64_t storageSize (const Type *t, const DataLayout &dl) {
    return dl.getTypeStoreSize (const_cast<Type*> (t));
  }
  
  static void normalizeCmpInst(CmpInst &I) {
    switch (I.getPredicate()){
      case ICmpInst::ICMP_UGT:	
      case ICmpInst::ICMP_SGT: I.swapOperands(); break; 
      case ICmpInst::ICMP_UGE:	
      case ICmpInst::ICMP_SGE: I.swapOperands(); break; 
      default: ;
    }
  }

  static bool isLogicalOp(const Instruction &I) {
    return (I.getOpcode() >= Instruction::Shl &&
            I.getOpcode() <= Instruction::Xor);
  }

  static bool isIntArithOp(const Instruction &I) {
    return (I.getOpcode() == Instruction::Add ||
            I.getOpcode() == Instruction::Sub ||
            I.getOpcode() == Instruction::Mul ||
            I.getOpcode() == Instruction::UDiv ||
            I.getOpcode() == Instruction::SDiv ||
            I.getOpcode() == Instruction::URem ||
              I.getOpcode() == Instruction::SRem);
  }

  static bool isIntCast(const CastInst &I) {
    return I.isIntegerCast();
  }

  static bool isPointerCast(const CastInst &I) {
    return isa<IntToPtrInst>(I) || isa<PtrToIntInst>(I) || isa<BitCastInst>(I);
  }
  
  static bool isIntToBool(const CastInst &I) {
    return (isa<TruncInst>(I) && I.getDestTy()->isIntegerTy(1));
  }

  static bool isBoolToInt(const CastInst &I) {
    return ((isa<ZExtInst>(I) || isa<SExtInst>(I))  && I.getSrcTy()->isIntegerTy(1));
  }

  static bool isBoolArray (const Type &T) {
    return (T.isArrayTy() && T.getArrayElementType()->isIntegerTy(1));
  }
  
  static bool isIntArray (const Type &T) {
    return (T.isArrayTy() &&
	    T.getArrayElementType()->isIntegerTy() &&
	    !(T.getArrayElementType()->isIntegerTy(1)));
  }

  static bool isPointerArray (const Type &T) {
    return (T.isArrayTy() && T.getArrayElementType()->isPointerTy());
  }
  
  static bool isAssertFn(const Function* F) {
    return (F->getName().equals("verifier.assert") || 
            F->getName().equals("crab.assert") || 
            F->getName().equals("__CRAB_assert"));
  }

  static bool isErrorFn(const Function* F) {
    return (F->getName().equals("seahorn.error") ||
	    F->getName().equals("verifier.error") ||
	    F->getName().equals("__VERIFIER_error") || 	    	    
            F->getName().equals("__SEAHORN_error"));
  }

  static bool isAssumeFn(const Function* F) {
    return (F->getName().equals("verifier.assume"));
  }

  static bool isNotAssumeFn(const Function* F) {
    return (F->getName().equals("verifier.assume.not"));
  }

  static bool isVerifierCall (const Function *F) {
    return (isAssertFn (F) || isErrorFn (F) ||
	    isAssumeFn(F) || isNotAssumeFn (F));
  }

  static bool isZeroInitializer(const Function *F) {
    return F->getName().startswith("verifier.zero_initializer");
  }

  static bool isIntInitializer(const Function *F) {
    return F->getName().startswith("verifier.int_initializer");
  }
  
  static bool containsVerifierCall (BasicBlock &B) {
    for (auto &I: B) {
      if (CallInst *CI = dyn_cast<CallInst>(&I)) {
        CallSite CS (CI);
	const Value *calleeV = CS.getCalledValue ();
	const Function *callee =
	  dyn_cast<Function>(calleeV->stripPointerCasts());	
        if (callee && isVerifierCall (callee)) 
          return true;
      }
    }
    return false;
  }


  // Return true if all uses are BranchInst's
  static bool AllUsesAreBrInst (Value* V) {
    // XXX: do not strip pointers here
    for (auto &U: V->uses ())
      if (!isa<BranchInst> (U.getUser()))
  	return false;
    return true;
  }  

  // Return true if all uses are BranchInst's or Select's
  static bool AllUsesAreBrOrIntSelectCondInst (Value* V) {
    // XXX: do not strip pointers here
    for (auto &U: V->uses ()) {
      if ((!isa<BranchInst> (U.getUser())) && (!isa<SelectInst> (U.getUser())))
  	return false;
      if (SelectInst *SI = dyn_cast<SelectInst>(U.getUser())) {
	if (isBool(*SI) || SI->getCondition () != V) {
	  // if the operands are bool or V is not the condition
	  return false;
	}
      }
    }
    return true;
  }
  
  // Return true if all uses are the callee at callsites
  static bool AllUsesAreIndirectCalls (Value* V)  {
    // XXX: do not strip pointers here
    for (auto &U: V->uses ()) {
      if (CallInst *CI = dyn_cast<CallInst> (U.getUser ())) {
	CallSite CS (CI);
	const Value *callee = CS.getCalledValue ();
	if (callee == V) continue;
      }
      return false;
    }
    return true;
  }

  // Return true if all uses are GEPs
  static bool AllUsesAreGEP(Value *V) {
    for (auto &U: V->uses ())
      if (!isa<GetElementPtrInst> (U.getUser()))
	return false;
    return true;
  }
      
  // internal type for typed variables
  typedef std::pair<varname_t, variable_type> typed_variable_t;

  /* Used only during the translation of formal/actual function parameters */
  static variable_type getCrabType (Type * ty) {
    if (ty->isIntegerTy(1) && !CrabBoolAsInt)
      return BOOL_TYPE;
    
    if (ty->isIntegerTy())
      return INT_TYPE;
    
    if (ty->isPointerTy())
      return PTR_TYPE;
    
    return UNK_TYPE;
  }


  // helper to handle option CrabEnableUniqueScalars
  template<typename HeapAbstraction>
  static const Value* isGlobalSingleton (Region<HeapAbstraction> r) {
    if (CrabEnableUniqueScalars) {
      if (r.isUnknown()) return nullptr;
      if (const Value* v = r.getSingleton()) return v;
    }
    return nullptr;
  }

  // static void mkUnsignedLtOrLeq (CmpInst &I,
  // 				    crabLitFactory &lfac,
  // 				    basic_block_t &bb,
  // 				    z_lin_exp_t op1, z_lin_exp_t op2) {
  //   /*
  //      if (op1 >= 0)
  // 	      if (op2 >= 0)
  // 	         op1 LT|LEQ op2
  //          else 
  //             false
  //      else 
  //          if (op2 < 0)
  //             op1 LT|LEQ op2
  //          else 
  //             true
  //   */
  //   assert (I.getPredicate () == CmpInst::ICMP_ULT ||
  // 	       I.getPredicate () == CmpInst::ICMP_ULE);
    
  //   if (CrabBoolAsInt) {
  //     /*
  // 	b1 := op1 LT|LEQ op2 ? 1  : 0
  // 	b2 := op2 >= 0       ? b1 : 0
  // 	b3 := op2 < 0        ? b1 : 1
  // 	b4 := op1 >= 0       ? b2 : b3
  //     */
  //     varname_t b1 = FRESH_VAR(lfac);
  //     z_lin_cst_t c1  = ((I.getPredicate () == CmpInst::ICMP_ULT) ?
  // 			    z_lin_cst_t(op1 <= op2 - 1) :
  // 			    z_lin_cst_t(op1 <= op2));
  //     bb.select (b1, c1 , 1, 0);	  
  //     varname_t b2 = FRESH_VAR(lfac);
  //     z_lin_cst_t c2 (op2 >= z_number (0));
  //     bb.select (b2, c2, z_var(b1), 0);	  
  //     varname_t b3 = FRESH_VAR(lfac);
  //     z_lin_cst_t c3 (op2 <= -1);	  
  //     bb.select (b3, c3, z_var(b1), 1);
  //     z_lin_cst_t c4 (op1 >= 0);	  
  //     bb.select (GET_VAR(I,lfac), c4, z_var(b2),z_var(b3));
  //   } else {
  //     
  //   }
  // }

  static void havoc(varname_t v, basic_block_t &bb) {
    if (CrabIncludeHavoc)
      bb.havoc(v);
  }
  
  // Perform one of these translations depending on CrabDisableBoolean:
  //    %x = icmp geq %y, 10  ---> select(%x, %y >= 10, 1, 0)
  //                          ---> bool_assign(%x, y >= 0)
  static void cmpInstToCrabBool(CmpInst &I,
				crabLitFactory &lfac, basic_block_t &bb) {
				
    normalizeCmpInst(I);
    
    const Value& v0 = *I.getOperand(0);
    const Value& v1 = *I.getOperand(1);

    varname_t lhs = GET_VAR(I, lfac);
    
    boost::optional<crabNumLit> l0 = lfac.getNumLit(v0);
    if (!l0) { havoc(lhs,bb); return;}
    boost::optional<crabNumLit> l1 = lfac.getNumLit(v1); 
    if (!l1) { havoc(lhs,bb); return;}
    
    z_lin_exp_t op0 = (*l0).getExp();
    z_lin_exp_t op1 = (*l1).getExp();

    switch (I.getPredicate ()) {
    case CmpInst::ICMP_EQ: {
      z_lin_cst_t cst(op0 == op1);
      if (isBool(I))
	bb.bool_assign(lhs, cst);
      else 
	bb.select(lhs, cst, 1, 0);
      break;
    }
    case CmpInst::ICMP_NE: {
      z_lin_cst_t cst(op0 != op1);
      if (isBool(I))
	bb.bool_assign(lhs, cst);
      else
	bb.select(lhs, cst, 1, 0);
      break;
    }
    case CmpInst::ICMP_SLT: {
      z_lin_cst_t cst(op0 <= op1 - 1);
      if (isBool(I))
	bb.bool_assign(lhs, cst);
      else 
	bb.select(lhs, cst, 1, 0);
      break;
    }
    case CmpInst::ICMP_SLE: {
      z_lin_cst_t cst(op0 <= op1);
      if (isBool(I))
	bb.bool_assign(lhs, cst);
      else	    
	bb.select(lhs, cst, 1, 0);
      break;
    }	
    case CmpInst::ICMP_ULT: 
    case CmpInst::ICMP_ULE:
      CRABLLVM_WARNING("translation skipped " << I << ".\n" <<
		       "Enable --lower-unsigned-icmp");	
      break;
    default:  
      llvm_unreachable ("ERROR: it should not happen while translating CmpInst");
    }
  }

  /* If possible, return a linear constraint from CmpInst */
  static boost::optional<z_lin_cst_t>
  cmpInstToCrabInt (CmpInst &I, crabLitFactory &lfac, const bool isNegated = false) {
    normalizeCmpInst(I);
    
    const Value& v0 = *I.getOperand (0);
    const Value& v1 = *I.getOperand (1);

    boost::optional<crabNumLit> l0 = lfac.getNumLit(v0);
    if (!l0) return boost::optional<z_lin_cst_t>();
    boost::optional<crabNumLit> l1 = lfac.getNumLit(v1); 
    if (!l1) return boost::optional<z_lin_cst_t>();
    
    z_lin_exp_t op0 = (*l0).getExp();
    z_lin_exp_t op1 = (*l1).getExp();
    
    switch (I.getPredicate()) {
    case CmpInst::ICMP_EQ:
      if (!isNegated)
	return z_lin_cst_t(op0 == op1);
      else
	return z_lin_cst_t(op0 != op1);
      break;
    case CmpInst::ICMP_NE:
      if (!isNegated)
	return z_lin_cst_t(op0 != op1);
      else
	return z_lin_cst_t(op0 == op1);
	break;
    case CmpInst::ICMP_SLT:
      if (!isNegated)
	return z_lin_cst_t(op0 <= op1 - 1);
      else
	return z_lin_cst_t(op0 >= op1);
      break; 
    case CmpInst::ICMP_SLE:
      if (!isNegated)
	return z_lin_cst_t(op0 <= op1);
      else
	return z_lin_cst_t(op0 >= op1 + 1);
      break;
    case CmpInst::ICMP_ULT: // loss of precision
    case CmpInst::ICMP_ULE: 
      CRABLLVM_WARNING("translation skipped " << I << ".\n" <<
		       "Enable --lower-unsigned-icmp");	
      break;	
    default: ;;  
    }
    return boost::optional<z_lin_cst_t> ();
  }

  // Create statement lhs := c from constant c and return type of c
  static variable_type assignConstant(Constant *c, varname_t lhs,
				      basic_block_t &bb, crabLitFactory &lfac) {
    if (isa<ConstantInt> (c)) {
      if (isBool(*c)) {
	boost::optional<crabBoolLit> lit = lfac.getBoolLit(*c);
	if (lit && (*lit).isConst()) {
	  bb.bool_assign(lhs, (*lit).isTrue() ? CST_TRUE: CST_FALSE);
	  return BOOL_TYPE;
	}
      } else {
	boost::optional<crabNumLit> lit = lfac.getNumLit(*c);
	if (lit && (*lit).isNum()) {
	  bb.assign(lhs, (*lit).getExp());
	  return INT_TYPE;
	}
      }
    } else if (isa<ConstantPointerNull>(c)) {
      if (isPointer(*c, lfac.get_track())) {
	boost::optional<crabPtrLit> lit = lfac.getPtrLit(*c);
	if (lit && (*lit).isNull()) {
	  bb.ptr_null(lhs);
	  return PTR_TYPE;
	}
      }
    }
    bb.havoc (lhs);
    return UNK_TYPE;
  }
  
  static typed_variable_t normalizeFuncParamOrRet(Value& V, basic_block_t &bb,
						  crabLitFactory &lfac) {
    if (Constant *c = dyn_cast< Constant> (&V)) {
      varname_t lhs = FRESH_VAR(lfac);
      variable_type c_ty = assignConstant(c, lhs, bb, lfac);
      return typed_variable_t(lhs, c_ty);      
    }  else {
      return typed_variable_t(GET_VAR(V, lfac), getCrabType(V.getType()));
    }
  }
  
  //! Translate PHI nodes
  struct CrabPhiVisitor : public InstVisitor<CrabPhiVisitor> {
    
    crabLitFactory &m_lfac;
    HeapAbstraction& m_mem;
    // block where assignment will be inserted
    basic_block_t& m_bb; 
    // incoming block of the PHI instruction
    const BasicBlock& m_inc_BB; 

    CrabPhiVisitor (crabLitFactory &lfac,
		    HeapAbstraction& mem,
		    basic_block_t& bb, 
                    const BasicBlock& inc_BB): 
      m_lfac(lfac), m_mem(mem), m_bb(bb), m_inc_BB(inc_BB) {}


    bool doBoolPHINode(varname_t lhs, const Value& v){
      boost::optional<crabBoolLit> lit = m_lfac.getBoolLit(v);
      bool res = true;
      if (lit && (*lit).isConst())
	m_bb.bool_assign (lhs, (*lit).isTrue() ? CST_TRUE : CST_FALSE);		
      else if (lit && (*lit).isVar())				
	m_bb.bool_assign (lhs, (*lit).getVar());		
      else {
	CRABLLVM_WARNING("unexpected boolean PHI node");
	res = false;
      }
      return res;
    }

    bool doNumPHINode(varname_t lhs, const Value& v) {
      boost::optional<crabNumLit> lit = m_lfac.getNumLit(v);
      bool res = true;
      if (lit)
	m_bb.assign (lhs, (*lit).getExp());
      else {
	CRABLLVM_WARNING("unexpected integer PHI node");
	res = false;
      }
      return res;
    }

    bool doPtrPHINode(varname_t lhs, const Value& v) {
      boost::optional<crabPtrLit> lit = m_lfac.getPtrLit(v);
      bool res = true;
      if (lit && (*lit).isNull())
	m_bb.ptr_null(lhs);
      else if (lit && (*lit).isVar())
	m_bb.ptr_assign(lhs, (*lit).getVar(), z_lin_exp_t(0));		  
      else {
	CRABLLVM_WARNING("unexpected pointer PHI node");
	res = false;
      }
      return res;
    }
    
    void visitBasicBlock (BasicBlock &BB) {
      auto curr = BB.begin ();
      if (!isa<PHINode> (curr)) return;

      DenseMap<const Value*, varname_t> old_val_map;

      // --- All the phi-nodes must be evaluated atomically. This
      //     means that if one phi node v1 has as incoming value
      //     another phi node v2 in the same block then it should take
      //     the v2's old value (i.e., before v2's evaluation).

      for (; PHINode *phi = dyn_cast<PHINode> (curr); ++curr) {        

        const Value &v = *phi->getIncomingValueForBlock (&m_inc_BB);

        if (!isTracked(v, m_lfac.get_track())) continue;
	
        const PHINode* phi_v = dyn_cast<PHINode> (&v);
        if (phi_v && (phi_v->getParent () == &BB)) {
	  // -- save the old version of the variable that maps to the
	  //    phi node v
	  auto it = old_val_map.find(&v);
	  if (it == old_val_map.end()) {
	    varname_t old_val = FRESH_VAR(m_lfac);	    
	    bool success = true;
	    if (isBool(*phi_v)) {
	      success &= doBoolPHINode(old_val, v);
	    } else if (phi_v->getType()->isIntegerTy()) {
	      success &= doNumPHINode(old_val, v);
	    } else if (isPointer(*phi_v, m_lfac.get_track())){
	      success &= doPtrPHINode(old_val, v);
	    }
	    if (success) {
	      old_val_map.insert (std::make_pair(&v, old_val));
	    } else {
	      // var leaking: it not success we would have created a
	      // useless fresh variable
	    }
	  }
	}
      }

      curr = BB.begin ();
      for (unsigned i = 0; isa<PHINode> (curr); ++curr) {
        PHINode &phi = *cast<PHINode> (curr);
        if (!isTracked(phi, m_lfac.get_track())) continue;
	
        const Value &v = *phi.getIncomingValueForBlock (&m_inc_BB);
        varname_t lhs = GET_VAR(phi, m_lfac);
        auto it = old_val_map.find (&v);
        if (it != old_val_map.end ()) {
	  // -- use old version if exists
	  if (isBool(phi)) {
	    m_bb.bool_assign(lhs, it->second);
	  } else if (phi.getType()->isIntegerTy()) {
	    m_bb.assign (lhs, z_lin_exp_t(it->second));
	  } else if (isPointer(phi, m_lfac.get_track())){
	    m_bb.ptr_assign(lhs, it->second, z_lin_exp_t(0));
	  }
        } else {
	  bool success=true;
	  if (isBool(phi)) {
	    success &= doBoolPHINode(lhs, v);
	  } else if (phi.getType()->isIntegerTy()) {
	    success &= doNumPHINode(lhs,v);
	  } else if (isPointer(phi, m_lfac.get_track())){
	    success &= doPtrPHINode(lhs, v);
	  }
	  
	  if (!success) havoc(lhs,m_bb);
	}
      }
    }
  };

			     
  //! Translate the rest of instructions
  class CrabInstVisitor : public InstVisitor<CrabInstVisitor> {

    crabLitFactory &m_lfac;
    HeapAbstraction& m_mem;
    const DataLayout* m_dl;
    const TargetLibraryInfo* m_tli;
    basic_block_t& m_bb;
    const bool m_is_inter_proc;
    unsigned int m_object_id;
    
    typedef typename HeapAbstraction::region_t region_t;
    typedef typename HeapAbstraction::region_set_t region_set_t;
    
    unsigned fieldOffset (const StructType *t, unsigned field) {
      return m_dl->getStructLayout (const_cast<StructType*>(t))->
          getElementOffset (field);
    }

    uint64_t storageSize(const Type *t) {
      return crab_llvm::storageSize(t, *m_dl);
    }
        
    // Return true if all uses of V are non-trackable memory accesses.
    // Useful to avoid translating bitcode that won't have any effect
    // anyway.
    bool AllUsesAreNonTrackMem (Value* V) const {
      // XXX: not sure if we should strip pointers here
      V = V->stripPointerCasts();
      for (auto &U: V->uses ()) {
        if (StoreInst *SI = dyn_cast<StoreInst> (U.getUser())) {
          Type* ty = SI->getOperand (0)->getType ();
          if (ty->isIntegerTy ()) {
            if (Instruction *I = dyn_cast<Instruction> (V))  {
              if (GET_REGION((*I),V).isUnknown())
                continue;
            }
            return false;
          }
        }
        else if (LoadInst *LI = dyn_cast<LoadInst> (U.getUser())) {
          Type *ty = LI->getType();
          if (ty->isIntegerTy ()) {
            if (Instruction *I = dyn_cast<Instruction> (V))  {
              if (GET_REGION((*I),V).isUnknown ())
                continue;
            }
            return false;
          }
        }
        else if (CallInst *CI = dyn_cast<CallInst> (U.getUser())) { 
          CallSite CS (CI);
          Function* callee = CS.getCalledFunction ();
          if (callee && (callee->getName().startswith ("llvm.dbg") || 
                         callee->getName().startswith ("shadow.mem")))
            continue;
          else // conservatively return false
            return false; 
        }
        else
          return false;
      }
      return true;
    }
    
    void doArithmetic (varname_t lhs, BinaryOperator &i) {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);

      boost::optional<crabNumLit> l1 = m_lfac.getNumLit(v1);
      if (!l1) {havoc(lhs,m_bb); return;}
      boost::optional<crabNumLit> l2 = m_lfac.getNumLit(v2); 
      if (!l2) {havoc(lhs,m_bb); return;}
      
      z_lin_exp_t op1 = (*l1).getExp();
      z_lin_exp_t op2 = (*l2).getExp();
      
      switch(i.getOpcode()) {
      case BinaryOperator::Add:
	m_bb.add(lhs, op1, op2);
	break;
      case BinaryOperator::Sub:
	if (op1.is_constant()) { 
	  // Crab cfg does not support subtraction of a constant by a
	  // variable because the crab api for abstract domains
	  // does not support it.
	  m_bb.assign(lhs, z_lin_exp_t (op1.constant ()));
	  m_bb.sub(lhs, z_lin_exp_t (lhs), op2);
	} else {
	  m_bb.sub(lhs, op1, op2);
	}
	break;
      case BinaryOperator::Mul:
	m_bb.mul(lhs, op1, op2);
	break;
      case BinaryOperator::SDiv:
	if (op1.is_constant()) {            
	  // Crab cfg does not support division of a constant by a
	  // variable because the crab api for abstract domains
	  // does not support it.
	  m_bb.assign (lhs, z_lin_exp_t (op1.constant ()));
	  m_bb.div (lhs, z_lin_exp_t (lhs), op2);
	} else {
	  m_bb.div (lhs, op1, op2);
	}
	break;
      case BinaryOperator::UDiv:
	if (op1.is_constant() && op2.is_constant ()) {
	  // Crab cfg api does not support unsigned arithmetic operations
	  // with both constant operands. Llvm frontend should get
	  // rid of them.
	  CRABLLVM_WARNING("ignored udiv with both constant operands");
	  havoc(lhs,m_bb);
	} else if (op1.is_constant()) {  
	  // Crab cfg does not support division of a constant by a
	  // variable because the crab api for abstract domains
	  // does not support it.
	  m_bb.assign(lhs, z_lin_exp_t (op1.constant ()));
	  m_bb.udiv(lhs, z_lin_exp_t (lhs), op2);
	} else {
	  m_bb.udiv(lhs, op1, op2);
	}
	break;
      case BinaryOperator::SRem:
	if (op1.is_constant()) {           
	  // Crab cfg does not support rem of a constant by a
	  // variable because the crab api for abstract domains
	  // does not support it.
	  m_bb.assign(lhs, z_lin_exp_t (op1.constant ()));
	  m_bb.rem(lhs, z_lin_exp_t (lhs), op2);
	} else {
	  m_bb.rem(lhs, op1, op2);
	}
	break;
      case BinaryOperator::URem:
	if (op1.is_constant() && op2.is_constant ()) {
	  // Crab cfg does not support unsigned arithmetic
	  // operations with both constant operands. Llvm frontend
	  // should get rid of them.
	  CRABLLVM_WARNING("ignored urem with constant operands");
	  havoc(lhs,m_bb);
	} else if (op1.is_constant()) {
	  // Crab cfg does not support rem of a constant by a
	  // variable because the crab api for abstract domains does
	  // not support it.
	  m_bb.assign (lhs, z_lin_exp_t (op1.constant ()));
	  m_bb.urem (lhs, z_lin_exp_t (lhs), op2);
	} else {
	  m_bb.urem (lhs, op1, op2);
	}
	break;
      case BinaryOperator::Shl:
	if (op2.is_constant()) {
	  ikos::z_number k = op2.constant ();
	  int shift = (int) k;
	  assert (shift >= 0);
	  unsigned factor = 1;
	  for (unsigned i = 0; i < (unsigned) shift; i++) 
	    factor *= 2;
	  m_bb.mul (lhs, op1, z_lin_exp_t (factor));            
	} else {
	  CRABLLVM_WARNING("translation skipped shl with non-constant shift");
	  havoc(lhs,m_bb);
	}
	break;
      case BinaryOperator::AShr:
	if (op2.is_constant()) {
	  ikos::z_number k = op2.constant ();
	  int shift = (int) k;
	  assert (shift >= 0);
	  unsigned factor = 1;
	  for (unsigned i = 0; i < (unsigned) shift; i++) 
	    factor *= 2;
	  m_bb.div (lhs, op1, z_lin_exp_t (factor));            
	} else {
	  CRABLLVM_WARNING("translation skipped ashr with non-constant shift");
	  havoc(lhs,m_bb);
	}
	break;
      default:
	CRABLLVM_WARNING("translation skipped " << i);
	havoc(lhs,m_bb);
      }
    }

    void doBoolLogicOp(varname_t lhs, BinaryOperator &i) {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);

      boost::optional<crabBoolLit> l1 = m_lfac.getBoolLit(v1);
      if (!l1) {havoc(lhs,m_bb); return;}
      boost::optional<crabBoolLit> l2 = m_lfac.getBoolLit(v2); 
      if (!l2) {havoc(lhs,m_bb); return;}
      
      crabBoolLit b1 = *l1;
      crabBoolLit b2 = *l2;
      
      switch(i.getOpcode()) {
      case BinaryOperator::And:
	if (b1.isConst() && b2.isConst ()) {
	  m_bb.bool_assign (lhs, b1.isTrue() && b2.isTrue()?
			    CST_TRUE : CST_FALSE);
	} else if (b1.isFalse() || b2.isFalse()) {
	  m_bb.bool_assign (lhs, CST_FALSE);
	} else if (b1.isTrue()) {
	  m_bb.bool_assign (lhs, b2.getVar());
	} else if (b2.isTrue()) {
	  m_bb.bool_assign (lhs, b1.getVar());
	} else if (b1.isVar() && b2.isVar()) {
	  m_bb.bool_and(lhs, b1.getVar(), b2.getVar());
	} else {
	  llvm_unreachable("ERROR: unexpected uncovered case in doBoolLogicOp And");
	}
	break;
      case BinaryOperator::Or:
	if (b1.isConst() && b2.isConst ()) {
	  m_bb.bool_assign (lhs,  b1.isTrue() || b2.isTrue()?
			    CST_TRUE: CST_FALSE);
	} else if (b1.isTrue() || b2.isTrue()){
	  m_bb.bool_assign (lhs, CST_TRUE);
	} else if (b1.isFalse()) {
	  m_bb.bool_assign (lhs, b2.getVar());
	} else if (b2.isFalse()) {
	  m_bb.bool_assign (lhs, b1.getVar());
	} else if (b1.isVar() && b2.isVar()) {
	  m_bb.bool_or(lhs, b1.getVar(), b2.getVar());
	} else {
	  llvm_unreachable("ERROR: unexpected uncovered case in doBoolLogicOp Or");
	  }
	break;
      case BinaryOperator::Xor:
	if (b1.isConst() && b2.isConst ()) {
	  m_bb.bool_assign (lhs, (((b1.isTrue() && b2.isFalse()) ||
				   (b1.isFalse() && b2.isTrue())) ?
				  CST_TRUE: CST_FALSE));
	} else if (b1.isTrue()){
	  m_bb.bool_assign (lhs, b2.getVar(), true /*negate rhs*/);
	} else if (b1.isFalse()){
	  m_bb.bool_assign (lhs, b2.getVar());
	} else if (b2.isTrue()) {
	    m_bb.bool_assign (lhs, b1.getVar(), true /*negate rhs*/);
	} else if (b2.isFalse()) {
	  m_bb.bool_assign (lhs, b1.getVar());	    
	} else if (b1.isVar() && b2.isVar()){
	  m_bb.bool_xor(lhs, b1.getVar(), b2.getVar());
	} else {
	  llvm_unreachable("ERROR: unexpected uncovered case in doBoolLogicOp Xor");
	}
	break;
      default:
	CRABLLVM_WARNING("translation skipped " << i);
	havoc(lhs,m_bb);
      }
    }

    void doIntLogicOp(varname_t lhs, BinaryOperator &i) {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);

      boost::optional<crabNumLit> l1 = m_lfac.getNumLit(v1);
      if (!l1) {havoc(lhs,m_bb); return;}
      boost::optional<crabNumLit> l2 = m_lfac.getNumLit(v2); 
      if (!l2) {havoc(lhs,m_bb); return;}
      
      z_lin_exp_t op1 = (*l1).getExp();
      z_lin_exp_t op2 = (*l2).getExp();
      
      switch(i.getOpcode()) {
        case BinaryOperator::And:
          m_bb.bitwise_and(lhs, op1, op2);
          break;
        case BinaryOperator::Or:
          m_bb.bitwise_or(lhs, op1, op2);
          break;
        case BinaryOperator::Xor:
          m_bb.bitwise_xor(lhs, op1, op2);
          break;
        default:
	  CRABLLVM_WARNING("translation skipped " << i);
          havoc(lhs,m_bb);
      }
    }

    void doAllocFn (Instruction &I) {
      
      if (isPointer(I, m_lfac.get_track())) {
	m_bb.ptr_new_object(GET_VAR(I, m_lfac), m_object_id++);
      }

      if (false &&
	  m_lfac.get_track() >= ARR && CrabArrayInit && CrabUnsoundArrayInit) {
        region_t r = GET_REGION(I,&I);
        bool isMainCaller =
	  I.getParent()->getParent()->getName().equals("main");
        if (isMainCaller && !r.isUnknown()) {
          // We apply here again the "Initialization hook"
	  // TODO: add an array_assume statement.
	  // We need to figure out:
	  // - the number of elements and the size of each element
	  // - Otherwise, we create a fresh (unbounded) variable and use it
	  //   for the upper bound. 
        }
      }

      // -- havoc return value
      if (!I.getType()->isVoidTy() && isTracked(I, m_lfac.get_track()) &&
	  !isPointer(I, m_lfac.get_track())) {
	havoc(GET_VAR(I, m_lfac), m_bb);
      }
    }

    void doMemIntrinsic(MemIntrinsic& I) {

      if (MemCpyInst *MCI = dyn_cast<MemCpyInst>(&I)) {
	Value* src = MCI->getSource ();
	Value* dst = MCI->getDest ();
	region_t dst_reg = GET_REGION(I,dst); 
	region_t src_reg = GET_REGION(I,src);
	if (dst_reg.isUnknown () || src_reg.isUnknown ()) return;
	m_bb.havoc (GET_VAR_FROM_REGION(dst_reg, m_lfac));
	m_bb.array_assign (GET_VAR_FROM_REGION(dst_reg, m_lfac),
			   GET_VAR_FROM_REGION(src_reg, m_lfac),
			   /*TODO: adapt code if region contains booleans or pointers*/
			   ARR_INT_TYPE);
      } else if (MemSetInst *MSI = dyn_cast<MemSetInst>(&I)) {
	
	if (CrabUnsoundArrayInit && isInteger(*(MSI->getValue()))) {
	  // TODO: array of booleans
	  // TODO: array of pointers
	  Value* dst = MSI->getDest();
	  region_t r = GET_REGION(I,dst);
	  if (r.isUnknown()) return;

	  boost::optional<crabNumLit> len = m_lfac.getNumLit(*(MSI->getLength()));
	  boost::optional<crabNumLit> val = m_lfac.getNumLit(*(MSI->getValue()));
	  if (val && len) {
	    z_lin_exp_t lb_idx(ikos::z_number(0));
	    z_lin_exp_t ub_idx((*len).getExp());
	    m_bb.havoc(GET_VAR_FROM_REGION(r, m_lfac));
	    // FIXME/TODO: double check this
	    uint64_t elem_size = MSI->getAlignment();
	    if ((*val).isNum()) {
	      m_bb.array_assume (GET_VAR_FROM_REGION(r, m_lfac),
				 ARR_INT_TYPE, elem_size,
				 lb_idx, ub_idx,
				 (*val).getNum());
	    } else {
	      m_bb.array_assume (GET_VAR_FROM_REGION(r, m_lfac),
				 ARR_INT_TYPE, elem_size,
				 lb_idx, ub_idx,
				 (*val).getVar());
	      
	    }
	  }
	}
      }
    }

    void doInitializer(CallInst &I) {

      CallSite CS (&I);
      // v is either a global variable or a gep instruction that
      // indexes an address inside the global variable.
      Value *v =  CS.getArgument (0);
      
      auto r = GET_REGION(I, v);
      if (!r.isUnknown()) {
	// TODO: array of booleans
	// TODO: array of pointers	
	varname_t a = GET_VAR_FROM_REGION(r, m_lfac);
	ikos::z_number init_val(ikos::z_number(0));
	if (CS.arg_size() == 2) {
	  /* verifier.int_initializer(v,k) */
	  boost::optional<crabNumLit> linit_val = m_lfac.getNumLit(*(CS.getArgument(1)));
	  if (linit_val)
	    init_val = (*linit_val).getNum();
	}
	
	z_lin_exp_t lb_idx(ikos::z_number(0));
	
	Type* Ty = cast<PointerType>(v->getType())->getElementType();
	if (isInteger(Ty)) {
	  if (const Value* s = isGlobalSingleton(r)) {
	    m_bb.assign (GET_VAR(*s, m_lfac), init_val);	    
	  } else {
	    uint64_t elem_size = storageSize(Ty);
	    z_lin_exp_t ub_idx(ikos::z_number(0));
	    m_bb.array_assume (a, ARR_INT_TYPE, elem_size,
			       lb_idx, ub_idx,init_val);
	  }
	} else if (isIntArray(*Ty)){
	  if (cast<ArrayType>(Ty)->getNumElements() == 0) {
	    // TODO: zero-length array are possible inside structs We
	    // can simply make ub_idx > 0.  However, DSA is very
	    // likely that it will collapse anyway so the fact we skip
	    // the translation won't make any difference.
	    CRAB_WARN("translation skipped a zero-length array");
	    return;
	  }
	  uint64_t elem_size = storageSize(cast<ArrayType>(Ty)->getElementType());
	  z_lin_exp_t ub_idx((cast<ArrayType>(Ty)->getNumElements() - 1)* elem_size);
	  m_bb.array_assume (a, ARR_INT_TYPE, elem_size,
			     lb_idx, ub_idx,init_val);
	}
      }
    }
    
    void doVerifierCall (CallInst &I) {
      CallSite CS (&I);

      const Value *calleeV = CS.getCalledValue ();
      const Function *callee = dyn_cast<Function>(calleeV->stripPointerCasts());
      if (!callee) return;
            
      if (isErrorFn(callee)) {
        m_bb.assertion(CST_FALSE, getDebugLoc(&I));
        return;
      }

      if (!isAssertFn(callee) && !isAssumeFn(callee) && !isNotAssumeFn(callee))
	return; 

      Value *cond = CS.getArgument (0);
      
      if (!isTracked(*cond, m_lfac.get_track())) return;

      if (ConstantInt *CI = dyn_cast<ConstantInt> (cond)) {
        // -- cond is a constant
	if (auto n = getIntConstant(CI)) {
	  int64_t cond_val = *n;
	  if (cond_val > 0) {
	    if (isAssertFn(callee) || isAssumeFn(callee)) {
	      // do nothing
	    } else {
	      assert(isNotAssumeFn(callee));
	      m_bb.assume(CST_FALSE); 	      
	    }
	  } else {
	    if (isNotAssumeFn(callee)) {
	      // do nothing
	    } else if (isAssumeFn(callee)) {
	      m_bb.assume(CST_FALSE);
	    } else {
	      assert(isAssertFn(callee));
	      m_bb.assertion(CST_FALSE, getDebugLoc(&I));
	    }
	  }
	}
      } else {
	varname_t lhs = GET_VAR(*cond, m_lfac);
	// -- cond is not a constant
	if (isBool(*cond)) {
	  if (isNotAssumeFn(callee))
	    m_bb.bool_not_assume(lhs);
	  else if (isAssumeFn(callee))
	    m_bb.bool_assume(lhs);
	  else  {
	    assert(isAssertFn(callee));
	    m_bb.bool_assert(lhs, getDebugLoc(&I));
	  }
	} else {
	  if (isNotAssumeFn(callee))
	    m_bb.assume(z_lin_exp_t(lhs) <= z_lin_exp_t(0));
	  else if (isAssumeFn(callee))
	    m_bb.assume(z_lin_exp_t(lhs) >= z_lin_exp_t(1));
	  else  {
	    assert(isAssertFn(callee));
	    m_bb.assertion(z_lin_exp_t(lhs) >= z_lin_exp_t(1),
			   getDebugLoc(&I));
	  }
	}
      }
    }

   public:

    CrabInstVisitor (crabLitFactory &lfac, HeapAbstraction &mem,
                     const DataLayout* dl, const TargetLibraryInfo* tli,
                     basic_block_t &bb, bool isInterProc)
      : m_lfac(lfac), m_mem(mem),
	m_dl(dl), m_tli(tli), 
	m_bb(bb), 
	m_is_inter_proc (isInterProc),
	m_object_id (0) {}
    
    /// skip PHI nodes (processed elsewhere)
    void visitPHINode (PHINode &I) {}

    /// skip BranchInst (processed elsewhere)
    void visitBranchInst (BranchInst &I) {}
    
    /// I is already translated if it is the condition of a branch or
    /// a select's condition.  Here we cover cases where I is an
    /// operand of other instructions.
    void visitCmpInst (CmpInst &I) {
      
      if (!isTracked(I, m_lfac.get_track())) return;

      if (isPointer(*I.getOperand(0),m_lfac.get_track()) &&
	  isPointer(*I.getOperand(1),m_lfac.get_track())) {
	
	if (!AllUsesAreBrInst(&I)) {
	  CRABLLVM_WARNING("translation skipped comparison between pointers");
	  havoc(GET_VAR(I, m_lfac), m_bb);		  
	}
	return;
      }
      
      // make sure we only translate if both operands are integers
      if (!I.getOperand(0)->getType()->isIntegerTy() ||
	  !I.getOperand(1)->getType()->isIntegerTy()) {
	havoc(GET_VAR(I, m_lfac), m_bb);	
	return;
      }

      // already lowered elsewhere
      if (AllUsesAreBrOrIntSelectCondInst(&I)) return;
      
      // otherwise we lower the ICmpInst
      cmpInstToCrabBool(I, m_lfac, m_bb);
    }
      
    void visitBinaryOperator(BinaryOperator &I) {
      if (!isTracked(I, m_lfac.get_track())) return;
      
      varname_t lhs = GET_VAR(I, m_lfac);
      switch (I.getOpcode()) {
      case BinaryOperator::Add:
      case BinaryOperator::Sub:
      case BinaryOperator::Mul:
      case BinaryOperator::SDiv:
      case BinaryOperator::UDiv:
      case BinaryOperator::SRem:
      case BinaryOperator::URem:
      case BinaryOperator::Shl:
      case BinaryOperator::AShr:
	doArithmetic(lhs, I);
	break;
      case BinaryOperator::And:
      case BinaryOperator::Or:
      case BinaryOperator::Xor:
	if (isBool(I))
	  doBoolLogicOp(lhs, I);
	else
	  doIntLogicOp(lhs, I);
	break;
      case BinaryOperator::LShr:
	// FALL-THROUGH
      default:
	havoc(lhs, m_bb);
      }
    }
        
    void visitCastInst (CastInst &I) {
      if (!isTracked(I, m_lfac.get_track())) return;
      
      if ((isa<SExtInst>(I) || isa<ZExtInst> (I)) && AllUsesAreGEP(&I)) {
	/* 
	 *  This optimization tries to reduce the number variables within
	 *  a basic block. This will put less pressure on the numerical
	 *  abstract domain later on. Search for this idiom: 
	 *  %_14 = zext i8 %_13 to i32
	 *  %_15 = getelementptr inbounds [10 x i8]* @_ptr, i32 0, i32 %_14
	 */
	return;
      }
      
      if (AllUsesAreNonTrackMem (&I) || AllUsesAreIndirectCalls (&I)) {
	return;
      }
      
      varname_t dst = GET_VAR(I, m_lfac);
      
      // -- INTEGER OR BOOLEAN CAST
      if (isIntCast(I)) {
	if (I.getSrcTy() == I.getDestTy()) {
	  // assume the frontend removes useless casts.
	  CRABLLVM_WARNING("translation does not support non-op integer casts");
	  havoc(dst, m_bb);
	} else {
	  if (CrabBoolAsInt) {
	    if (isIntToBool(I)) {
	      // OVER-APPROXIMATION
	      m_bb.havoc(dst);
	      m_bb.assume(z_lin_exp_t(dst) >= z_lin_exp_t(0));
	      m_bb.assume(z_lin_exp_t(dst) <= z_lin_exp_t(1));
	    } else {
	      // it might be UNSOUND in cases like:
	      //- if sext i1 true to i8 then it will assign 1 to dst
	      // rather than -1 (the actual result of sext).
	      //- if zext i8 -128 to i32 then it will assign -128
	      // to dst rather 128 (the actual result of zext)
	      if (boost::optional<crabNumLit> lsrc = m_lfac.getNumLit(*I.getOperand(0))) {
		m_bb.assign(dst, (*lsrc).getExp());
	      }
	    } 
	  } else {
	    if (isBoolToInt(I)) {
	      if (boost::optional<crabBoolLit> lsrc = m_lfac.getBoolLit(*I.getOperand(0))) {
		if ((*lsrc).isConst()) {
		  varname_t tmp = FRESH_VAR(m_lfac);
		  m_bb.bool_assign(tmp, (*lsrc).isTrue() ? CST_TRUE: CST_FALSE);
		  if (isa<SExtInst>(I)) {
		    m_bb.sext(tmp, I.getSrcTy()->getIntegerBitWidth(),
			      dst, I.getDestTy()->getIntegerBitWidth());
		  } else if (isa<ZExtInst>(I)) {
		    m_bb.zext(tmp, I.getSrcTy()->getIntegerBitWidth(),
			      dst, I.getDestTy()->getIntegerBitWidth());
		  }
		} else {
		  if (isa<SExtInst>(I)) {
		    m_bb.sext((*lsrc).getVar(), I.getSrcTy()->getIntegerBitWidth(),
			      dst, I.getDestTy()->getIntegerBitWidth());
		  } else if (isa<ZExtInst>(I)) {
		    m_bb.zext((*lsrc).getVar(), I.getSrcTy()->getIntegerBitWidth(),
			      dst, I.getDestTy()->getIntegerBitWidth());
		  }
		}
	      }
	    } else { // src is in integer
	      if (boost::optional<crabNumLit> lsrc = m_lfac.getNumLit(*I.getOperand(0))) {
		if ((*lsrc).isVar()) {
		  if (isa<SExtInst>(I)) {
		    m_bb.sext((*lsrc).getVar(), I.getSrcTy()->getIntegerBitWidth(),
			      dst, I.getDestTy()->getIntegerBitWidth());
		  } else if (isa<ZExtInst>(I)) {
		    m_bb.zext((*lsrc).getVar(), I.getSrcTy()->getIntegerBitWidth(),
			      dst, I.getDestTy()->getIntegerBitWidth());
		    
		  } else if (isa<TruncInst>(I)) {
		    m_bb.truncate((*lsrc).getVar(), I.getSrcTy()->getIntegerBitWidth(),
				  dst, I.getDestTy()->getIntegerBitWidth());
		    
		  } else {
		    llvm_unreachable("ERROR: unexpected cast operation");		    
		  }
		} else {
		  varname_t tmp = FRESH_VAR(m_lfac);
		  m_bb.assign(tmp, (*lsrc).getNum());
		  if (isa<SExtInst>(I)) {
		    m_bb.sext(tmp, I.getSrcTy()->getIntegerBitWidth(),
			      dst, I.getDestTy()->getIntegerBitWidth());
		  } else if (isa<ZExtInst>(I)) {
		  m_bb.zext(tmp, I.getSrcTy()->getIntegerBitWidth(),
			    dst, I.getDestTy()->getIntegerBitWidth());
		  
		  } else if (isa<TruncInst>(I)) {
		    m_bb.truncate(tmp, I.getSrcTy()->getIntegerBitWidth(),
				  dst, I.getDestTy()->getIntegerBitWidth());
		  } else {
		  llvm_unreachable("ERROR: unexpected cast operation");
		  }
		}
	      }
	    }
	  }
	}
	return;
      }

      // -- POINTER CAST      
      if (isPointerCast(I)) {
	if (isa<PtrToIntInst>(I)) {
	  CRABLLVM_WARNING("translation skipped pointer to integer cast");
	} else if (isa<IntToPtrInst>(I)) {
	  CRABLLVM_WARNING("translation skipped integer to pointer cast");
	} else if (isa<BitCastInst>(I) && isPointer(*I.getOperand(0), m_lfac.get_track())) {
	  boost::optional<crabPtrLit> lsrc = m_lfac.getPtrLit(*I.getOperand(0));
	  if (lsrc) {
	    if ((*lsrc).isNull())
	      m_bb.ptr_null(dst);
	    else if ((*lsrc).isVar())
	      m_bb.ptr_assign(dst, (*lsrc).getVar(), z_lin_exp_t(0));
	    else {} // unreachable
	    return;
	  }
	  CRABLLVM_WARNING("translation skipped " << I);	  
	}
      }
      
      havoc(dst, m_bb);
    }

    // Analysis of select instructions is cumbersome since it requires
    // a sequence of assume and join operations. Moreover, if many
    // select instructions appear in the same block its analysis can
    // be very inefficient due to the high number of joins.
    // 
    // If possible the simplest solution is to get rid of select
    // instructions. This can be done by adding option
    // --lower-select. This option will remove select instructions at
    // the expense of adding new basic blocks although hopefully the
    // llvm frontend will simplify them. If this is not possible or
    // undesirable then we try to deal with the select instruction
    // here.
    void visitSelectInst(SelectInst &I) {
      if (!isTracked(I, m_lfac.get_track())) return;
      
      varname_t lhs = GET_VAR(I, m_lfac);
      
      if (isPointer(I, m_lfac.get_track())) {
	// We don't even bother with pointers
	CRABLLVM_WARNING("skipped " << I << "\n" << "Enable --lower-select.");
	havoc(lhs, m_bb);
	return;
      }
      
      Value& cond = *I.getCondition ();
      
      if (isBool(I)) {
	// --- All operands are BOOL
	boost::optional<crabBoolLit> l1 = m_lfac.getBoolLit(*I.getTrueValue ());
	if (!l1) {havoc(lhs, m_bb); return;}
	boost::optional<crabBoolLit> l2 = m_lfac.getBoolLit(*I.getFalseValue ()); 
	if (!l2) {havoc(lhs, m_bb); return;}
    
	crabBoolLit b1 = *l1;
	crabBoolLit b2 = *l2;
	
	// -- simple cases first: we know the condition is either true or false
	if (ConstantInt *ci = dyn_cast<ConstantInt> (&cond)) {
	  if (ci->isOne ()) {
	    if (b1.isConst())
	      m_bb.bool_assign(lhs, (b1.isTrue()? CST_TRUE: CST_FALSE));
	    else  
	      m_bb.bool_assign(lhs, b1.getVar());
	    return;
	  } else if (ci->isZero ()) {
	    if (b2.isConst())
	      m_bb.bool_assign(lhs, (b2.isTrue()? CST_TRUE: CST_FALSE));
	    else  
	      m_bb.bool_assign(lhs, b2.getVar());
	    return;
	  }
	}

	// -- general case: we don't know whether condition is true or not.
	if (b1.isConst() && b2.isConst()) {
	  varname_t tt_v = FRESH_VAR(m_lfac);
	  varname_t ff_v = FRESH_VAR(m_lfac);
	  m_bb.bool_assign(tt_v, (b1.isTrue()? CST_TRUE: CST_FALSE));
	  m_bb.bool_assign(ff_v, (b2.isTrue()? CST_TRUE: CST_FALSE));
	  m_bb.bool_select(lhs, GET_VAR(cond,m_lfac), tt_v, ff_v);	
	} else if (b1.isConst()) {
	  varname_t tt_v = FRESH_VAR(m_lfac);
	  m_bb.bool_assign(tt_v, (b1.isTrue()? CST_TRUE: CST_FALSE));
	  m_bb.bool_select(lhs, GET_VAR(cond,m_lfac), tt_v, b2.getVar());	
	} else if (b2.isConst()) {
	  varname_t ff_v = FRESH_VAR(m_lfac);
	  m_bb.bool_assign(ff_v, (b2.isTrue()? CST_TRUE: CST_FALSE));
	  m_bb.bool_select(lhs, GET_VAR(cond,m_lfac), b1.getVar(), ff_v);	
	} else {
	  m_bb.bool_select(lhs, GET_VAR(cond,m_lfac), b1.getVar(), b2.getVar());
	}
      } else if (isInteger(I)) {
	
	// --- All operands except the condition are INTEGERS
	boost::optional<crabNumLit> l1 = m_lfac.getNumLit(*I.getTrueValue ());
	if (!l1) {havoc(lhs, m_bb); return;}
	boost::optional<crabNumLit> l2 = m_lfac.getNumLit(*I.getFalseValue ()); 
	if (!l2) {havoc(lhs, m_bb); return;}
    
	z_lin_exp_t op1 = (*l1).getExp();
	z_lin_exp_t op2 = (*l2).getExp();
	
	// -- simple cases first: we know the condition is either true or false
	if (ConstantInt *ci = dyn_cast<ConstantInt> (&cond)) {
	  if (ci->isOne ()) {
	    m_bb.assign (lhs, op1);
	    return;
	  } else if (ci->isZero ()) {
	    m_bb.assign (lhs, op2);
	    return;
	  }
	}
	
	// -- general case: we don't know whether the condition is true or not
	if (CmpInst* CI = dyn_cast<CmpInst> (&cond)) {
	  if (auto cst_opt = cmpInstToCrabInt(*CI, m_lfac)) {
	    m_bb.select (lhs, *cst_opt, op1, op2);
	    return;
	  }
	}
	if (CrabBoolAsInt) {      
	  //  select will transform the select condition to "cond >= 1"
	  m_bb.select(lhs, GET_VAR(cond, m_lfac), op1, op2);
	} else {
	  // The condition is a boolean but neither select or
	  // bool_select are the right choice. The latter is only when
	  // all operands are booleans. The former will have this form
	  // (select (x:= cond >=1 ? e1: e2). This will be propagated
	  // only to numerical domain which doesn't know anything about
	  // cond. One solution is to zext cond to an integer. But maybe
	  // another solution is to allow select to be a variable rather
	  // than constraint.
          #if 1
	  auto icond = FRESH_VAR(m_lfac);
	  m_bb.zext(GET_VAR(cond, m_lfac), 1, icond, 64 /*any bitwdith >1*/);
	  m_bb.select(lhs, icond, op1, op2);
          #else
	  CRABLLVM_WARNING("skipped " << I << "\n" <<
			   "Crab select does not support natively boolean conditions.\n" << 
			   "Meanwhile, enable --lower-select or --crab-bool-as-int");
	  havoc(lhs, m_bb);
          #endif 
	}
      } 
    }
    
    void visitGetElementPtrInst (GetElementPtrInst &I) {
      if (!isTracked(I, m_lfac.get_track())) return;
      
      CRAB_LOG ("cfg-gep", llvm::errs () << "Translating " << I << "\n");

      region_t r = GET_REGION(I, &I); 
      if (isGlobalSingleton(r)) {
	CRAB_LOG("cfg-gep", llvm::errs() << "Skipped singleton region\n");
	return;
      }
      
      boost::optional<crabPtrLit> ptr_lit = m_lfac.getPtrLit(*I.getPointerOperand ());
      if (!ptr_lit) {
        havoc(GET_VAR(I, m_lfac), m_bb);
        return;
      }
      
      if ((*ptr_lit).isNull()) {
	CRABLLVM_WARNING(I << " doing pointer arithmetic with null pointer.");
        havoc(GET_VAR(I, m_lfac), m_bb);
        return;
      }

      varname_t ptr = (*ptr_lit).getVar();
      varname_t res = GET_VAR(I, m_lfac);

      // -- translation if the GEP offset is constant
      unsigned BitWidth = m_dl->getPointerTypeSizeInBits(I.getType());
      APInt Offset(BitWidth, 0);
      if (I.accumulateConstantOffset (*m_dl, Offset)) {
        z_lin_exp_t offset (toMpz (Offset).get_str ());
	m_bb.ptr_assign (res, ptr, offset);
	CRAB_LOG("cfg-gep",
		 crab::outs() << "-- " << res << ":=" << ptr  << "+" << offset << "\n");
        return;
      }

      // -- translation if symbolic GEP offset
      // If here, we know that there is at least one non-zero,
      // symbolic index.
      SmallVector<const Value*, 4> ps;
      SmallVector<const Type*, 4> ts;
      gep_type_iterator typeIt = gep_type_begin (I);
      for (unsigned i = 1; i < I.getNumOperands (); ++i, ++typeIt) {
        // strip zext/sext if there is one
        if (const ZExtInst *ze = dyn_cast<const ZExtInst> (I.getOperand (i)))
          ps.push_back (ze->getOperand (0));
        else if(const SExtInst *se = dyn_cast<const SExtInst>(I.getOperand(i)))
          ps.push_back (se->getOperand (0));
        else 
          ps.push_back (I.getOperand (i));
        ts.push_back (*typeIt);
      }

      bool already_assigned = false;
      for (unsigned i = 0; i < ps.size (); ++i) {
        if (const StructType *st = dyn_cast<const StructType> (ts [i])) {
          if (const ConstantInt *ci = dyn_cast<const ConstantInt> (ps [i])) {
            ikos::z_number offset (fieldOffset (st, ci->getZExtValue ()));
	    m_bb.ptr_assign (res, (!already_assigned) ? ptr : res, offset);
	    CRAB_LOG("cfg-gep",
		     if (!already_assigned) {
		       crab::outs() << res << ":=" << ptr << "+" << offset << "\n";
		     } else {
		       crab::outs() << res << ":=" << res << "+" << offset << "\n";
		     }); 
	    already_assigned = true;
          } else {
            llvm_unreachable ("ERROR: GEP index expected only to be an integer");
	  }
        }
        else if (const SequentialType *seqt = dyn_cast<const SequentialType>(ts[i])) {
          if (const ConstantInt *ci = dyn_cast<const ConstantInt> (ps [i])) {
            if (ci->isZero ())
              continue;
          }
	  boost::optional<crabNumLit> lp = m_lfac.getNumLit(*ps[i]);
	  if (!lp) llvm_unreachable ("ERROR: unexpected GEP index");
	  z_lin_exp_t offset((*lp).getExp() *
			     ikos::z_number(storageSize(seqt->getElementType())));
	  m_bb.ptr_assign(res, (!already_assigned) ? ptr : res, offset);
	  CRAB_LOG("cfg-gep",
		   if (!already_assigned) {
		     crab::outs() << res << ":=" << ptr << "+" << offset << "\n";
		   } else {
		     crab::outs() << res << ":=" << res << "+" << offset << "\n";
		   }); 
	  already_assigned = true;
        }
      }
    }

    void visitLoadInst (LoadInst &I) {

      varname_t lhs = GET_VAR(I, m_lfac);      
      boost::optional<crabPtrLit> ptr_lit = m_lfac.getPtrLit(*I.getPointerOperand ());
      if (!ptr_lit) {
	CRABLLVM_WARNING("unexpected pointer operand " << I);
	havoc(lhs, m_bb);
	return;
      }
      if ((*ptr_lit).isNull()) {
	// OVER-APPROXIMATING: we don't translate this case	
	CRABLLVM_WARNING(I << " is possibly dereferencing a null pointer");
	havoc(lhs, m_bb);
	return;
      }
      
      if (isPointer(I, m_lfac.get_track())) {
	// -- lhs is a pointer -> add pointer statement
	m_bb.ptr_load(lhs, (*ptr_lit).getVar());
	return;
      } else if (m_lfac.get_track() >= ARR && isInteger(I)) {
	// -- lhs is an integer -> add array statement
	
	// TODO: array of booleans.
	// TODO: array of pointers. Note that we use the same variable
	// name (lhs) whether the load instruction assigns to a
	// pointer or integer variable. This is OK because these two
	// cases cannot happen simultaneously.  However, if we allow
	// array of pointers we need to have two different variable
	// names.
	region_t r = GET_REGION(I, I.getPointerOperand ()); 
	if (!(r.isUnknown ())) {
	  if (const Value* s = isGlobalSingleton (r)) {
	    m_bb.assign (lhs, z_lin_exp_t(GET_VAR(*s, m_lfac)));
	  } else {
	    // We havoc the array index if we are not tracking it.
	    if (!isPointer(*I.getPointerOperand(), m_lfac.get_track())) {
	      m_bb.havoc((*ptr_lit).getVar());
	    }	    
	    z_lin_exp_t idx((*ptr_lit).getVar());
	    m_bb.array_load(lhs,
			    GET_VAR_FROM_REGION(r, m_lfac), ARR_INT_TYPE,
			    idx,
			    m_dl->getTypeAllocSize (I.getType()));
	  }
	  return;
	}
      }
      
      if (isTracked(I, m_lfac.get_track()) && !isPointer(I, m_lfac.get_track()))
	havoc(lhs, m_bb);
    }

    void visitStoreInst (StoreInst &I) {

      boost::optional<crabPtrLit> ptr_lit = m_lfac.getPtrLit(*I.getPointerOperand());
      if (!ptr_lit) {
	CRABLLVM_WARNING("unexpected pointer operand " << I);
	return;
      }
      
      if ((*ptr_lit).isNull()) {
	// OVER-APPROXIMATING: we don't translate this case
	CRABLLVM_WARNING(I << " is possibly dereferencing a null pointer");
	return;
      }
            
      if (isPointer(*I.getValueOperand(), m_lfac.get_track())) {
	// -- value is a pointer -> add pointer statement

	boost::optional<crabPtrLit> val_lit = m_lfac.getPtrLit(*I.getValueOperand());
	if (!val_lit) {
	  CRABLLVM_WARNING("unexpected value operand " << I);
	  havoc((*ptr_lit).getVar(), m_bb);
	} else if (!(*val_lit). isNull()) {
	  // OVER-APPROXIMATION: we ignore the case if we store a null pointer. In
	  // most cases, it will be fine since typical pointer analyses
	  // ignore that case but it might be imprecise with certain
	  // analyses.
	  m_bb.ptr_store((*ptr_lit).getVar(), (*val_lit).getVar());
	}
      } else if (m_lfac.get_track() >= ARR && isInteger(*I.getValueOperand())){
	// -- value is an integer -> add array statement
	// TODO: array of booleans
	// TODO: arrays of pointers
	region_t r = GET_REGION(I, I.getPointerOperand()); 
	if (!r.isUnknown()) {
	  boost::optional<crabNumLit> val_lit = m_lfac.getNumLit(*I.getValueOperand());

	  if (!val_lit) {
	    CRABLLVM_WARNING("unexpected value operand " << I);
	    havoc((*ptr_lit).getVar(), m_bb);
	    return;
	  }
	  
	  if (const Value* s = isGlobalSingleton(r)) {
	    m_bb.assign((*ptr_lit).getVar(), (*val_lit).getExp());
	  } else {
	    Type* ty = I.getOperand (0)->getType();
	    // We havoc the array index if we are not tracking it.
	    if (!isPointer(*I.getPointerOperand(), m_lfac.get_track())) {
	      m_bb.havoc((*ptr_lit).getVar());
	    }	    
	    z_lin_exp_t idx((*ptr_lit).getVar());
	    if ((*val_lit).isNum()) {
	      m_bb.array_store (GET_VAR_FROM_REGION(r, m_lfac), ARR_INT_TYPE, 
				idx, (*val_lit).getNum(),
				m_dl->getTypeAllocSize(ty),
				r.getSingleton () != nullptr);

	    } else {
	      m_bb.array_store (GET_VAR_FROM_REGION(r, m_lfac), ARR_INT_TYPE, 
				idx, (*val_lit).getVar(),
				m_dl->getTypeAllocSize(ty),
				r.getSingleton () != nullptr);
	    }
	  }
	}
      }
    }

    void visitAllocaInst (AllocaInst &I) {
      
      if (isPointer(I, m_lfac.get_track())) {
	m_bb.ptr_new_object(GET_VAR(I, m_lfac), m_object_id++);
      }

      if (m_lfac.get_track() >= ARR && CrabArrayInit &&
	  isIntArray(*I.getAllocatedType())) {
	// TODO: array of booleans
	// TODO: array of pointers
	region_t r = GET_REGION(I,&I);
	if (!r.isUnknown()) {
	  // "Initialization hook": nodes which do not have an
	  // explicit initialization are initially undefined. Instead,
	  // we assume they are zero initialized so that Crab's array
	  // smashing can infer something meaningful. This is correct
	  // because in presence of undefined behaviour we can do
	  // whatever we want.
	  unsigned elem_size = storageSize (I.getAllocatedType()->getArrayElementType());
	  if (elem_size > 0) {
	    ikos::z_number init_val(0); /*any value we want*/
	    z_lin_exp_t lb_idx(ikos::z_number(0));
	    unsigned num_elems = I.getAllocatedType()->getArrayNumElements();
	    z_lin_exp_t ub_idx((num_elems - 1) * elem_size);
	    m_bb.array_assume (GET_VAR_FROM_REGION(r,m_lfac),
			       ARR_INT_TYPE, elem_size,
			       lb_idx, ub_idx, init_val);
	  }
	}
      }
    }

    void visitReturnInst (ReturnInst &I) {
      // translated elsewhere
    }

    void visitCallInst (CallInst &I) {
      CallSite CS (&I);
      const Value *calleeV = CS.getCalledValue ();
      const Function *callee =dyn_cast<Function>(calleeV->stripPointerCasts());
      
      if (!callee) {         
	if (I.isInlineAsm()) {
	  // -- inline asm: do nothing 
	} else {
	  // -- unresolved indirect call
	  CRABLLVM_WARNING("skipped indirect call. Enable --devirt-functions");
	  
	  if (!I.getType()->isVoidTy() && isTracked(I, m_lfac.get_track())) {
	    // havoc return value
	    havoc(GET_VAR(I, m_lfac), m_bb);
	  }
	}
	return;
      }

      // -- ignore any shadow functions created by seahorn
      if (callee->getName().startswith ("shadow.mem")) return;
      if (callee->getName().equals ("seahorn.fn.enter")) return;

      if (isVerifierCall(callee)) {
        doVerifierCall(I);
        return;
      }
      
      if (isAllocationFn(&I, m_tli)){
        doAllocFn(I);
        return;
      }

      if (CrabArrayInit && (isZeroInitializer(callee) || isIntInitializer(callee))) {
	doInitializer(I);
	return;
      }
      
      if (callee->isIntrinsic ()) {
        if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(&I)) {
          doMemIntrinsic(*MI);
        } else {
	  if (!I.getType()->isVoidTy() && isTracked(I, m_lfac.get_track())) {
	    // -- havoc return value of the intrinsics
            havoc(GET_VAR(I, m_lfac), m_bb);
	  }
	}
	return;
      }
      

      /**
       * Intra-procedural translation
       **/      
      if (callee->isDeclaration() || callee->isVarArg() || !m_is_inter_proc) {
        // -- havoc return value
        if (!I.getType()->isVoidTy() && isTracked(I, m_lfac.get_track())) {
	  havoc(GET_VAR(I, m_lfac), m_bb);
	}
        // -- havoc all modified regions by the callee
        if (m_lfac.get_track() == ARR) {
          region_set_t mods = m_mem.getModifiedRegions (I);
          for (auto a : mods) {
            m_bb.havoc (GET_VAR_FROM_REGION(a, m_lfac));
          }
        }
        return;
      }

      /**
       * Inter-procedural translation
       * 
       * Translate a LLVM callsite 
       *     o := foo(i1,...,i_n) 
       * 
       * into a crab callsite
       *     (o,a_o1,...,a_om) := foo(i1,...,in,a_i1,...,a_in) where
       *  
       *    - a_i1,...,a_in are read-only and modified arrays by foo.
       *    - a_o1,...,a_om are modified and new arrays created inside foo.
       **/

      
      std::vector<typed_variable_t> inputs, outputs;
      
      // -- add the actual parameters of the llvm callsite: i1,...in.
      for (auto &a : boost::make_iterator_range (CS.arg_begin(),
                                                 CS.arg_end())) {
        Value *v = a.get();
        if (!isTracked(*v, m_lfac.get_track())) continue;
        inputs.push_back (normalizeFuncParamOrRet(*v, m_bb, m_lfac));
      }
      
      // -- add the return value of the llvm calliste: o
      if ((getCrabType(I.getType ()) != UNK_TYPE) && isTracked(I, m_lfac.get_track())) {
	outputs.push_back(typed_variable_t(GET_VAR(I, m_lfac),
					   getCrabType(I.getType())));
      }
      
      if (m_lfac.get_track() == ARR) {
	// -- add the input and output array parameters a_i1,...,a_in
	// -- and a_o1,...,a_om.
        region_set_t onlyreads = m_mem.getOnlyReadRegions (I);
        region_set_t mods = m_mem.getModifiedRegions (I);
        region_set_t news = m_mem.getNewRegions (I);
        
        CRAB_LOG("cfg-mem",
                 llvm::errs() << "Callsite " << I << "\n"
 		              << "\tOnly-Read regions: " << onlyreads << "\n"
		              << "\tModified regions: " << mods << "\n"
		              << "\tNew regions:" << news << "\n");

        // -- add only read regions as array input parameters
        for (auto a: onlyreads) {
	  // TODO: adapt code if region contains booleans or pointers
          if (const Value* s = isGlobalSingleton(a)) {
            inputs.push_back(typed_variable_t(GET_VAR(*s, m_lfac), INT_TYPE));
	  } else {
            inputs.push_back(typed_variable_t(GET_VAR_FROM_REGION(a, m_lfac),
					      ARR_INT_TYPE));
	  }
        }
	
        // -- add modified regions as both input and output parameters
        for (auto a: mods) {
          if (news.find(a) != news.end()) continue;

	  #if 0
          // given "x" add the statements "x_in = x; x = *;" just
          // before the callsite.	  
          varname_t a_in = FRESH_VAR(m_lfac);
          if (const Value* s = isGlobalSingleton(a)) {
	    boost::optional<crabPtrLit> ptr_lit = m_lfac.getPtrLit(*s);
	    if (ptr_lit && (*ptr_lit).isVar()) {
	      // -- If singleton we don't add a pointer statement
	      // -- but instead an integer one.
	      // TODO: adapt code if region contains booleans or pointers
	      m_bb.assign(a_in, z_lin_exp_t((*ptr_lit).getVar()));
	    } else {
	      CRABLLVM_WARNING("unexpected pointer region at callsite");
	    }
	    m_bb.havoc(GET_VAR(*s, m_lfac));
          } else {
	    // TODO: adapt code if region contains booleans or pointers	    
            m_bb.array_assign(a_in, GET_VAR_FROM_REGION(a, m_lfac), ARR_INT_TYPE); 
            m_bb.havoc(GET_VAR_FROM_REGION(a, m_lfac)); 
          }
	  #else
	  varname_t a_in = GET_VAR_FROM_REGION(a, m_lfac);
	  #endif
	  
          // input version
	  // TODO: adapt code if region contains booleans or pointers	    	  
          if (const Value* s = isGlobalSingleton(a)) {
            inputs.push_back (typed_variable_t(a_in, INT_TYPE));  
	  } else {
            inputs.push_back (typed_variable_t(a_in, ARR_INT_TYPE));
	  }

          // output version
	  // TODO: adapt code if region contains booleans or pointers	  
          if (const Value* s = isGlobalSingleton(a)) {
            outputs.push_back(typed_variable_t(GET_VAR(*s, m_lfac), INT_TYPE));
	  } else  {
            outputs.push_back(typed_variable_t(GET_VAR_FROM_REGION(a, m_lfac),
					       ARR_INT_TYPE));
	  }
          
        }	
        // -- add more output parameters
        for (auto a: news) {
	  // TODO: adapt code if region contains booleans or pointers	  	  
          outputs.push_back (typed_variable_t(GET_VAR_FROM_REGION(a, m_lfac),
					      ARR_INT_TYPE));
	}
      }
      // -- Finally, add the callsite
      m_bb.callsite(GET_VAR(*callee, m_lfac), outputs, inputs);
      
    }

    void visitUnreachableInst (UnreachableInst &I) {
      m_bb.unreachable();
    }

    /// base case. if all else fails.
    void visitInstruction (Instruction &I) {
      if (!isTracked(I, m_lfac.get_track())) return;
      havoc(GET_VAR(I, m_lfac), m_bb);
    }
    
  }; // end class

  CfgBuilder::CfgBuilder(Function& func, 
			 llvm_variable_factory &vfac,
			 HeapAbstraction &mem, 
			 tracked_precision tracklev,
			 bool isInterProc,
			 const TargetLibraryInfo* tli)
    : m_is_cfg_built(false),                          
      m_func(func),
      m_vfac(vfac), m_mem(mem), m_tracklev(tracklev), m_id(0),      
      m_cfg(boost::make_shared<cfg_t>(&m_func.getEntryBlock(), tracklev)),
      m_is_inter_proc(isInterProc),
      m_dl(func.getParent()->getDataLayout ()),
      m_tli(tli) { }

  CfgBuilder::~CfgBuilder () { 
    for (BasicBlock* B: m_fake_assume_blocks) {
      delete B; // B->eraseFromParent ();
    }
  }

  cfg_ptr_t CfgBuilder::getCfg () { 
    if (!m_is_cfg_built) {
      build_cfg ();
      m_is_cfg_built = true;
    }
    return m_cfg;
  }
  
  CfgBuilder::opt_basic_block_t CfgBuilder::lookup (const BasicBlock &B) {  
    BasicBlock* BB = const_cast<BasicBlock*> (&B);
    llvm_bb_map_t::iterator it = m_bb_map.find (BB);
    if (it == m_bb_map.end ())
      return CfgBuilder::opt_basic_block_t ();
    else
      return CfgBuilder::opt_basic_block_t (it->second);
  }

  void CfgBuilder::add_block (BasicBlock &B)
  {
    assert (!lookup(B));
    BasicBlock *BB = &B;
    basic_block_t &bb = m_cfg->insert ( BB);
    m_bb_map.insert (llvm_bb_map_t::value_type (BB, bb));
  }  

  basic_block_t& CfgBuilder::
  add_block_in_between (basic_block_t &src, basic_block_t &dst,
			const BasicBlock* BB) {

    assert (m_bb_map.find (BB) == m_bb_map.end()); 

    basic_block_t &bb = m_cfg->insert (BB);

    src -= dst;
    src >> bb;
    bb >> dst;

    return bb;
  }  


  void CfgBuilder::add_edge (BasicBlock &S, const BasicBlock &D) {
    opt_basic_block_t SS = lookup(S);
    opt_basic_block_t DD = lookup(D);
    assert (SS && DD);
    *SS >> *DD;
  }  

  std::string create_bb_name(unsigned &id, std::string prefix = "") {
    if (prefix == "") prefix = std::string("__@bb_");
    ++id;
    std::string id_str = std::to_string(id);
    return prefix + id_str;
  }
  
  // add a fake llvm BasicBlock into the function: this BasicBlock is
  // needed because the translation cannot generate a Crab basic block
  // without being associated with a llvm BasicBlock.  We do not
  // attach it to a parent so we need to free them later.
  const BasicBlock* CfgBuilder::create_fake_block(LLVMContext &ctx, 
						  const Twine &name,
						  Function *parent) {

    BasicBlock* B = BasicBlock::Create (ctx, name, nullptr);
    // IRBuilder<> Builder (B);
    // Builder.CreateUnreachable (); // to make the block well formed.
    m_fake_assume_blocks.push_back (B);
    return B;
  }

  //! return the new block inserted between src and dest if any
  CfgBuilder::opt_basic_block_t
  CfgBuilder::execBr (BasicBlock &src, const BasicBlock &dst) {
    crabLitFactory lfac(m_vfac, m_tracklev);    
    // -- the branch condition
    if (const BranchInst *br=dyn_cast<const BranchInst>(src.getTerminator())) {
      if (br->isConditional ()) {
        opt_basic_block_t Src = lookup(src);
        opt_basic_block_t Dst = lookup(dst);
        assert (Src && Dst);

        const BasicBlock* BB = create_fake_block (m_func.getContext (),
						  create_bb_name (m_id),
						  &m_func);
                                                                      
        basic_block_t &bb = add_block_in_between (*Src, *Dst, BB);
        
        const Value &c = *br->getCondition ();
        if (const ConstantInt *ci = dyn_cast<const ConstantInt> (&c)) {
          if ((ci->isOne()  && br->getSuccessor(0) != &dst) ||
              (ci->isZero() && br->getSuccessor(1) != &dst))
            bb.unreachable();
        } else {
          bool isNegated = (br->getSuccessor (1) == &dst);
	  bool lower_cond_as_bool = false;
          if (CmpInst* CI = dyn_cast<CmpInst> (& const_cast<Value&>(c)))  {
	    if (isBool(*(CI->getOperand(0))) && isBool(*(CI->getOperand(1)))) {
	      lower_cond_as_bool = true;
	    } else if (isInteger(*(CI->getOperand(0))) && isInteger(*(CI->getOperand(1)))) {
	      if (auto cst_opt = cmpInstToCrabInt(*CI, lfac, isNegated)) {
		bb.assume (*cst_opt); 
	      }
	    } else if (isPointer(*(CI->getOperand(0)), lfac.get_track()) &&
		       isPointer(*(CI->getOperand(1)), lfac.get_track())) {
	      // TODO: add ptr_assume statement	      
	      CRABLLVM_WARNING("translation skipped comparison between pointers");
	    }
	    if (c.hasNUsesOrMore (2)) {
	      // If I is used by another instruction apart from a
	      // branch condition.
	      lower_cond_as_bool = true;
	    }
          } else  {
            // If the boolean condition is passed directly (e.g.,
            // after optimization) as a function argument.
	    lower_cond_as_bool = true;
	  }

	  if (lower_cond_as_bool) {
            varname_t lhs = GET_VAR(c, lfac);
	    if (isNegated)
	      bb.bool_not_assume(lhs);
	    else
	      bb.bool_assume(lhs);
	  }
        }
        return opt_basic_block_t(bb);
      }
      else add_edge (src,dst);
    }
    return opt_basic_block_t();    
  }

  void CfgBuilder::build_cfg() {

    crab::ScopedCrabStats __st__("CFG Construction");

    std::vector<BasicBlock*> blocks;
    for (auto &B : m_func) { 
      add_block (B); 
      blocks.push_back (&B);
    }

    crabLitFactory lfac(m_vfac, m_tracklev);

    basic_block_t *ret_block = nullptr;
    boost::optional<typed_variable_t> ret_val;
    
    
    // execBr can add new BasicBlock's in m_func and we do not want to
    // iterate over them.
    for (BasicBlock* B : blocks) {
      opt_basic_block_t BB = lookup (*B);
      if (!BB) continue;

      // -- build a CFG block ignoring branches, phi-nodes, and return
      CrabInstVisitor v (lfac, m_mem, m_dl, m_tli, *BB, m_is_inter_proc);
      v.visit (*B);

      // -- process the exit block of the function and its returned value.
      if (ReturnInst *RI = dyn_cast<ReturnInst> (B->getTerminator ())) {
	if (ret_block)
	  llvm_unreachable("LLVM UnifyFunctionExitNodes pass should have been run first");
	
        basic_block_t &bb = *BB;
        ret_block = &bb;
	m_cfg->set_exit(ret_block->label());
	
	if (m_is_inter_proc) {
	  if (Value * RV = RI->getReturnValue()) {
	    if (isTracked(*RV, lfac.get_track())) {
	      ret_val = normalizeFuncParamOrRet(*RV, *ret_block, lfac);
	      bb.ret((*ret_val).first, (*ret_val).second);
	    }
	  }
	}
	
      } else {
        for (const BasicBlock *dst : succs (*B)) {
          // -- move branch condition in bb to a new block inserted
          //    between bb and dst
          opt_basic_block_t mid_bb = execBr (*B, *dst);

          // -- phi nodes in dst are translated into assignments in
          //    the predecessor
          CrabPhiVisitor v (lfac, m_mem, (mid_bb ? *mid_bb : *BB), *B);
          v.visit (const_cast<BasicBlock &>(*dst));
        }
      }
    }

        
    /// Allocate arrays with initial values 
    if (m_tracklev == ARR && CrabArrayInit && CrabUnsoundArrayInit) {
      // getNewRegions returns all the new nodes created by the
      // function (via malloc-like functions) except if the function
      // is main.
      basic_block_t & entry = m_cfg->get_node (m_cfg->entry ());
      auto news =  m_mem.getNewRegions (m_func);
      for (auto n: news) {
	entry.set_insert_point_front ();
	// TODO: we can apply here the "Initialization hook".
      }
    }

    /// Add function declaration
    if (m_is_inter_proc && !m_func.isVarArg ()) {

      /**
       * Translate LLVM function declaration 
       *   o_ty foo (i1,...,in)
       * 
       * into a crab function declaration
       *
       *   o, a_o1,...,a_om foo (i1,...,in,a_i1,...,a_in) where
       *
       *   - o is the **returned value** of the function (translation
       *     ensures there is always one return instruction and the
       *     returned value is a variable, i.e., cannot be a
       *     constant).
       *
       *   - a_i1,...,a_in are read-only and modified arrays in function foo
       * 
       *   - a_o1,....,a_om are modified and new arrays created inside
       *     foo.
       *
       * It ensures that the set {a_i1,...,a_in} is disjoint from
       * {a_o1,....,a_om}, otherwise crab will complain.
       **/
      
      std::vector<typed_variable_t> inputs, outputs;

      basic_block_t & entry = m_cfg->get_node (m_cfg->entry ());
      
      // -- add the returned value of the llvm function: o
      if (ret_val) {
	outputs.push_back(*ret_val);
      }
      
      // -- add input parameters i1,...,in
      for (Value &arg : boost::make_iterator_range (m_func.arg_begin (),
						    m_func.arg_end ())) {
        if (!isTracked(arg, lfac.get_track())) continue;

	typed_variable_t i(GET_VAR(arg, lfac), getCrabType(arg.getType()));
	if (ret_val && i == *ret_val) {
	  // rename i to avoid having same name as output (crab requirement)
          varname_t fresh_i = FRESH_VAR(lfac);	  	  
	  if (i.second == BOOL_TYPE) {
	    entry.bool_assign(fresh_i, i.first);
	  } else if (i.second == INT_TYPE) {
	    entry.assign(fresh_i, z_lin_exp_t(i.first));
	  } else if (i.second == PTR_TYPE) {
	    entry.ptr_assign(fresh_i, i.first, z_lin_exp_t(0));
	  } else {
	    llvm_unreachable("ERROR: unexpected function parameter type\n");
	  }
	  inputs.push_back (typed_variable_t(fresh_i, i.second));
	} else {
	  inputs.push_back (i);
	}
      }

	
      if (m_tracklev == ARR && (!m_func.getName ().equals ("main"))) {
	
        // -- add the input and output array parameters 

        auto onlyreads = m_mem.getOnlyReadRegions (m_func);
        auto mods = m_mem.getModifiedRegions (m_func);
        auto news = m_mem.getNewRegions (m_func);

        CRAB_LOG("cfg-mem",
                 llvm::errs() << "Function " << m_func.getName () 
                              << "\n\tOnly-Read regions: " << onlyreads
                              << "\n\tModified regions: " << mods
                              << "\n\tNew regions:" << news << "\n");
		 
        // -- add only read regions as input parameters
        for (auto a: onlyreads) {
	  // TODO: adapt code if region contains booleans or pointers	  	  	  
          if (const Value* s = isGlobalSingleton (a)) {
            inputs.push_back(typed_variable_t(GET_VAR(*s, lfac), INT_TYPE));
	  } else {
            inputs.push_back(typed_variable_t(GET_VAR_FROM_REGION(a, lfac),
					      ARR_INT_TYPE));
	  }
        }

        // -- add input/output parameters
        for (auto a: mods) {

          if (news.find(a) != news.end()) continue;

	  #if 1
          varname_t a_in = FRESH_VAR(lfac);	  	  
          // -- for each parameter `a` we create a fresh version
          //    `a_in` where `a_in` acts as the input version of the
          //    parameter and `a` is the output version. Note that the
          //    translation of the function will not produce new
          //    versions of `a` since all array stores overwrite `a`.
          entry.set_insert_point_front ();
          if (const Value* s = isGlobalSingleton (a)) {
	    boost::optional<crabPtrLit> ptr_lit = lfac.getPtrLit(*s);	    	    
	    if (ptr_lit && (*ptr_lit).isVar()) {
	      //-- If singleton we don't add a pointer statement but
	      //-- instead an integer one.
	      // TODO: adapt code if region contains booleans or pointers	      
	      entry.assign((*ptr_lit).getVar(), z_lin_exp_t(a_in));
	    } else {
	      CRABLLVM_WARNING("Array function parameter is not a pointer variable");
	    }
	  } else {
	    // TODO: adapt code if region contains booleans or pointers	    
            entry.array_assign(GET_VAR_FROM_REGION(a, lfac), a_in, ARR_INT_TYPE);
	  }
	  #else
	  varname_t a_in = GET_VAR_FROM_REGION(a, lfac);
	  #endif 

          // input version
	  // TODO: adapt code if region contains booleans or pointers
          if (const Value *s = isGlobalSingleton (a)) {
            inputs.push_back(typed_variable_t(a_in, INT_TYPE));
	  } else {
	    inputs.push_back(typed_variable_t(a_in, ARR_INT_TYPE));
	  }
	  
          // output version
	  // TODO: adapt code if region contains booleans or pointers
          if (const Value* s = isGlobalSingleton (a)) { 
            outputs.push_back(typed_variable_t(GET_VAR(*s, lfac), INT_TYPE));
	  } else {
            outputs.push_back(typed_variable_t(GET_VAR_FROM_REGION(a, lfac),
					       ARR_INT_TYPE));
	  }
        }

        // -- add more output parameters
        for (auto a: news) {
	  // TODO: adapt code if region contains booleans or pointers
          outputs.push_back(typed_variable_t(GET_VAR_FROM_REGION(a, lfac),
					     ARR_INT_TYPE));
        }
        
      }

      // -- Finally, we add the function declaration

      #if 0
      // SANITY CHECK
      std::vector<varname_t> sorted_ins, sorted_outs;
      sorted_ins.reserve(inputs.size());
      sorted_outs.reserve(outputs.size());
      for (auto kv: inputs) sorted_ins.push_back(kv.first);
      for (auto kv: outputs) sorted_outs.push_back(kv.first);
      std::sort(sorted_ins.begin(), sorted_ins.end());
      std::sort(sorted_outs.begin(), sorted_outs.end());
      std::vector<varname_t> intersect;
      std::set_intersection(sorted_ins.begin(), sorted_ins.end(),
			    sorted_outs.begin(), sorted_outs.end(),
			    std::back_inserter(intersect));
      if (!intersect.empty()) {
	llvm::errs () << "Function inputs and outputs should not intersect\n";
	crab::outs () << "INPUTS: {";
	for (auto kv: inputs) { crab::outs () << kv.first << ":" << kv.second << ";"; }
	crab::outs () << "}\n";
	crab::outs () << "OUTPUTS: {";
	for (auto kv: outputs) { crab::outs () << kv.first << ":" << kv.second  << ";"; }
	crab::outs () << "}\n";
      }
      #endif

      
      function_decl<varname_t> decl(GET_VAR(m_func, lfac), inputs, outputs);
      m_cfg->set_func_decl (decl);
      
    }

    #if 1
    // -- Connect all sink blocks with an unreachable instruction to
    //    the exit block.  For a forward analysis this doesn't have
    //    any impact since unreachable becomes bottom anyway.
    //    However, a backward analysis starting with an invariant that
    //    says the exit is unreachable may incorrectly infer that the
    //    preconditions of the error states is false just because it
    //    never propagates backwards from these special sink blocks.
    if (m_cfg->has_exit()) {
      basic_block_t &exit = m_cfg->get_node (m_cfg->exit());
      for (auto &B: m_func) {
	if (opt_basic_block_t b = lookup(B)) {
	  if ((*b).label() == m_cfg->exit ())
	    continue;
	  
	  auto it_pair = (*b).next_blocks();
	  if (it_pair.first == it_pair.second) {
	    // block has no successors and it is not the exit block
	    for (auto &I: B) 
	      if (isa<UnreachableInst>(I))
		(*b) >> exit;
	  }
	}
      }
    }
    #endif
    
    if (CrabCFGSimplify) m_cfg->simplify ();
    if (CrabPrintCFG) crab::outs() << *m_cfg << "\n";
    return ;
  }

} // end namespace crab_llvm
