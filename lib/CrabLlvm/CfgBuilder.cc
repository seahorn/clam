/* 
 * Translate a LLVM function to an *abstracted* CFG language
 * understood by Crab.
 * 
 * The main purpose of the translation is to replace PHI nodes with
 * assignments and branches into assumes. This part is quite standard.
 * 
 * What it's not so standard is that the translation also performs
 * code abstractions by replacing concrete instructions with
 * abstracted ones:
 * 
 * (1) If the abstraction level includes only integers the translation
 *     will cover only instructions with operands of integer type. 
 *
 * (2) If the abstraction level includes pointers then in addition to
 *     integer scalars it will translate instructions that compute
 *     pointer numerical offsets. All pointers are abstracted to their
 *     numerical offsets *ignoring* their actual addresses. By offset
 *     we mean the distance between the base of the memory object to
 *     which the pointer points to and the pointer's address.
 *
 * (3) If the abstraction level includes memory contents, in addition
 *     to the previous abstractions, load and stores as well as other
 *     memory builtins (malloc-like functions) and llvm intrinsics
 *     (memset, memcpy, etc) will be translated using a memory
 *     abstraction based on DSA. This abstraction consists of
 *     partitioning the heap into a finite set of disjoint heaplets so
 *     that an array abstract domain can be used to reason about
 *     heaplets by mapping each heaplet to an array.
 *
 * ## Assumptions and limitations ##
 * 
 * The translation covers only integer scalars, limited reasoning
 * about pointer offsets (e.g., for buffer overflow checks) and memory
 * contents of integer type so that a numerical array abstract domain
 * can be used.
 * 
 * Although DSA provides us with separation information (i.e., whether
 * two pointers may alias or not) the abstraction level (2) is very
 * imprecise (though sound) in cases where a pointer address is read
 * from memory. In fact, the translation does *not* currently generate
 * Crab pointer statements such as ptr_store, ptr_load, ptr_assign,
 * new_object, and new_ptr_func (see TODO's). This would allow a
 * pointer analysis to keep track of offsets even when a pointer
 * address is read from memory, and of course to infer more precise
 * aliasing information than DSA.
 * 
 */

#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/IRBuilder.h"
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
#include "crab_llvm/Support/CFG.hh"

// FIXME: this translation may generate a lot dead code, in
// particular, when an instruction feeds load or stores that are not
// tracked. 

using namespace llvm;

llvm::cl::opt<bool>
CrabCFGSimplify ("crab-cfg-simplify",
                 cl::desc ("Simplify Crab CFG"), 
                 cl::init (false),
                 cl::Hidden);

llvm::cl::opt<bool>
CrabPrintCFG ("crab-print-cfg",
              cl::desc ("Print Crab CFG"), 
              cl::init (false));

llvm::cl::opt<bool>
CrabEnableUniqueScalars ("crab-singleton-aliases",
                         cl::desc ("Treat singleton alias sets as scalar values"), 
                         cl::init (false));

/* 
 Allow to track memory but ignoring pointer arithmetic. This makes
 sense because some crab analyses can reason about memory without
 having any knowledge about pointer arithmetic assuming these two
 conditions are satisfied by the translation:
    1) a cell can only be pointed by objects of same (or compatible)
       types, and
    2) memory accesses are aligned.

 DSAMemAnalysis guarantees these conditions.
*/
llvm::cl::opt<bool>
CrabNoPtrArith ("crab-disable-offsets",
                cl::desc ("Disable translation of pointer offsets"), 
                cl::init (false),
                cl::Hidden);

// If a pointer is, for instance, a global variable or an alloca then
// its numerical offset starts from zero. As result, invariants like
// b <= offset <= b + 40 can be simplified to 0 <= offset <= 40.
llvm::cl::opt<bool>
CrabStartPtrOffsetFromZero ("crab-start-offset-from-zero",
                cl::desc ("Pointer offsets start from 0 rather than from its object's base address"), 
                cl::init (true),
                cl::Hidden);

/* 
  Since LLVM IR is in SSA form many of the havoc statements are
  redundant since variables can be defined only once.
 */
llvm::cl::opt<bool>
CrabIncludeHavoc ("crab-include-useless-havoc",
                  cl::desc ("Include all havoc statements."), 
                  cl::init (true),
                  cl::Hidden);

/*
  Enable initialization of arrays that correspond to objects that may
  contain an infinite number of memory locations. 

  Initialization of allocation sites originated from calloc or memset
  instructions may be unsound if it can be executed by more than one
  execution.
*/
llvm::cl::opt<bool>
CrabUnsoundArrayInit ("crab-arr-unsound-init",
                      cl::desc ("Unsound initialization of arrays."), 
                      cl::init (false),
                      cl::Hidden);

namespace crab_llvm
{

  using namespace crab::cfg_impl;
  using namespace boost;

  inline variable_type getType (Type * ty) {
    if (ty->isIntegerTy ())
      return INT_TYPE;
    else if (ty->isPointerTy ())
      return PTR_TYPE;
    else
      return UNK_TYPE;
  }

  // helper to handle option CrabEnableUniqueScalars
  const Value* isGlobalSingleton (region_t r) {
    if (CrabEnableUniqueScalars) {
      if (const Value* v = r.getSingleton ()) {
        return v;
      }
    }
    return nullptr;
  }


  //! Translate flag conditions 
  //  Preconditions: this visitor is not intended to be executed for a
  //                 block. Instead, it should be called to visit a
  //                 single statement (BinaryOperator or CmpInst).
  struct NumAbsCondVisitor : 
      public InstVisitor<NumAbsCondVisitor>
  {
    sym_eval_t& m_sev;
    basic_block_t& m_bb;
    bool m_is_negated;
    // if true constraints are added as assumptions, otherwise as
    // assertions.
    bool m_is_assumption; 

    NumAbsCondVisitor (sym_eval_t& sev, basic_block_t& bb, bool is_negated,
                       bool is_assumption = true)
        : m_sev (sev),
          m_bb (bb), 
          m_is_negated (is_negated),
          m_is_assumption (is_assumption) { }

    void add_cst (z_lin_cst_t cst) {
      if (m_is_assumption)
        m_bb.assume(cst);
      else
        m_bb.assertion(cst);
    }

    void normalizeCmpInst(CmpInst &I) {
      switch (I.getPredicate()){
        case ICmpInst::ICMP_UGT:	
        case ICmpInst::ICMP_SGT: I.swapOperands(); break; 
        case ICmpInst::ICMP_UGE:	
        case ICmpInst::ICMP_SGE: I.swapOperands(); break; 
        default: ;
      }
    }

    z_lin_cst_sys_t gen_cst_sys (CmpInst &I, bool Unsigned = true) {

      normalizeCmpInst(I);
      
      const Value& v0 = *I.getOperand (0);
      const Value& v1 = *I.getOperand (1);

      optional<z_lin_exp_t> op1 = m_sev.lookup (v0);
      optional<z_lin_exp_t> op2 = m_sev.lookup (v1);
      
      z_lin_cst_sys_t res;
      
      if (op1 && op2) { 
        switch (I.getPredicate ()) {
          case CmpInst::ICMP_EQ:
            if (!m_is_negated)
              res += z_lin_cst_t (*op1 == *op2);
            else
              res += z_lin_cst_t (*op1 != *op2);
            break;
          case CmpInst::ICMP_NE:
            if (!m_is_negated)
              res += z_lin_cst_t (*op1 != *op2);
            else
              res += z_lin_cst_t (*op1 == *op2);
            break;
          case CmpInst::ICMP_ULT:
            if (Unsigned) {
              if (m_sev.isVar (*op1))
                res += z_lin_cst_t (*op1 >= z_number (0));
              if (m_sev.isVar (*op2))
                res += z_lin_cst_t (*op2 >= z_number (0));
            }
          case CmpInst::ICMP_SLT:
            if (!m_is_negated)
              res += z_lin_cst_t (*op1 <= *op2 - 1);
            else
              res += z_lin_cst_t (*op1 >= *op2);
            break; 
          case CmpInst::ICMP_ULE:
            if (Unsigned) {
              if (m_sev.isVar (*op1))
                res += z_lin_cst_t (*op1 >= z_number (0));
              if (m_sev.isVar (*op2))
                res += z_lin_cst_t (*op2 >= z_number (0));
            }
          case CmpInst::ICMP_SLE:
            if (!m_is_negated)
              res += z_lin_cst_t (*op1 <= *op2);
            else
              res += z_lin_cst_t (*op1 >= *op2 + 1);
            break;
          default:  
            llvm_unreachable ("unreachable");
        }
      }
      return res;
    }

    void translateBitwiseBranchTrueCond (BinaryOperator&I) {

      assert (!m_is_negated);

      if (!m_sev.isTracked (I)) return;

      CmpInst* C1 = nullptr;
      CmpInst* C2 = nullptr;

      if (I.getOperand (0)->getType ()->isIntegerTy () &&
          I.getOperand (1)->getType ()->isIntegerTy ()) {
        C1 = dyn_cast<CmpInst> (I.getOperand (0));
        if (C1)
          for (auto cst: gen_cst_sys (*C1)) 
            add_cst(cst);
        else if (BinaryOperator* I0 = dyn_cast<BinaryOperator> (I.getOperand (0)))
          translateBitwiseBranchTrueCond (*I0);
        
        C2 = dyn_cast<CmpInst> (I.getOperand (1));
        if (C2)
          for (auto cst: gen_cst_sys (*C2)) 
            add_cst(cst);
        else if (BinaryOperator* I1 = dyn_cast<BinaryOperator> (I.getOperand (1))) 
          translateBitwiseBranchTrueCond (*I1);
      }
    }

    void visitBinaryOperator(BinaryOperator &I) {
      #if 0
      // It searches only for this particular pattern:
      //   %o1 = icmp ...
      //   %o2 = icmp ...
      //   %f = and %o1 %o2 
      //   br %f bb1 bb2
      // and it adds *only* in bb1 the constraints from %o1 and %2. 
      // 
      // ** The bitwise operation will be anyway translated. The
      //    purpose here is to add extra constraints to the abstract
      //    domain which otherwise would be very hard (or sometimes
      //    impossible) to infer by itself.
      //
      // ** We do not add any constraint in bb2 because we would need
      //    to add a disjunction and the CFG language does not allow
      //    that.
      if (!m_is_negated)
        translateBitwiseBranchTrueCond (I);      
     #endif 
    }

    void visitCmpInst (CmpInst &I) {
      if (CrabNoPtrArith && 
          (!I.getOperand (0)->getType ()->isIntegerTy () ||
           !I.getOperand (1)->getType ()->isIntegerTy ())) 
        return;    

      for (auto cst: gen_cst_sys (I))
        add_cst(cst);

      // If this is reached then I is at least used by some branch so
      // we check if there is another use. If not, we don't bother to
      // add these extra constraints.
      const Value&v = I;
      if (v.hasNUsesOrMore (2)) {
        varname_t lhs = m_sev.symVar (v);
        if (m_is_negated)
          add_cst(z_lin_exp_t (lhs) == z_lin_exp_t (0));
        else
          add_cst(z_lin_exp_t (lhs) == z_lin_exp_t (1));
      }
    }
  };

  //! Translate PHI nodes
  struct NumAbsPhiVisitor : 
      public InstVisitor<NumAbsPhiVisitor> {

    sym_eval_t& m_sev;
    // block where assignment/havoc will be inserted
    basic_block_t& m_bb; 
    // incoming block of the PHI instruction
    const llvm::BasicBlock& m_inc_BB; 

    NumAbsPhiVisitor (sym_eval_t& sev, 
                      basic_block_t& bb, 
                      const llvm::BasicBlock& inc_BB): 
        m_sev (sev), m_bb (bb), m_inc_BB (inc_BB)
    { }

    void visitBasicBlock (llvm::BasicBlock &BB) {

      auto curr = BB.begin ();
      if (!isa<PHINode> (curr)) return;

      DenseMap<const Value*, z_lin_exp_t> oldValMap;

      // --- All the phi-nodes must be evaluated atomically. This
      //     means that if one phi node v1 has as incoming value
      //     another phi node v2 in the same block then it should take
      //     the v2's old value (i.e., before v2's evaluation).

      for (; PHINode *phi = dyn_cast<PHINode> (curr); ++curr) {        

        const Value &v = *phi->getIncomingValueForBlock (&m_inc_BB);

        if (!m_sev.isTracked (v)) continue;

        if (CrabNoPtrArith && !phi->getType ()->isIntegerTy ()) continue;

        const PHINode* vv = dyn_cast<PHINode> (&v);
        if (vv && (vv->getParent () == &BB)) {
          if (auto x = m_sev.lookup (v)) {
            // --- save the old version of the variable that maps to
            //     the phi node v
            auto it = oldValMap.find (&v);
            if (it == oldValMap.end ()) {
              varname_t oldVal = m_sev.getVarFac().get ();
              m_bb.assign (oldVal, *x);
              oldValMap.insert (make_pair (&v, z_lin_exp_t (oldVal)));
            }
          }
        }
      }

      curr = BB.begin ();
      for (unsigned i = 0; isa<PHINode> (curr); ++curr) {
        PHINode &phi = *cast<PHINode> (curr);

        if (!m_sev.isTracked (phi)) continue;

        if (CrabNoPtrArith && !phi.getType ()->isIntegerTy ()) continue;

        varname_t lhs = m_sev.symVar(phi);
        const Value &v = *phi.getIncomingValueForBlock (&m_inc_BB);
        
        auto it = oldValMap.find (&v);
        if (it != oldValMap.end ()) {
          m_bb.assign (lhs, it->second);
        }
        else {
          if (auto op = m_sev.lookup (v))
            m_bb.assign(lhs, *op);
          else 
            m_bb.havoc(lhs);
        }
      }
    }    
  };

  //! Translate the rest of instructions
  struct NumAbsVisitor : 
      public InstVisitor<NumAbsVisitor> {

    sym_eval_t& m_sev;
    const DataLayout* m_dl;
    const TargetLibraryInfo* m_tli;
    basic_block_t& m_bb;
    bool m_is_inter_proc;

    NumAbsVisitor (sym_eval_t& sev, 
                   const DataLayout* dl, const TargetLibraryInfo* tli,
                   basic_block_t & bb, bool isInterProc): 
        m_sev (sev),
        m_dl (dl), 
        m_tli (tli),
        m_bb (bb), 
        m_is_inter_proc (isInterProc)  
    { }

   private:

    unsigned fieldOffset (const StructType *t, unsigned field) {
      return m_dl->getStructLayout (const_cast<StructType*>(t))->
          getElementOffset (field);
    }

    unsigned storageSize (const Type *t) {
      return m_dl->getTypeStoreSize (const_cast<Type*> (t));
    }

    bool startPtrOffsetFromZero (const Value *V) const {
      if (CrabStartPtrOffsetFromZero) {
        V = V->stripPointerCasts();
        if (isa<const AllocaInst> (V) || isa<const GlobalVariable> (V))
          return true;
        // pointer originated by malloc-like functions?
      }
      return false;
    }

    // Return true if all uses of V are non-trackable memory accesses.
    // Useful to avoid translating bitcode that won't have any effect
    // anyway.
    bool AllUsesAreNonTrackMem (Value* V) const {
      V = V->stripPointerCasts();
      for (auto &U: V->uses ()) {
        if (StoreInst *SI = dyn_cast<StoreInst> (U.getUser())) {
          Type* ty = SI->getOperand (0)->getType ();
          if (ty->isIntegerTy ()) {
            if (Instruction *I = dyn_cast<Instruction> (V))  {
              Function*F = I->getParent ()->getParent ();      
              if (m_sev.getMem().getRegion (*F, V).isUnknown ())
                continue;
            }
            return false;
          }
        }
        else if (LoadInst *LI = dyn_cast<LoadInst> (U.getUser())) {
          Type *ty = LI->getType();
          if (ty->isIntegerTy ()) {
            if (Instruction *I = dyn_cast<Instruction> (V))  {
              Function*F = I->getParent ()->getParent ();      
              if (m_sev.getMem().getRegion (*F, V).isUnknown ())
                continue;
            }
            return false;
          }
        }
        else if (CallInst *CI = dyn_cast<CallInst> (U.getUser())) { 
          CallSite CS (CI);
          Function* callee = CS.getCalledFunction ();
          if (callee && ( callee->getName().startswith ("llvm.dbg") || 
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
    
    static bool isLogicalOp(const Instruction &I) {
      return (I.getOpcode() >= Instruction::And && 
              I.getOpcode() <= Instruction::Xor);
    }

   public:
        
    /// skip PHI nodes
    void visitPHINode (PHINode &I) {}

    /// skip BranchInst
    void visitBranchInst (BranchInst &I) {}
    
    /// CmpInst's are translated only if they feed the terminator of a
    /// block (execBr) or a select.    
    void visitCmpInst (CmpInst &I) {
     #if 0
      /// --- We cover the cases where it feeds a binary operator
      /// (e.g., bitwise operator).

      /// XXX: this is overkill if there are many cases because Crab
      /// select statements perform joins. A solution would be to
      /// perform multiple select's together so the number of join
      /// operations can be reduced. This would be require to change
      /// Crab CFG language.

      if (!m_sev.isTracked (I)) return;

      Value*V = &I;

      bool isBinOp = false;
      for (auto &U: V->uses ())
        isBinOp |= isa<BinaryOperator> (U.getUser ());

      if (isBinOp) {
        // Perform the following translation:
        //    %x = geq %y, 10  ---> select (%x, (geq %y, #10), 1, 0)

        NumAbsCondVisitor v (m_sev, m_bb, false);
        auto csts = v.gen_cst_sys (I, false /*force generating one constraint*/);
        if (std::distance (csts.begin (), csts.end ()) == 1)
          m_bb.select (m_sev.symVar(I), *(csts.begin ()), 1, 0);
        else if (LlvmIncludeHavoc) 
          m_bb.havoc(m_sev.symVar(I));
      }
     #endif       
    }
      
    void visitBinaryOperator(BinaryOperator &I) {
      if (!m_sev.isTracked (I)) return;
      
      varname_t lhs = m_sev.symVar(I);
      switch (I.getOpcode ())
      {
        case BinaryOperator::Add:
        case BinaryOperator::Sub:
        case BinaryOperator::Mul:
        case BinaryOperator::SDiv:
        case BinaryOperator::UDiv:
        case BinaryOperator::SRem:
        case BinaryOperator::URem:
        case BinaryOperator::Shl:
        case BinaryOperator::AShr:
          doArithmetic (lhs, I);
          break;
        case BinaryOperator::And:
        case BinaryOperator::Or:
        case BinaryOperator::Xor:
          doBitwise (lhs, I);
          break;
        case BinaryOperator::LShr:
        default:
          if (CrabIncludeHavoc) m_bb.havoc(lhs);
          break;
      }
    }
    
    void doArithmetic (varname_t lhs, BinaryOperator &i) {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);
      
      optional<z_lin_exp_t> op1 = m_sev.lookup (v1);
      optional<z_lin_exp_t> op2 = m_sev.lookup (v2);
      
      if (!(op1 && op2)) return;
      
      switch(i.getOpcode())
      {
        case BinaryOperator::Add:
          m_bb.add (lhs, *op1, *op2);
          break;
        case BinaryOperator::Sub:
          if ((*op1).is_constant())            
          { // Crab cfg does not support subtraction of a constant by a
            // variable because the crab api for abstract domains
            // does not support it.
            m_bb.assign (lhs, z_lin_exp_t ((*op1).constant ()));
            m_bb.sub (lhs, z_lin_exp_t (lhs), *op2);
          }
          else
            m_bb.sub (lhs, *op1, *op2);
          break;
        case BinaryOperator::Mul:
          m_bb.mul (lhs, *op1, *op2);
          break;
        case BinaryOperator::SDiv:
          if ((*op1).is_constant())            
          { // Crab cfg does not support division of a constant by a
            // variable because the crab api for abstract domains
            // does not support it.
            m_bb.assign (lhs, z_lin_exp_t ((*op1).constant ()));
            m_bb.div (lhs, z_lin_exp_t (lhs), *op2);
          }
          else
            m_bb.div (lhs, *op1, *op2);
          break;
        case BinaryOperator::UDiv:
          if ((*op1).is_constant() && (*op2).is_constant ()) {
            // Crab cfg api does not support unsigned arithmetic operations
            // with both constant operands. Llvm frontend should get
            // rid of them.
            errs () << "Warning: ignored udiv with both constant operands\n";
            if (CrabIncludeHavoc)
              m_bb.havoc(lhs);
          }
          else if ((*op1).is_constant()) {           
            // Crab cfg does not support division of a constant by a
            // variable because the crab api for abstract domains
            // does not support it.
            m_bb.assign (lhs, z_lin_exp_t ((*op1).constant ()));
            m_bb.udiv (lhs, z_lin_exp_t (lhs), *op2);
          }
          else
            m_bb.udiv (lhs, *op1, *op2);
          break;
        case BinaryOperator::SRem:
          if ((*op1).is_constant()) {           
            // Crab cfg does not support rem of a constant by a
            // variable because the crab api for abstract domains
            // does not support it.
            m_bb.assign (lhs, z_lin_exp_t ((*op1).constant ()));
            m_bb.rem (lhs, z_lin_exp_t (lhs), *op2);
          }
          else
            m_bb.rem (lhs, *op1, *op2);
          break;
        case BinaryOperator::URem:
          if ((*op1).is_constant() && (*op2).is_constant ()) {
            // Crab cfg does not support unsigned arithmetic
            // operations with both constant operands. Llvm frontend
            // should get rid of them.
            errs () << "Warning: ignored urem with constant operands\n";
            if (CrabIncludeHavoc)
              m_bb.havoc(lhs);
          }
          else if ((*op1).is_constant()) {           
            // Crab cfg does not support rem of a constant by a
            // variable because the crab api for abstract domains does
            // not support it.
            m_bb.assign (lhs, z_lin_exp_t ((*op1).constant ()));
            m_bb.urem (lhs, z_lin_exp_t (lhs), *op2);
          }
          else
            m_bb.urem (lhs, *op1, *op2);
          break;
        case BinaryOperator::Shl:
          if ((*op2).is_constant()) {
            ikos::z_number k = (*op2).constant ();
            int shift = (int) k;
            assert (shift >= 0);
            unsigned factor = 1;
            for (unsigned i = 0; i < (unsigned) shift; i++) 
              factor *= 2;
            m_bb.mul (lhs, *op1, z_lin_exp_t (factor));            
          }
          else if (CrabIncludeHavoc)
            m_bb.havoc(lhs);
          break;
        case BinaryOperator::AShr:
          if ((*op2).is_constant()) {
            ikos::z_number k = (*op2).constant ();
            int shift = (int) k;
            assert (shift >= 0);
            unsigned factor = 1;
            for (unsigned i = 0; i < (unsigned) shift; i++) 
              factor *= 2;
            m_bb.div (lhs, *op1, z_lin_exp_t (factor));            
          }
          else if (CrabIncludeHavoc)
            m_bb.havoc(lhs);
          break;
        default:
          if (CrabIncludeHavoc)
            m_bb.havoc(lhs);
          break;
      }
    }

    void doBitwise (varname_t lhs, BinaryOperator &i) {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);
      
      optional<z_lin_exp_t> op1 = m_sev.lookup (v1);
      optional<z_lin_exp_t> op2 = m_sev.lookup (v2);
      
      if (!(op1 && op2)) return;

      switch(i.getOpcode()) {
        case BinaryOperator::And:
          m_bb.bitwise_and (lhs, *op1, *op2);
          break;
        case BinaryOperator::Or:
          m_bb.bitwise_or (lhs, *op1, *op2);
          break;
        case BinaryOperator::Xor:
          m_bb.bitwise_xor (lhs, *op1, *op2);
          break;
        default:
          if (CrabIncludeHavoc)
            m_bb.havoc(lhs);
          break;
      }
    }
        
    void visitZExtInst (ZExtInst &I) {
      // This optimization tries to reduce the number variables within
      // a basic block. This will put less pressure on the numerical
      // abstract domain later on. Search for this idiom: 
      /*
         %_14 = zext i8 %_13 to i32
         %_15 = getelementptr inbounds [10 x i8]* @_ptr, i32 0, i32 %_14
         
         If all users of the zext instructions are gep's then we can
         skip the translation of the zext instruction.
      */
      Value* dest = &I; 
      bool all_gep = true;
      for (Use &U: boost::make_iterator_range(dest->use_begin(),
                                              dest->use_end()))
        all_gep &= isa<GetElementPtrInst> (U.getUser ());

      if (!all_gep) doCast (I); 
    }

    void visitSExtInst (SExtInst &I) {
      // try same optimization than with zext
      Value* dest = &I; 
      bool all_gep = true;
      for (Use &U: boost::make_iterator_range(dest->use_begin(),
                                              dest->use_end()))
        all_gep &= isa<GetElementPtrInst> (U.getUser ());
      if (!all_gep) doCast (I); 
    }

    void visitSelectInst(SelectInst &I) {
      if (!m_sev.isTracked (I)) return;

      if (CrabNoPtrArith && 
          (!I.getTrueValue ()->getType ()->isIntegerTy () ||
           !I.getFalseValue ()->getType ()->isIntegerTy ())) 
        return;    
      
      Value& cond = *I.getCondition ();
      optional<z_lin_exp_t> op0 = m_sev.lookup (*I.getTrueValue ());
      optional<z_lin_exp_t> op1 = m_sev.lookup (*I.getFalseValue ());
      
      if (!(op0 && op1)) return;

      varname_t lhs = m_sev.symVar(I);      
      if (ConstantInt *ci = dyn_cast<ConstantInt> (&cond)) {
        if (ci->isOne ()) {
          m_bb.assign (lhs, *op0);
          return;
        }
        else if (ci->isZero ()) {
          m_bb.assign (lhs, *op1);
          return;
        }
      }

      if (CmpInst* CI = dyn_cast<CmpInst> (&cond)) {
        NumAbsCondVisitor v (m_sev, m_bb, false);
        auto csts = v.gen_cst_sys (*CI, false /*force generating one constraint*/);
        if (std::distance (csts.begin (), csts.end ()) == 1) {
          // select only takes a single constraint otherwise its
          // reasoning gets complicated if the analysis needs to
          // negate conjunction of constraints. 
          m_bb.select (lhs, *(csts.begin ()), *op0, *op1);
          return;
        }
      }

      // default case if everything fails ...

      // Warning: we don't precisely reason about select instructions
      // so we lose precision here. One simple solution is to use the
      // option --lower-select to remove all select instructions.
      if (CrabIncludeHavoc)
        m_bb.havoc (m_sev.symVar (cond));
      
      m_bb.select(lhs, m_sev.symVar (cond),  *op0, *op1);
    }

    void doCast(CastInst &I) {
      /// TODO: generate a ptr_assign instruction if the operands are
      ///       pointers.

      if (!m_sev.isTracked (I)) 
        return;

      if (CrabNoPtrArith && !I.getType ()->isIntegerTy ())
        return;

      if (AllUsesAreNonTrackMem (&I))
        return;

      varname_t dst = m_sev.symVar (I);
      const Value& v = *I.getOperand (0); // the value to be casted

      optional<z_lin_exp_t> src = m_sev.lookup (v);

      if (src) {
        // //if (startPtrOffsetFromZero (&v)) {
        //   m_bb.assign(dst, 0);
        // } else {
        m_bb.assign(dst, *src);
        //}
      }
      else {
        if (v.getType ()->isIntegerTy (1)) {
          m_bb.assume ( z_lin_exp_t (dst) >= z_lin_exp_t (0) );
          m_bb.assume ( z_lin_exp_t (dst) <= z_lin_exp_t (1) );
        }
        else if (CrabIncludeHavoc)
          m_bb.havoc(dst);
      }
    }

    // This will cover the whole class of cast instructions
    void visitCastInst (CastInst &I) {
      doCast (I);
    }

    void visitGetElementPtrInst (GetElementPtrInst &I) {
      /// TODO: generate a ptr_assign(b, offset) instruction.

      if (!m_sev.isTracked (I)) {
        return;
      }

      CRAB_LOG ("cfg-gep",
                errs () << "Translating " << I << "\n");

      if (CrabNoPtrArith || AllUsesAreNonTrackMem (&I)) {
        if (CrabIncludeHavoc) m_bb.havoc (m_sev.symVar (I)); 
        CRAB_LOG("cfg-gep",
                 errs () << " -- skipped translation: all uses are not track mem\n");
        return;
      }

      optional<z_lin_exp_t> ptr = m_sev.lookup (*I.getPointerOperand ());
      if (!ptr)  {
        if (CrabIncludeHavoc) m_bb.havoc (m_sev.symVar (I));
        return;
      }

      varname_t res = m_sev.symVar (I);

      // -- more efficient translation if the GEP offset is constant
      unsigned BitWidth = m_dl->getPointerTypeSizeInBits(I.getType());
      APInt Offset(BitWidth, 0);
      if (I.accumulateConstantOffset (*m_dl, Offset)) {
        z_lin_exp_t offset (m_sev.toMpz (Offset).get_str ());
        if (startPtrOffsetFromZero(I.getPointerOperand ())) {
          m_bb.assign (res, offset);
          CRAB_LOG("cfg-gep",
                   crab::outs() << "-- " << res << ":=" << offset << "\n");        
        }
        else {
          m_bb.add (res, *ptr, offset);
          CRAB_LOG("cfg-gep",
                   crab::outs() << "-- " << res << ":=" << *ptr  << "+" << offset << "\n");        
        }
        return;
      }

      SmallVector<const Value*, 4> ps;
      SmallVector<const Type*, 4> ts;
      gep_type_iterator typeIt = gep_type_begin (I);
      for (unsigned i = 1; i < I.getNumOperands (); ++i, ++typeIt) {
        // strip zext/sext if there is one
        if (const ZExtInst *ze = dyn_cast<const ZExtInst> (I.getOperand (i)))
          ps.push_back (ze->getOperand (0));
        else if (const SExtInst *se = dyn_cast<const SExtInst> (I.getOperand (i)))
          ps.push_back (se->getOperand (0));
        else 
          ps.push_back (I.getOperand (i));
        ts.push_back (*typeIt);
      }

      bool is_init = false;
      for (unsigned i = 0; i < ps.size (); ++i) {
        if (const StructType *st = dyn_cast<const StructType> (ts [i]))
        { // --- we do not optimize this case because it will often
          //     optimized by accumulateConstantOffset
          if (const ConstantInt *ci = dyn_cast<const ConstantInt> (ps [i])) {
            ikos::z_number offset (fieldOffset (st, ci->getZExtValue ()));
            if (i == 0) {
              if (startPtrOffsetFromZero(I.getPointerOperand ())) {
                m_bb.assign (res, offset);
                CRAB_LOG("cfg-gep",
                         crab::outs() << "-- " << res << ":=" << offset << "\n");
              }
              else {
                m_bb.add (res, *ptr, offset);
                CRAB_LOG("cfg-gep",
                         crab::outs() << "-- " << res << ":=" << *ptr << "+" << offset << "\n");
              }
            }
            else {
              m_bb.add (res, res, offset);
              CRAB_LOG("cfg-gep",
                       crab::outs() << "-- " << res << ":=" << res << "+" << offset << "\n");
            }
          }
          else 
            llvm_unreachable ("unreachable");
        }
        else if (const SequentialType *seqt = dyn_cast<const SequentialType> (ts [i]))
        {
          if (const ConstantInt *ci = dyn_cast<const ConstantInt> (ps [i])) {
            if (ci->isZero ())
              continue;
          }

          optional<z_lin_exp_t> p = m_sev.lookup (*ps[i]);
          assert (p);
          if (!is_init) {
            if (startPtrOffsetFromZero(I.getPointerOperand ())) {
              ikos::z_number offset (storageSize (seqt->getElementType ()));
              m_bb.mul (res, *p, offset);
              is_init = true;
              CRAB_LOG("cfg-gep",
                       crab::outs() << "-- " << res << ":=" << *p << "*" << offset << "\n");
            } else {
              ikos::z_number offset (storageSize (seqt->getElementType ()));
              varname_t tmp = m_sev.getVarFac().get ();
              m_bb.mul (tmp, *p, offset);
              m_bb.add (res, *ptr, z_lin_exp_t (tmp));
              CRAB_LOG("cfg-gep",
                       crab::outs() << "-- " << tmp << ":=" << *p << "*" << offset << "\n"
                       << "-- " << res << ":=" << *ptr << "+" << tmp << "\n");
            }
          } else {
            ikos::z_number offset (storageSize (seqt->getElementType ()));
            varname_t tmp = m_sev.getVarFac().get ();
            m_bb.mul (tmp, *p, offset);
            m_bb.add (res, res, tmp);
            CRAB_LOG("cfg-gep",
                     crab::outs() << "-- " << tmp << ":=" << *p << "*" << offset << "\n"
                     << "-- " << res << ":=" << res << "+" << tmp << "\n");
          }
        }
      }
    }

    void visitLoadInst (LoadInst &I) {
      /// --- Translate only loads into integer variables
      
      Type *ty = I.getType();
      if (ty->isIntegerTy () && m_sev.getTrackLevel () == ARR) {
        
        region_t r = m_sev.getMem().getRegion (*(I.getParent ()->getParent ()), 
                                               I.getPointerOperand ());
        
        if (!(r.isUnknown ())) {
          // Post: r corresponds to a memory region pointed by
          //       objects with same type and all accesses are
          //       aligned.
          if (const Value* s = isGlobalSingleton (r)) {
            m_bb.assign (m_sev.symVar (I), z_lin_exp_t (m_sev.symVar(*s)));
            return;
          }
          if (optional<z_lin_exp_t> idx = m_sev.lookup (*I.getPointerOperand ())) {
            m_bb.array_load (m_sev.symVar (I), m_sev.symVar (r), *idx,
                             ikos::z_number (m_dl->getTypeAllocSize (ty)));
            return;
          }
        }
      }

      // TODO: generate a ptr_load instruction here.
      // Even, for offset purposes we will lose dramatically
      // information here!
      if (m_sev.isTracked (I)) {
        if (CrabIncludeHavoc) m_bb.havoc (m_sev.symVar (I));
      }
    }
    
    void visitStoreInst (StoreInst &I) {
      // TODO: generate a ptr_store instruction here.

      /// --- Translate only stores of integer values

      Type* ty = I.getOperand (0)->getType ();
      if (ty->isIntegerTy () && m_sev.getTrackLevel () == ARR) {                
        region_t r = m_sev.getMem().getRegion (*(I.getParent ()->getParent ()),
                                               I.getOperand (1)); 
        if (r.isUnknown ()) return;
        // Post: r corresponds to a memory region pointed by
        //       objects with same type and all accesses are aligned.

        optional<z_lin_exp_t> val = m_sev.lookup (*I.getOperand (0));
        if (!val) return; // FIXME: we should havoc the array

        if (const Value* s = isGlobalSingleton (r)) {
          m_bb.assign (m_sev.symVar (*s), *val);
          return;
        }

        optional<z_lin_exp_t> idx = m_sev.lookup (*I.getPointerOperand ());
        if (!idx) return; // FIXME: we should havoc the array
        m_bb.array_store (m_sev.symVar (r), 
                          *idx, *val, 
                          ikos::z_number (m_dl->getTypeAllocSize (ty)),
                          r.getSingleton () != nullptr); 
      }
    }

    void visitAllocaInst (AllocaInst &I) {
      // TODO: new_object instruction here

      if (m_sev.getTrackLevel () == ARR) {        
        region_t r = m_sev.getMem().getRegion(*(I.getParent()->getParent()), &I);
        if (r.isUnknown ()) return;
        // Post: r corresponds to a memory region pointed by
        //       objects with same type and all accesses are aligned.

        // "Initialization hook": nodes which do not have an explicit
        // initialization are initially undefined. Instead, we assume
        // they are zero initialized so that Crab's array smashing can
        // infer something meaningful. This is correct because in
        // presence of undefined behaviour we can do whatever we want
        // but it will, for instance, preclude the analysis to detect
        // things like uninitialized variables and also if a more
        // expressive array domain is used then this initialization
        // will make it more imprecise.
        m_bb.assume_array (m_sev.symVar(r), 0 /*any value we want*/);
      }
    }

    void doAllocFn (Instruction &I) {
      // TODO: new_object instruction here

      if (CrabUnsoundArrayInit) {
        Value *ptr = &I;
        region_t r = m_sev.getMem().getRegion (*(I.getParent ()->getParent ()), ptr);
        bool isMainCaller = I.getParent()->getParent()->getName().equals("main");
        if (isMainCaller && !r.isUnknown ()) {
          // Post: r corresponds to a memory region pointed by
          //       objects with same type and all accesses are
          //       aligned.
          // We apply here again the "Initialization hook"
          m_bb.assume_array (m_sev.symVar (r), 0);        
        }
      }
      
      // -- havoc return value
      if (m_sev.isTracked (I)) {
        Value *ret = &I;
        if (!ret->getType()->isVoidTy() && CrabIncludeHavoc)
          m_bb.havoc (m_sev.symVar (I));
      }
    }

    void doMemIntrinsic(MemIntrinsic& I) {

      if (isa<MemMoveInst>(I)) return;
      
      if (CrabUnsoundArrayInit) {
        if (MemCpyInst *MCI = dyn_cast<MemCpyInst>(&I)) {
          Value* src = MCI->getSource ();
          Value* dst = MCI->getDest ();
          region_t dst_reg = m_sev.getMem().getRegion(*(I.getParent()->getParent()), dst);
          region_t src_reg = m_sev.getMem().getRegion(*(I.getParent()->getParent ()), src);
          if (dst_reg.isUnknown () || src_reg.isUnknown ()) return;
          m_bb.havoc (m_sev.symVar (dst_reg));
          m_bb.assign (m_sev.symVar (dst_reg), z_lin_exp_t (m_sev.symVar (src_reg)));
        }
        
        if (MemSetInst *MSI = dyn_cast<MemSetInst>(&I)) {
          Value* dst = MSI->getDest();
          Value *val = MSI->getValue();
          region_t r = m_sev.getMem().getRegion(*(I.getParent ()->getParent ()), dst);
          if (r.isUnknown()) return;
          // Post: r corresponds to a memory region pointed by
          //       objects with same type and all accesses are aligned.
          optional<z_lin_exp_t> op0 = m_sev.lookup(*val);          
          if (op0 && (*op0).is_constant()) {
            m_bb.havoc (m_sev.symVar(r));
            m_bb.assume_array (m_sev.symVar(r), (*op0).constant ());
          }
        }
      }
    }

    void visitReturnInst (ReturnInst &I) {
      if (!m_is_inter_proc) return;

      if (I.getNumOperands () > 0) {
        if (!m_sev.isTracked (*(I.getOperand (0))))
          return;

        if (CrabNoPtrArith && 
            !I.getOperand (0)->getType ()->isIntegerTy ())
          return;

          m_bb.ret ( m_sev.symVar (*(I.getOperand (0))), 
                     getType ((*I.getOperand (0)).getType ()));
      }
    }

   private:

    std::pair<varname_t, variable_type> normalizeScalarParam (Value& V) {
      if (Constant *cst = dyn_cast< Constant> (&V)) {
        varname_t v = m_sev.getVarFac().get ();
        if (ConstantInt * intCst = dyn_cast<ConstantInt> (cst)) {
          auto e = m_sev.lookup (*intCst);
          if (e) {
            m_bb.assign (v, *e);
            return make_pair (v, INT_TYPE);
          }
        }
        m_bb.havoc (v);
        return make_pair (v, UNK_TYPE);
      }
      else
        return make_pair (m_sev.symVar (V), getType (V.getType ()));
    }

   public:

    static bool isAssertFn(Function* F) {
      return (F->getName().equals("verifier.assert") || 
              F->getName().equals("crab.assert") || 
              F->getName().equals("__CRAB_assert"));
    }

    static bool isAssumeFn(Function* F) {
      return (F->getName().equals("verifier.assume"));
    }

    static bool isNegAssumeFn(Function* F) {
      return (F->getName().equals("verifier.assume.not"));
    }
    
    void visitCallInst (CallInst &I) {
      CallSite CS (&I);
      Function * callee = CS.getCalledFunction ();

      if (!callee) {         
        // --- If we are here is either because the function is Asm or
        //     we could not resolve the indirect call.

        // havoc return value
        if (m_sev.isTracked (I)) {
          Value *ret = &I;
          if (!ret->getType()->isVoidTy() && CrabIncludeHavoc)
            m_bb.havoc (m_sev.symVar (I));
        }
        return;
      }

      // -- ignore any shadow functions created by seahorn
      if (callee->getName().startswith ("shadow.mem")) return;
      if (callee->getName().equals ("seahorn.fn.enter")) return;

      if (isAllocationFn(&I, m_tli)){
        doAllocFn(I);
        return;
      }

      if (callee->isIntrinsic ()) {
        if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(&I)) {
          doMemIntrinsic(*MI);
        } else if (m_sev.isTracked (I)) {
          // -- havoc return value
          Value *ret = &I;
          if (!ret->getType()->isVoidTy() && CrabIncludeHavoc)
            m_bb.havoc (m_sev.symVar (I));
        }
        return;
      }
      
      if (isAssertFn(callee) || isAssumeFn(callee) || isNegAssumeFn(callee)) {
        Value *cond = CS.getArgument (0);        
        // strip zext if there is one
        if (const ZExtInst *ze = dyn_cast<const ZExtInst> (cond))
          cond = ze->getOperand (0);
        if (llvm::Instruction *I = dyn_cast<llvm::Instruction> (cond)) {
          NumAbsCondVisitor v (m_sev, m_bb, isNegAssumeFn(callee), !isAssertFn(callee));
          v.visit (I);
        }
        return;
      }

      if (callee->isDeclaration() || callee->isVarArg() || !m_is_inter_proc) {
        // -- havoc return value
        if (m_sev.isTracked (I)) {
          Value *ret = &I;
          if (!ret->getType()->isVoidTy() && CrabIncludeHavoc)
            m_bb.havoc (m_sev.symVar (I));
        }
        // -- havoc all modified nodes by the callee
        if (m_sev.getTrackLevel () == ARR) {
          region_set_t mods = m_sev.getMem().getModifiedRegions (I);
          for (auto a : mods) {
            m_bb.havoc (m_sev.symVar (a));
          }
        }
        return;
      }

      // --- call to a user-defined function

      vector<pair<varname_t,variable_type> > actuals;
      // -- add the scalar actual parameters
      for (auto &a : boost::make_iterator_range (CS.arg_begin(),
                                                 CS.arg_end())) {
        Value *v = a.get();
        if (!m_sev.isTracked (*v)) 
          continue;
        if (CrabNoPtrArith && !v->getType ()->isIntegerTy ())
          continue;
        
        actuals.push_back (normalizeScalarParam(*v));
      }
      
      if (m_sev.getTrackLevel () == ARR) {
        // -- add the array actual parameters
        region_set_t onlyreads = m_sev.getMem().getOnlyReadRegions (I);
        region_set_t mods = m_sev.getMem().getModifiedRegions (I);
        region_set_t news = m_sev.getMem().getNewRegions (I);
        
        CRAB_LOG("cfg-mem",
                 errs () << "Callsite " << I << "\n";
                 errs () << "\tOnly-Read regions: " << m_sev.getMem().getOnlyReadRegions (I) << "\n";
                 errs () << "\tModified regions: " << m_sev.getMem().getModifiedRegions (I) << "\n";
                 errs () << "\tNew regions:" << m_sev.getMem().getNewRegions (I) << "\n");

        // Make sure that the order of the actuals parameters is the
        // same than the order of the formal parameters when the
        // callee signature is built, search below for (**). This is
        // ensured because array names are globals and get*Regions
        // methods return always the regions in the same order.
        
        // -- add input parameters
        for (auto a: onlyreads) { 
          if (const Value* s = isGlobalSingleton (a))
            actuals.push_back (make_pair (m_sev.symVar (*s), INT_TYPE));
          else
            actuals.push_back (make_pair (m_sev.symVar (a), ARR_TYPE));
        }
        // -- add input/output parameters
        for (auto a: mods) {

          if (news.find(a) != news.end()) continue;

          varname_t a_in = m_sev.getVarFac ().get (); // fresh variable
          // given x: x_in = x; x = *;
          if (const Value* s = isGlobalSingleton (a)) {
            m_bb.assign (a_in, z_lin_exp_t (m_sev.symVar(*s))); 
            m_bb.havoc (m_sev.symVar(*s)); 
          } else {
            m_bb.assign (a_in, z_lin_exp_t (m_sev.symVar (a))); 
            m_bb.havoc (m_sev.symVar (a)); 
          }

          actuals.push_back (make_pair (a_in, ARR_TYPE));
          if (const Value* s = isGlobalSingleton (a))
            actuals.push_back (make_pair (m_sev.symVar (*s), INT_TYPE));
          else
            actuals.push_back (make_pair (m_sev.symVar (a), ARR_TYPE));
        }
        // -- add output parameters
        for (auto a: news) 
          actuals.push_back (make_pair (m_sev.symVar (a), ARR_TYPE));
      }
      
      // -- add the callsite
      if ( (getType (I.getType ()) != UNK_TYPE) && m_sev.isTracked (I) &&
           (!CrabNoPtrArith || I.getType ()->isIntegerTy ())) {
        m_bb.callsite (make_pair (m_sev.symVar (I), getType (I.getType ())), 
                       m_sev.getVarFac() [*callee], actuals);
      }
      else {
        m_bb.callsite (m_sev.getVarFac() [*callee], actuals);        
      }

    }

    void visitUnreachableInst (UnreachableInst &I) {
      m_bb.unreachable();
    }

    /// base case. if all else fails.
    void visitInstruction (Instruction &I) {
      if (!m_sev.isTracked (I)) return;
      if (CrabIncludeHavoc) m_bb.havoc(m_sev.symVar(I));
    }
    
  }; // end class


  CfgBuilder::opt_basic_block_t CfgBuilder::lookup (const llvm::BasicBlock &B) {  
    llvm::BasicBlock* BB = const_cast<llvm::BasicBlock*> (&B);
    llvm_bb_map_t::iterator it = m_bb_map.find (BB);
    if (it == m_bb_map.end ())
      return CfgBuilder::opt_basic_block_t ();
    else
      return CfgBuilder::opt_basic_block_t (it->second);
  }

  void CfgBuilder::add_block (llvm::BasicBlock &B)
  {
    assert (!lookup(B));
    llvm::BasicBlock *BB = &B;
    basic_block_t &bb = m_cfg->insert ( BB);
    m_bb_map.insert (llvm_bb_map_t::value_type (BB, bb));
  }  

  basic_block_t& CfgBuilder::add_block_in_between (basic_block_t &src, 
                                                   basic_block_t &dst,
                                                   const llvm::BasicBlock* BB) {

    assert (m_bb_map.find (BB) == m_bb_map.end()); 

    basic_block_t &bb = m_cfg->insert (BB);

    src -= dst;
    src >> bb;
    bb >> dst;

    return bb;
  }  


  void CfgBuilder::add_edge (llvm::BasicBlock &S, const llvm::BasicBlock &D) {
    opt_basic_block_t SS = lookup(S);
    opt_basic_block_t DD = lookup(D);
    assert (SS && DD);
    *SS >> *DD;
  }  

  // add a fake llvm BasicBlock into the function: this BasicBlock is
  // needed because the translation cannot generate a Crab basic block
  // without being associated with a llvm BasicBlock.  We do not
  // attach it to a parent so we need to free them later.
  const llvm::BasicBlock* CfgBuilder::createFakeBlock (LLVMContext &ctx, 
                                                       const Twine &name,
                                                       Function *parent) {

    llvm::BasicBlock* B = llvm::BasicBlock::Create (ctx, name, nullptr);
    // IRBuilder<> Builder (B);
    // Builder.CreateUnreachable (); // to make the block well formed.
    m_fake_assume_blocks.push_back (B);
    return B;
  }

  //! return the new block inserted between src and dest if any
  CfgBuilder::opt_basic_block_t CfgBuilder::execBr (llvm::BasicBlock &src, 
                                                    const llvm::BasicBlock &dst) {
    // -- the branch condition
    if (const BranchInst *br = dyn_cast<const BranchInst> (src.getTerminator ())) {
      if (br->isConditional ()) {
        opt_basic_block_t Src = lookup(src);
        opt_basic_block_t Dst = lookup(dst);
        assert (Src && Dst);

        const llvm::BasicBlock* BB = createFakeBlock (m_func.getContext (),
                                                      create_bb_name (),
                                                      &m_func);
                                                                      
        basic_block_t &bb = add_block_in_between (*Src, *Dst, BB);
        
        const Value &c = *br->getCondition ();
        if (const ConstantInt *ci = dyn_cast<const ConstantInt> (&c)) {
          if ((ci->isOne ()  && br->getSuccessor (0) != &dst) ||
              (ci->isZero () && br->getSuccessor (1) != &dst))
            bb.unreachable();
        }
        else {
          if (llvm::Instruction *I = 
              dyn_cast<llvm::Instruction> (& const_cast<llvm::Value&>(c))) {
            NumAbsCondVisitor v (m_sev, bb, br->getSuccessor (1) == &dst);
            v.visit (I); 
          }
          else {
            // -- this can happen if the boolean condition is passed
            //    directly (after optimization) as a function
            //    parameter
            varname_t lhs = m_sev.symVar (c);
            if (br->getSuccessor (1) == &dst)
              bb.assume (z_lin_exp_t (lhs) == z_lin_exp_t (0));
            else
              bb.assume (z_lin_exp_t (lhs) == z_lin_exp_t (1));
          }
        }
        return opt_basic_block_t(bb);
      }
      else add_edge (src,dst);
    }
    return opt_basic_block_t();    
  }

  void doInitializer (Constant* cst, basic_block_t& bb, varname_t a) {

    if (isa<ConstantAggregateZero> (cst)){
      // --- a struct or array with all elements initialized to zero
      bb.assume_array (a, 0);
    }
    else if (ConstantInt * ci = dyn_cast<ConstantInt> (cst)) {
      // --- an integer scalar global variable
      vector<ikos::z_number> val;
      val.push_back (ci->getZExtValue ());
      bb.array_init (a, val);
    }
    else if (ConstantDataSequential  *cds = dyn_cast<ConstantDataSequential> (cst)) {
      // --- an array of integers 1/2/4/8 bytes
      if (cds->getElementType ()->isIntegerTy ()) {
        vector<ikos::z_number> vals;
        for (unsigned i=0; i < cds->getNumElements (); i++)
          vals.push_back (cds->getElementAsInteger (i));
        bb.array_init (a, vals);
      }
    }
    else if (GlobalAlias* alias = dyn_cast< GlobalAlias >(cst)) {
      doInitializer (alias->getAliasee (), bb, a);
    }
  }

  void CfgBuilder::build_cfg() {

    crab::ScopedCrabStats __st__("CFG");

    std::vector<llvm::BasicBlock*> blks;
    for (auto &B : m_func) { 
      add_block (B); 
      blks.push_back (&B);
    }

    std::vector<basic_block_t*> retBlks;
    // execBr can add new BasicBlock's in m_func and we do not want to
    // iterate over them.
    for (llvm::BasicBlock* B : blks)
    {
      if (isa<ReturnInst> (B->getTerminator ())) {
        opt_basic_block_t BB = lookup (*B);
        assert (BB);
        if (!BB) continue;

        basic_block_t& bb = *BB;
        retBlks.push_back (&bb);
        NumAbsVisitor v (m_sev, m_dl, m_tli, *BB, m_is_inter_proc);
        v.visit (*B);
      }
      else {
        opt_basic_block_t BB = lookup (*B);
        assert (BB);
        if (!BB) continue;

        // -- build an initial CFG block from bb but ignoring branches
        //    and phi-nodes
        NumAbsVisitor v (m_sev, m_dl, m_tli, *BB, m_is_inter_proc);
        v.visit (*B);
        
        for (const llvm::BasicBlock *dst : succs (*B)) {
          // -- move branch condition in bb to a new block inserted
          //    between bb and dst
          opt_basic_block_t mid_bb = execBr (*B, *dst);

          // -- phi nodes in dst are translated into assignments in
          //    the predecessor
          NumAbsPhiVisitor v (m_sev, (mid_bb ? *mid_bb : *BB), *B);
          v.visit (const_cast<llvm::BasicBlock &>(*dst));
        }
      }
    }
    
    // -- unify multiple return blocks
    if (retBlks.size () == 1) { 
      m_cfg->set_exit (retBlks [0]->label ());
    }
    else if (retBlks.size () > 1) {

      const llvm::BasicBlock* singleRetBlk = createFakeBlock (m_func.getContext (),
                                                              create_bb_name (),
                                                              &m_func);
      
      basic_block_t &unified_ret = m_cfg->insert (singleRetBlk);

      for(unsigned int i=0; i<retBlks.size (); i++)
        *(retBlks [i]) >> unified_ret;
      m_cfg->set_exit (unified_ret.label ());
    }

    /// Allocate arrays with initial values 
    if (m_sev.getTrackLevel () == ARR) {
      if (m_func.getName ().equals ("main")) {
        Module*M = m_func.getParent ();
        basic_block_t & entry = m_cfg->get_node (m_cfg->entry ());
        for (GlobalVariable &gv : boost::make_iterator_range (M->global_begin (),
                                                              M->global_end ())) {
          if (gv.hasInitializer ())  {
            region_t region = m_sev.getMem().getRegion (m_func, &gv);
            if (region.isUnknown ()) continue;
            // Post: region corresponds to a memory region pointed by
            //       objects with same type and all accesses are
            //       aligned.
            entry.set_insert_point_front ();
            
            if (const Value* s = isGlobalSingleton (region)) {
              if (ConstantInt * ci = dyn_cast<ConstantInt> (gv.getInitializer ()))
                entry.assign (m_sev.symVar (*s),ci->getZExtValue ());
            } else {
              doInitializer (gv.getInitializer (), entry, m_sev.symVar (region));
            }
          }
        } 
      }
      
      if (CrabUnsoundArrayInit) {
        // getNewRegions returns all the new nodes created by the
        // function (via malloc-like functions) except if the function
        // is main.
        basic_block_t & entry = m_cfg->get_node (m_cfg->entry ());
        region_set_t news =  m_sev.getMem ().getNewRegions (m_func);
        for (auto n: news) {
          entry.set_insert_point_front ();
          // We apply here the "Initialization hook".
          entry.assume_array (m_sev.symVar (n), 0 /*any value we want*/);
        }
      }
    }

    /// Add function declaration
    if (m_is_inter_proc) {
      assert (!m_func.isVarArg ());

      vector<pair<varname_t,variable_type> > params;
      // -- add scalar formal parameters
      for (llvm::Value &arg : boost::make_iterator_range (m_func.arg_begin (),
                                                          m_func.arg_end ())) {
        if (!m_sev.isTracked (arg))
          continue;
        if (CrabNoPtrArith && !arg.getType()->isIntegerTy ())
          continue;

        params.push_back (make_pair (m_sev.symVar (arg), 
                                     getType (arg.getType ())));
      }
      
      if (m_sev.getTrackLevel () == ARR && 
          (!m_func.getName ().equals ("main"))) {
        // -- add array formal parameters 
        basic_block_t & entry = m_cfg->get_node (m_cfg->entry ());

        region_set_t onlyreads = m_sev.getMem().getOnlyReadRegions (m_func);
        region_set_t mods = m_sev.getMem().getModifiedRegions (m_func);
        region_set_t news = m_sev.getMem().getNewRegions (m_func);

        CRAB_LOG("cfg-mem",
                 errs() << "Function " << m_func.getName () << "\n";
                 errs() << "\tOnly-Read regions: " << m_sev.getMem().getOnlyReadRegions (m_func) << "\n";
                 errs() << "\tModified regions: " << m_sev.getMem().getModifiedRegions (m_func) << "\n";
                 errs() << "\tNew regions:" << m_sev.getMem().getNewRegions (m_func) << "\n");

        // (**) The same order must be used by the callsites

        // -- add input parameters
        for (auto a: onlyreads) { 
          if (const Value* s = isGlobalSingleton (a))
            params.push_back (make_pair (m_sev.symVar (*s), INT_TYPE)); 
          else 
            params.push_back (make_pair (m_sev.symVar (a), ARR_TYPE));
        }

        // -- add input/output parameters        
        for (auto a: mods) {

          if (news.find(a) != news.end()) continue;

          // -- for each parameter `a` we create a fresh version
          //    `a_in` where `a_in` acts as the input version of the
          //    parameter and `a` is the output version. Note that the
          //    translation of the function will not produce new
          //    versions of `a` since all array stores overwrite `a`.
          entry.set_insert_point_front ();

          varname_t a_in = m_sev.getVarFac().get (); // fresh variable
          if (const Value* s = isGlobalSingleton (a))
            entry.assign (m_sev.symVar (*s), z_lin_exp_t (a_in)); 
          else
            entry.assign (m_sev.symVar (a), z_lin_exp_t (a_in)); 
          // input version
          params.push_back (make_pair (a_in, ARR_TYPE));
          // output version
          if (const Value* s = isGlobalSingleton (a))
            params.push_back (make_pair (m_sev.symVar (*s), INT_TYPE));
          else
            params.push_back (make_pair (m_sev.symVar (a), ARR_TYPE));
        }

        // -- add output parameters
        for (auto a: news) {
          params.push_back (make_pair (m_sev.symVar (a), ARR_TYPE));
        }
        
      }

      variable_type retTy = UNK_TYPE;
      if (!CrabNoPtrArith || m_func.getReturnType ()->isIntegerTy ())
        retTy = getType (m_func.getReturnType ());

      function_decl<varname_t> decl (retTy, m_sev.getVarFac()[m_func], params);
      m_cfg->set_func_decl (decl);
      
    }
    
    if (CrabCFGSimplify) m_cfg->simplify ();
    if (CrabPrintCFG) crab::outs() << *m_cfg << "\n";
    return ;
  }

} // end namespace crab_llvm
