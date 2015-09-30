/* 
 * Translate a LLVM function to a CFG language understood by Crab
 */

#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

#include "boost/tuple/tuple.hpp"
#include <boost/range/iterator_range.hpp>
#include "boost/range/algorithm/set_algorithm.hpp"

#include "crab_llvm/SymEval.hh"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/Support/CFG.hh"

using namespace llvm;

llvm::cl::opt<bool>
LlvmCrabCFGSimplify ("crab-cfg-simplify",
                       cl::desc ("Simplify Crab CFG"), 
                       cl::init (false),
                       cl::Hidden);

llvm::cl::opt<bool>
LlvmCrabPrintCFG ("crab-print-cfg",
                    cl::desc ("Print Crab CFG"), 
                    cl::init (false));
                    
/* 
 To allow to track memory ignoring pointer arithmetic. This makes
 sense because some crab analyses can reason about memory without
 having any knowledge about pointer arithmetic.
*/
llvm::cl::opt<bool>
LlvmCrabNoGEP ("crab-disable-ptr",
                 cl::desc ("Disable translation of pointer arithmetic in Crab CFG"), 
                 cl::init (false),
                 cl::Hidden);

namespace crab_llvm
{

  using namespace crab::cfg_impl;

  VariableType getType (Type * ty)
  {
    if (ty->isIntegerTy ())
      return INT_TYPE;
    else if (ty->isPointerTy ())
      return PTR_TYPE;
    else
      return UNK_TYPE;
  }

  PHINode* hasOnlyOnePHINodeUse (const Instruction &I) {
    const Value *v = &I;
    if (v->hasOneUse ()) {
      const Use &U = *(v->use_begin ());
      if (PHINode* PHI  = dyn_cast<PHINode> (U.getUser ())) {
        return PHI;
      }
    }
    return nullptr;
  }

  //! Translate flag conditions 
  struct SymExecConditionVisitor : 
      public InstVisitor<SymExecConditionVisitor>, 
      private SymEval<VariableFactory, z_lin_exp_t>
  {
    basic_block_t& m_bb;
    bool m_is_negated;
    MemAnalysis *m_mem;    
    
    SymExecConditionVisitor (VariableFactory& vfac, 
                             basic_block_t& bb, 
                             bool is_negated, 
                             MemAnalysis* mem):
        SymEval<VariableFactory, z_lin_exp_t> (vfac, mem->getTrackLevel ()), 
        m_bb (bb), 
        m_is_negated (is_negated),
        m_mem (mem)
    { }
    

    void normalizeCmpInst(CmpInst &I)
    {
      switch (I.getPredicate()){
        case ICmpInst::ICMP_UGT:	
        case ICmpInst::ICMP_SGT: I.swapOperands(); break; 
        case ICmpInst::ICMP_UGE:	
        case ICmpInst::ICMP_SGE: I.swapOperands(); break; 
        default: ;
      }
    }

    z_lin_cst_sys_t gen_assertion (CmpInst &I)
    {
      
      normalizeCmpInst(I);
      
      const Value& v0 = *I.getOperand (0);
      const Value& v1 = *I.getOperand (1);

      optional<z_lin_exp_t> op1 = lookup (v0);
      optional<z_lin_exp_t> op2 = lookup (v1);
      
      z_lin_cst_sys_t res;
      
      if (op1 && op2) 
      {
        switch (I.getPredicate ())
        {
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
            if (isVar (*op1))
              res += z_lin_cst_t (*op1 >= z_number (0));
            if (isVar (*op2))
              res += z_lin_cst_t (*op2 >= z_number (0));
          case CmpInst::ICMP_SLT:
            if (!m_is_negated)
              res += z_lin_cst_t (*op1 <= *op2 - 1);
            else
              res += z_lin_cst_t (*op1 >= *op2);
            break; 
          case CmpInst::ICMP_ULE:
            if (isVar (*op1))
              res += z_lin_cst_t (*op1 >= z_number (0));
            if (isVar (*op2))
              res += z_lin_cst_t (*op2 >= z_number (0));
          case CmpInst::ICMP_SLE:
            if (!m_is_negated)
              res += z_lin_cst_t (*op1 <= *op2);
            else
              res += z_lin_cst_t (*op1 >= *op2 + 1);
            break;
          default:  assert (false);
        }
      }
      return res;
    }

    void visitBinaryOperator(BinaryOperator &I)
    {
      // It searches only for this particular pattern:
      // 
      //   %o1 = icmp ...
      //   %o2 = icmp ...
      //   %f = and %o1 %o2 
      //   br %f bb1 bb2
      // 
      // and it adds *only* in bb1 the constraints from %o1 and %2. 
      // 
      // Note that we do not add any constraint in bb2 because we
      // would need to add a disjunction and the CFG language does not
      // allow that. 

      CmpInst* C1 = nullptr;
      CmpInst* C2 = nullptr;

      switch (I.getOpcode ())
      {
        case BinaryOperator::And:
          if (!m_is_negated) {
            if (I.getOperand (0)->getType ()->isIntegerTy ())
              C1 = dyn_cast<CmpInst> (I.getOperand (0));
            if (I.getOperand (1)->getType ()->isIntegerTy ())
              C2 = dyn_cast<CmpInst> (I.getOperand (1));
          }
          break;
        case BinaryOperator::Or:
          if (m_is_negated) {
            if (I.getOperand (0)->getType ()->isIntegerTy ())
              C1 = dyn_cast<CmpInst> (I.getOperand (0));
            if (I.getOperand (1)->getType ()->isIntegerTy ())
              C2 = dyn_cast<CmpInst> (I.getOperand (1));
          }
          break;
        default:
          if (isTracked (I))
            m_bb.havoc(symVar (I));
          break;
      }

      if (C1 && C2) {
        for (auto cst: gen_assertion (*C1)) {
          if (m_is_negated)
            m_bb.assume(cst.negate ());
          else
            m_bb.assume(cst);
        }
        for (auto cst: gen_assertion (*C2)) {
          if (m_is_negated)
            m_bb.assume(cst.negate ());
          else
            m_bb.assume(cst);
        }
      }
      
    }

    void visitCmpInst (CmpInst &I) 
    {

      if (!(isTracked (I) && 
            I.getOperand (0)->getType ()->isIntegerTy () &&
            I.getOperand (1)->getType ()->isIntegerTy ())) {
        return;    
      }

      for (auto cst: gen_assertion (I)) {
        m_bb.assume(cst);
      }
      
      varname_t lhs = symVar (I);
      if (m_is_negated)
        m_bb.assume (z_lin_exp_t (lhs) == z_lin_exp_t (0));
      else
        m_bb.assume (z_lin_exp_t (lhs) == z_lin_exp_t (1));
    }
  };

  //! Translate PHI nodes
  struct SymExecPhiVisitor : 
      public InstVisitor<SymExecPhiVisitor>, 
      private SymEval<VariableFactory, z_lin_exp_t>
  {
    // block where assignment/havoc will be inserted
    basic_block_t&    m_bb; 
    // incoming block of the PHI instruction
    const llvm::BasicBlock& m_inc_BB; 
    // memory analysis
    MemAnalysis *m_mem;

    SymExecPhiVisitor (VariableFactory& vfac, 
                       basic_block_t& bb, 
                       const llvm::BasicBlock& inc_BB, 
                       MemAnalysis* mem): 
        SymEval<VariableFactory, z_lin_exp_t> (vfac, mem->getTrackLevel ()), 
        m_bb (bb), m_inc_BB (inc_BB), m_mem (mem)
    { }
    
    void visitPHINode (PHINode &I) 
    {
      if (!isTracked (I)) return;

      const Value *LHS = dyn_cast<const Value>(&I);

      // --- Hook to ignore always integer shadow variables from
      //     SeaHorn ShadowMemDsa pass.
      if (LHS->getName ().startswith ("shadow.mem")) return;
      
      const Value &v = *I.getIncomingValueForBlock (&m_inc_BB);
      
      if (LHS == &v) return;

#if 0      
      // OptimizationXX:
      if (Instruction *II = dyn_cast<Instruction> (I.getIncomingValueForBlock (&m_inc_BB))) {
        if (hasOnlyOnePHINodeUse (*II))
          return;
      }
#endif 
      
      varname_t lhs = symVar(I);
      optional<z_lin_exp_t> rhs = lookup(v);
      if (rhs) 
        m_bb.assign(lhs, *rhs);
      else     
        m_bb.havoc(lhs);
    }
  };

  //! Translate the rest of instructions
  struct SymExecVisitor : 
      public InstVisitor<SymExecVisitor>, 
      private SymEval<VariableFactory, z_lin_exp_t>
  {
    const DataLayout* m_dl;
    basic_block_t& m_bb;
    MemAnalysis* m_mem;
    bool m_is_inter_proc;

    SymExecVisitor (const DataLayout* dl, VariableFactory& vfac, basic_block_t & bb,
                    MemAnalysis* mem, bool isInterProc): 
        SymEval<VariableFactory, z_lin_exp_t> (vfac, mem->getTrackLevel ()),
        m_dl (dl),
        m_bb (bb), 
        m_mem (mem),
        m_is_inter_proc (isInterProc)  
    { }

    bool isLogicalOp(const Instruction &I) const 
    {
      return (I.getOpcode() >= Instruction::And && 
              I.getOpcode() <= Instruction::Xor);
    }
        
    /// skip PHI nodes
    void visitPHINode (PHINode &I) {}

    /// skip BranchInst
    void visitBranchInst (BranchInst &I) {}
    
    void visitCmpInst (CmpInst &I) {
      // --- We translate I if it does not feed to the terminator of
      //     the block. Otherwise, it will be covered by execBr.

      //       if (!isTracked (I)) return;
      //       if (const Value *cond = dyn_cast<const Value> (&I))
      //       {
      //         TerminatorInst * TI = I.getParent()->getTerminator ();
      //         if (const BranchInst *br = dyn_cast<const BranchInst> (TI)) {
      //           if (!(br->isConditional () && br->getCondition() == cond)) {
      //             SymExecConditionVisitor v (m_vfac, m_bb, false, m_mem);
      //             auto csts = v.gen_assertion (I);
      //             if (csts.begin () != csts.end ()) {
      //               // select only takes a single constraint otherwise the
      //               // reasoning gets complicated if the analysis needs to
      //               // negate conjunction of constraints.
      //               // TODO: we just choose the first constraint arbitrarily.
      //               m_bb.select (symVar(I), *(csts.begin ()), 
      //                            z_lin_exp_t (1) /*tt*/, 
      //                            z_lin_exp_t (0) /*ff*/);
      //             }
      //             else
      //               m_bb.havoc (symVar (I));
      //           }
      //         }
      //       }
    }    
    
      
    void visitBinaryOperator(BinaryOperator &I)
    {
      if (!isTracked (I)) return;
      
      varname_t lhs = symVar(I);
      
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
#if 0
          // OptimizationXX: if the lhs has only one user and the user
          // is a phi node then we save one assignment.
          if (PHINode* PHI  = hasOnlyOnePHINodeUse (I)) {
              lhs = symVar (*PHI);
          }
#endif 
          doArithmetic (lhs, I);
          break;
        case BinaryOperator::And:
        case BinaryOperator::Or:
        case BinaryOperator::Xor:
          doBitwise (lhs, I);
          break;
        case BinaryOperator::LShr:
        default:
          m_bb.havoc(lhs);
          break;
      }
    }
    
    void doArithmetic (varname_t lhs, BinaryOperator &i)
    {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);
      
      optional<z_lin_exp_t> op1 = lookup (v1);
      optional<z_lin_exp_t> op2 = lookup (v2);
      
      if (!(op1 && op2)) return;
      
      switch(i.getOpcode())
      {
        case BinaryOperator::Add:
          m_bb.add (lhs, *op1, *op2);
          break;
        case BinaryOperator::Sub:
          if ((*op1).is_constant())            
          { // cfg does not support subtraction of a constant by a
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
          { // cfg does not support division of a constant by a
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
            // TODO: Cfg api does not support unsigned arithmetic
            // operations with both constant operands. Llvm frontend
            // should get rid of them.
            errs () << "Warning: ignored udiv with both constant operands\n";
            m_bb.havoc(lhs);
          }
          else if ((*op1).is_constant()) {           
            // cfg does not support division of a constant by a
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
            // cfg does not support rem of a constant by a
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
            // TODO: Cfg api does not support unsigned arithmetic
            // operations with both constant operands. Llvm frontend
            // should get rid of them.
            errs () << "Warning: ignored urem with constant operands\n";
            m_bb.havoc(lhs);
          }
          else if ((*op1).is_constant()) {           
            // cfg does not support rem of a constant by a
            // variable because the crab api for abstract domains
            // does not support it.
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
          else
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
          else
            m_bb.havoc(lhs);
          break;
        default:
          m_bb.havoc(lhs);
          break;
      }
    }

    void doBitwise (varname_t lhs, BinaryOperator &i)
    {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);
      
      optional<z_lin_exp_t> op1 = lookup (v1);
      optional<z_lin_exp_t> op2 = lookup (v2);
      
      if (!(op1 && op2)) return;
      
      switch(i.getOpcode())
      {
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
          m_bb.havoc(lhs);
          break;
      }
    }
    
    
    void visitZExtInst (ZExtInst &I)
    { 
      // This optimization tries to reduce the number variables within
      // a basic block. This will put less pressure on the numerical
      // abstract domain later on.
      /* --- Search for this idiom: 
         
         %_14 = zext i8 %_13 to i32
         %_15 = getelementptr inbounds [10 x i8]* @_ptr, i32 0, i32 %_14
         
         If all users of the zext instructions are gep's then we can
         skip the translation of the zext instruction.
      */
      Value* dest = &I; 
      bool all_gep = true;
      for (Use &U: boost::make_iterator_range(dest->use_begin(),
                                              dest->use_end())) {
        all_gep &= isa<GetElementPtrInst> (U.getUser ());
      }

      if (!all_gep) {
        doCast (I); 
      }

    }


    // void visitTruncInst(TruncInst &I)              
    // { doCast (I); }
    
    // void visitSExtInst (SExtInst &I)
    // { doCast (I); }
    
    void doCast(CastInst &I)
    {
      if (!isTracked (I)) return;

      varname_t dst = symVar (I);
      const Value& v = *I.getOperand (0); // the value to be casted

      optional<z_lin_exp_t> src = lookup (v);

      if (src)
        m_bb.assign(dst, *src);
      else
      {
        if (v.getType ()->isIntegerTy (1))
        {
          m_bb.assume ( z_lin_exp_t (dst) >= z_lin_exp_t (0) );
          m_bb.assume ( z_lin_exp_t (dst) <= z_lin_exp_t (1) );
        }
        else m_bb.havoc(dst);
      }
    }

    // This will cover the whole class of cast instructions
    void visitCastInst (CastInst &I) {
      doCast (I);
    }

    unsigned fieldOffset (const StructType *t, unsigned field) {
      return m_dl->getStructLayout (const_cast<StructType*>(t))->
          getElementOffset (field);
    }

    unsigned storageSize (const Type *t) {
      return m_dl->getTypeStoreSize (const_cast<Type*> (t));
    }

    void visitGetElementPtrInst (GetElementPtrInst &I)
    {
      if ( (!isTracked (*I.getPointerOperand ())) ||
           LlvmCrabNoGEP) {
        if (isTracked (I))
          m_bb.havoc (symVar (I));
        return;
      }

      optional<z_lin_exp_t> ptr = lookup (*I.getPointerOperand ());
      if (!ptr) {
        if (isTracked (I))
          m_bb.havoc (symVar (I));
        return;
      }

      varname_t res = symVar (I);

      // -- more efficient translation if the GEP offset is constant
      unsigned BitWidth = m_dl->getPointerTypeSizeInBits(I.getType());
      APInt Offset(BitWidth, 0);
      if (I.accumulateConstantOffset (*m_dl, Offset)) {
        m_bb.add (res, *ptr, z_lin_exp_t (toMpz (Offset).get_str ()));
        return;
      }

      m_bb.assign (res, *ptr);
      SmallVector<const Value*, 4> ps;
      SmallVector<const Type*, 4> ts;
      gep_type_iterator typeIt = gep_type_begin (I);
      for (unsigned i = 1; i < I.getNumOperands (); ++i, ++typeIt) {
        // strip zext if there is one
        if (const ZExtInst *ze = dyn_cast<const ZExtInst> (I.getOperand (i)))
          ps.push_back (ze->getOperand (0));
        else 
          ps.push_back (I.getOperand (i));
        ts.push_back (*typeIt);
      }

      for (unsigned i = 0; i < ps.size (); ++i) {
        if (const StructType *st = dyn_cast<const StructType> (ts [i]))
        {
          if (const ConstantInt *ci = dyn_cast<const ConstantInt> (ps [i]))
            m_bb.add (res, res, ikos::z_number (fieldOffset (st, ci->getZExtValue ())));
          else 
            assert (0);
        }
        else if (const SequentialType *seqt = dyn_cast<const SequentialType> (ts [i]))
        {
          optional<z_lin_exp_t> p = lookup (*ps[i]);
          assert (p);

          varname_t off = m_vfac.get ();
          m_bb.mul (off, *p, ikos::z_number (storageSize (seqt->getElementType ())));
          m_bb.add (res, res, off);
        }
      }
    }

    void visitLoadInst (LoadInst &I)
    { 
      /// --- Track only loads into integer variables
      Type *ty = I.getType();
      if (!ty->isIntegerTy ()) return;

      if (m_mem->getTrackLevel () == ARR) {
        int arr_idx = m_mem->getArrayId (*(I.getParent ()->getParent ()), 
                                         I.getPointerOperand ());
        if (arr_idx < 0) return;
        
        optional<z_lin_exp_t> idx = lookup (*I.getPointerOperand ());
        if (!idx) return ;
        
        m_bb.array_load (symVar (I), symVar (arr_idx), *idx,
                         ikos::z_number (m_dl->getTypeAllocSize (ty)));
      }
      else {
        if (isTracked (I))
          m_bb.havoc (symVar (I));
      }

    }
    
    void visitStoreInst (StoreInst &I)
    {
      /// --- Track only stores of integer values
      Type* ty = I.getOperand (0)->getType ();
      if (!ty->isIntegerTy ()) return;

      if (m_mem->getTrackLevel () == ARR) {        
        optional<z_lin_exp_t> idx = lookup (*I.getPointerOperand ());
        if (!idx) return;
        
        optional<z_lin_exp_t> val = lookup (*I.getOperand (0));
        if (!val) return;
        
        int arr_idx = m_mem->getArrayId (*(I.getParent ()->getParent ()),
                                         I.getOperand (1)); 
        if (arr_idx < 0) return;
        
        m_bb.array_store (symVar (arr_idx), *idx, *val, 
                          ikos::z_number (m_dl->getTypeAllocSize (ty)),
                          m_mem->isSingleton (arr_idx)); 
      }
    }

    void visitSelectInst(SelectInst &I)
    {
      
      if (!isTracked (I)) return;
      
      const Value& cond = *I.getCondition ();
      optional<z_lin_exp_t> op0 = lookup (*I.getTrueValue ());
      optional<z_lin_exp_t> op1 = lookup (*I.getFalseValue ());
      
      if (!(op0 && op1)) return;

      varname_t lhs = symVar(I);      
      if (const ConstantInt *ci = dyn_cast<const ConstantInt> (&cond))
      {
        if (ci->isOne ())
        {
          m_bb.assign (lhs, *op0);
          return;
        }
        else if (ci->isZero ())
        {
          m_bb.assign (lhs, *op1);
          return;
        }
      }
     
      m_bb.select(lhs, symVar (cond),  *op0, *op1);
    }

    void visitReturnInst (ReturnInst &I)
    {
      if (!m_is_inter_proc) return;

      // -- skip return argument of main
      if (I.getParent ()->getParent ()->getName ().equals ("main")) 
        return;
      
      if (I.getNumOperands () > 0) {
        if (isTracked (*(I.getOperand (0))))
          m_bb.ret ( symVar (*(I.getOperand (0))), 
                     getType ((*I.getOperand (0)).getType ()));
      }
    }

    pair<varname_t, VariableType> normalizeParam (Value& V)
    {
      if (Constant *cst = dyn_cast< Constant> (&V))
      { 
        varname_t v = m_vfac.get ();
        if (ConstantInt * intCst = dyn_cast<ConstantInt> (cst))
        {
          auto e = lookup (*intCst);
          if (e)
          {
            m_bb.assign (v, *e);
            return make_pair (v, INT_TYPE);
          }
        }
        m_bb.havoc (v);
        return make_pair (v, UNK_TYPE);
      }
      else
        return make_pair (symVar (V), getType (V.getType ()));
    }
    
    void visitCallInst (CallInst &I) 
    {
      CallSite CS (&I);
      Function * callee = CS.getCalledFunction ();

      if (!callee) {         
        // --- If HAVE_DSA then we have run first the devirt pass so
        //     if this is still happening is because DSA could not
        //     resolve the indirect call
        if (I.isInlineAsm ())
          errs () << "WARNING: skipped inline asm statement call " << I << "\n";
        else
          errs () << "WARNING: skipped indirect call " << I << "\n";

        // -- havoc return value
        Value *ret = &I;
        if (!ret->getType()->isVoidTy() && isTracked (*ret))
          m_bb.havoc (symVar (I));

        return;
      }

      // -- skip if intrinsic
      if (callee->isIntrinsic ()) return;

      // -- some special functions
      if (callee->getName ().equals ("verifier.assume")) {
        Value *cond = CS.getArgument (0);
        // strip zext if there is one
        if (const ZExtInst *ze = dyn_cast<const ZExtInst> (cond))
          cond = ze->getOperand (0);
        if (llvm::Instruction *I = dyn_cast<llvm::Instruction> (cond)) {
          SymExecConditionVisitor v (m_vfac, m_bb, false, m_mem);
          v.visit (I);
        }
        return;
      }

      if (callee->getName ().equals ("verifier.assume.not")) {
        Value *cond = CS.getArgument (0);
        // strip zext if there is one
        if (const ZExtInst *ze = dyn_cast<const ZExtInst> (cond))
          cond = ze->getOperand (0);
        if (llvm::Instruction *I = dyn_cast<llvm::Instruction> (cond)) {
          SymExecConditionVisitor v (m_vfac, m_bb, true, m_mem);
          v.visit (I);
        }
        return;
      }

      if ((!m_is_inter_proc) || callee->isVarArg()) {
        // -- havoc return value
        Value *ret = &I;
        if (!ret->getType()->isVoidTy() && isTracked (*ret))
          m_bb.havoc (symVar (I));
        
        // -- havoc all modified nodes by the callee
        if (m_mem->getTrackLevel () == ARR) {
          auto t = m_mem->getRefModNewArrays (I);
          auto mods = get<1> (t);
          for (auto a : mods)
            m_bb.havoc (symVar (a));
        }
      }
      else {
        vector<pair<varname_t,VariableType> > actuals;
        // -- add the scalar actual parameters
        for (auto &a : boost::make_iterator_range (CS.arg_begin(),
                                                   CS.arg_end())) {
          Value *v = a.get();
          if (!isTracked (*v)) 
            continue;

          actuals.push_back (normalizeParam (*v));
        }

        if (m_mem->getTrackLevel () == ARR) {
          // -- add the array actual parameters
          auto t = m_mem->getRefModNewArrays (I);
          auto refs = get<0> (t);
          auto mods = get<1> (t);
          auto news = get<2> (t);
          // Build sequence In's. Ref's . New's . New's where |In's| = |Ref's|
          for (auto a: refs) {
            varname_t a_in = m_vfac.get (); // fresh variable
            m_bb.assign (a_in, z_lin_exp_t (symVar (a))); 
            m_bb.havoc (symVar (a)); 
            actuals.push_back (make_pair (a_in, ARR_TYPE));
          }
          for (auto a: refs) 
            actuals.push_back (make_pair (symVar (a), ARR_TYPE));
          for (auto a: news) 
            actuals.push_back (make_pair (symVar (a), ARR_TYPE));

          // Make sure that the order of the actuals parameters is the
          // same than the order of the formal parameters when the
          // callee signature is built, search below for (**). This is
          // ensured because array names are globals and
          // getRefModNewArrays returns always the same order.
        }

        // -- add the callsite
        if ( (getType (I.getType ()) != UNK_TYPE) && isTracked (I))
          m_bb.callsite (make_pair (symVar (I), getType (I.getType ())), 
                         m_vfac [*callee], actuals);
        else
          m_bb.callsite (m_vfac [*callee], actuals);        
      }
      
    }

    /// base case. if all else fails.
    void visitInstruction (Instruction &I)
    {
      if (!isTracked (I)) return;

      if (isa<AllocaInst> (I)) return;

      varname_t lhs = symVar(I);
      m_bb.havoc(lhs);
    }
    
  };

  CfgBuilder::CfgBuilder (Function &func, VariableFactory &vfac, 
                          MemAnalysis* mem, bool isInterProc): 
        m_func (func), 
        m_vfac (vfac), 
        m_id (0),
        m_cfg (&m_func.getEntryBlock (), (mem->getTrackLevel ())),
        m_mem (mem),
        m_is_inter_proc (isInterProc),
        m_dl (func.getParent ()->getDataLayout ()){ }    

  CfgBuilder::~CfgBuilder () {
    // These extra blocks are not linked to a builder so llvm will not
    // free them.
    for (auto bb: m_extra_blks) {
      delete bb;
    }
  }

  CfgBuilder::opt_basic_block_t 
  CfgBuilder::lookup (const llvm::BasicBlock &B)  
  {
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
    basic_block_t &bb = m_cfg.insert ( BB);
    m_bb_map.insert (llvm_bb_map_t::value_type (BB, bb));
  }  

  // basic_block_t& 
  // CfgBuilder::split_block (basic_block_t & bb) 
  // {
  //   static unsigned id = 0;
  //   std::string new_bb_id_str(bb.name().str() + "_split_" + lexical_cast<string>(id));    
  //   basic_block_label_t new_bb_id(new_bb_id_str.str ());
  //   ++id;
  //   assert (m_bb_map.find (new_bb_id) == m_bb_map.end ()); 
  //   basic_block_t &new_bb = m_cfg.insert (new_bb_id);
  //   for (auto succ: bb.next_blocks ()) 
  //     new_bb >> lookup (succ);
  //   bb.reset_next_blocks ();
  //   bb >> new_bb ;
  //   return new_bb;
  // }

  basic_block_t& CfgBuilder::add_block_in_between (basic_block_t &src, 
                                                   basic_block_t &dst,
                                                   basic_block_label_t bb_id)
  {

    assert (m_bb_map.find(bb_id) == m_bb_map.end()); 

    basic_block_t &bb = m_cfg.insert (bb_id);

    src -= dst;
    src >> bb;
    bb >> dst;

    return bb;
  }  


  void CfgBuilder::add_edge (llvm::BasicBlock &S, 
                             const llvm::BasicBlock &D)
  {
    opt_basic_block_t SS = lookup(S);
    opt_basic_block_t DD = lookup(D);
    assert (SS && DD);
    *SS >> *DD;
  }  

  //! return the new block inserted between src and dest if any
  CfgBuilder::opt_basic_block_t CfgBuilder::execBr (llvm::BasicBlock &src, 
                                                    const llvm::BasicBlock &dst)
  {
    // -- the branch condition
    if (const BranchInst *br = dyn_cast<const BranchInst> (src.getTerminator ()))
    {
      if (br->isConditional ())
      {
        opt_basic_block_t Src = lookup(src);
        opt_basic_block_t Dst = lookup(dst);
        assert (Src && Dst);

        // -- dummy BasicBlock 
        basic_block_label_t bb_id = llvm::BasicBlock::Create(m_func.getContext (),
                                                             create_bb_name ());
        
        // -- remember this block to free it later because llvm will
        //    not do it.
        m_extra_blks.push_back (bb_id);

        basic_block_t &bb = add_block_in_between (*Src, *Dst, bb_id);
        
        const Value &c = *br->getCondition ();
        if (const ConstantInt *ci = dyn_cast<const ConstantInt> (&c))
        {
          if ((ci->isOne ()  && br->getSuccessor (0) != &dst) ||
              (ci->isZero () && br->getSuccessor (1) != &dst))
            bb.unreachable();
        }
        else 
        {
          if (llvm::Instruction *I = 
              dyn_cast<llvm::Instruction> (& const_cast<llvm::Value&>(c))) {
            SymExecConditionVisitor v (m_vfac, bb, br->getSuccessor (1) == &dst, m_mem);
            v.visit (I); 
          }
          else
            errs () << "Warning: cannot generate guard from " << c << "\n";
        }
        return opt_basic_block_t(bb);
      }
      else add_edge (src,dst);
    }
    return opt_basic_block_t();    
  }

  void doInitializer (Constant * cst, basic_block_t& bb, 
                      MemAnalysis* mem, varname_t a) {

    if (isa<ConstantAggregateZero> (cst)){
      //errs () << *cst << " is either struct or array with zero initialization\n";
      // a struct or array with all elements initialized to zero
      bb.assume_array (a, 0);
    }
    else if (ConstantInt * ci = dyn_cast<ConstantInt> (cst)) {
      //errs () << *cst << " is a scalar global variable\n";
      // an integer scalar global variable
      vector<ikos::z_number> val;
      val.push_back (ci->getZExtValue ());
      bb.array_init (a, val);
    }
    else if (ConstantDataSequential  *cds = dyn_cast<ConstantDataSequential> (cst)) {
      // an array of integers 1/2/4/8 bytes
      if (cds->getElementType ()->isIntegerTy ()) {
        //errs () << *cst << " is a constant data sequential\n";
        vector<ikos::z_number> vals;
        for (unsigned i=0; i < cds->getNumElements (); i++)
          vals.push_back (cds->getElementAsInteger (i));
        bb.array_init (a, vals);
      }
    }
    else if (GlobalAlias* alias = dyn_cast< GlobalAlias >(cst)) {
      doInitializer (alias->getAliasee (), bb, mem, a);
    }
    //else errs () << "Missed initialization of " << *cst << "\n";

  }

  void CfgBuilder::build_cfg()
  {

    for (auto &B : m_func) 
      add_block(B); 

    std::vector<basic_block_t*> rets;

    for (auto &B : m_func)
    {
      const llvm::BasicBlock *bb = &B;
      if (isa<ReturnInst> (B.getTerminator ()))
      {
        opt_basic_block_t BB = lookup (B);
        assert (BB);
        basic_block_t& bb = *BB;
        rets.push_back (&bb);
        SymExecVisitor v (m_dl, m_vfac, *BB, m_mem, m_is_inter_proc);
        v.visit (B);
      }
      else
      {
        opt_basic_block_t BB = lookup (B);
        assert (BB);

        // -- build an initial CFG block from bb but ignoring branches
        //    and phi-nodes
        {
          SymExecVisitor v (m_dl, m_vfac, *BB, m_mem, m_is_inter_proc);
          v.visit (B);
        }
        
        for (const llvm::BasicBlock *dst : succs (*bb))
        {
          // -- move branch condition in bb to a new block inserted
          //    between bb and dst
          opt_basic_block_t mid_bb = execBr (B, *dst);

          // -- phi nodes in dst are translated into assignments in
          //    the predecessor
          {            
            SymExecPhiVisitor v (m_vfac, (mid_bb ? *mid_bb : *BB), B, m_mem);
            v.visit (const_cast<llvm::BasicBlock &>(*dst));
          }
        }
      }
    }
    
    // -- unify multiple return blocks
    if (rets.size () == 1) { 
      m_cfg.set_exit (rets [0]->label ());
    }
    else if (rets.size () > 1) {
      // -- insert dummy BasicBlock 
      basic_block_label_t unified_ret_id = 
          llvm::BasicBlock::Create(m_func.getContext (),
                                   create_bb_name ());

      // -- remember this block to free it later because llvm will not do it.
      m_extra_blks.push_back (unified_ret_id);

      basic_block_t &unified_ret = m_cfg.insert (unified_ret_id);

      for(unsigned int i=0; i<rets.size (); i++)
        *(rets [i]) >> unified_ret;
      m_cfg.set_exit (unified_ret.label ());
    }

    SymEval<VariableFactory, z_lin_exp_t> s (m_vfac, m_mem->getTrackLevel ());

    if (m_mem->getTrackLevel () == ARR) {

      /// Allocate arrays with initial values 
      
      if (m_func.getName ().equals ("main")) {
        Module*M = m_func.getParent ();
        basic_block_t & entry = m_cfg.get_node (m_cfg.entry ());
        for (GlobalVariable &gv : boost::make_iterator_range (M->global_begin (),
                                                              M->global_end ())) {
          if (gv.hasInitializer ())  {
            int arr_idx = m_mem->getArrayId (m_func, &gv);
            if (arr_idx < 0) continue;
            entry.set_insert_point_front ();
            doInitializer (gv.getInitializer (), entry, 
                           m_mem, s.symVar (arr_idx));
          }
        } 
      }
      
      // HOOK: nodes which do not have an explicit initialization are
      // initially undefined. Instead, we assume they are zero
      // initialized so that Crab's array smashing can infer something
      // meaningful. This is correct because in presence of undefined
      // behaviour we can do whatever we want but it will, for
      // instance, preclude the analysis to detect things like
      // uninitialized variables and also if a more expressive array
      // domain is used then this initialization will make it more
      // imprecise.
      basic_block_t & entry = m_cfg.get_node (m_cfg.entry ());
      auto t = m_mem->getRefModNewArrays (m_func);
      auto news =  get<2> (t);
      for (auto n: news) {
        entry.set_insert_point_front ();
        entry.assume_array (s.symVar (n), 0);
      }
    }

    if (m_is_inter_proc) {
      // -- add function declaration
      assert (!m_func.isVarArg ());

      vector<pair<varname_t,VariableType> > params;
      // -- add scalar formal parameters
      for (llvm::Value &arg : boost::make_iterator_range (m_func.arg_begin (),
                                                          m_func.arg_end ())) {
        if (!s.isTracked (arg))
          continue;
        params.push_back (make_pair (s.symVar (arg), 
                                     getType (arg.getType ())));
      }
      
      if (m_mem->getTrackLevel () == ARR && 
          (!m_func.getName ().equals ("main"))) {
        // -- add array formal parameters 
        basic_block_t & entry = m_cfg.get_node (m_cfg.entry ());
        
        auto t = m_mem->getRefModNewArrays (m_func);
        auto refs =  get<0> (t);
        auto mods =  get<1> (t);
        auto news =  get<2> (t);
        // (**) Build sequence In's . Ref's . New's where |In's| = |Ref's|
        for (auto a: refs) {
          // -- for each ref parameter `a` we create a fresh version
          //    `a_in` where `a_in` acts as the input version of the
          //    parameter and `a` is the output version. Note that
          //    the translation of the function will not produce new
          //    versions of `a` since all array stores overwrite `a`.
          entry.set_insert_point_front ();
          varname_t a_in = m_vfac.get (); // fresh variable
          entry.assign (s.symVar (a), z_lin_exp_t (a_in)); 
            params.push_back (make_pair (a_in, ARR_TYPE));
        }
        for (auto a: refs) 
          params.push_back (make_pair (s.symVar (a), ARR_TYPE));
        for (auto a: news)
          params.push_back (make_pair (s.symVar (a), ARR_TYPE));
        
      }
      FunctionDecl<varname_t> decl (getType (m_func.getReturnType ()), 
                                    m_vfac[m_func], params);
      m_cfg.set_func_decl (decl);
      
    }
    
    if (LlvmCrabCFGSimplify) 
      m_cfg.simplify ();
    
    if (LlvmCrabPrintCFG)
      cout << m_cfg << "\n";
    
    return ;
  }

} // end namespace crab_llvm
