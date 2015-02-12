
/* Translate a LLVM function to a custom CFG. */

#include "include/CfgBuilder.hh"
#include "include/Support/CFG.hh"
#include "include/Support/bignums.hh"

#include "llvm/InstVisitor.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/APInt.h"
#include <boost/lexical_cast.hpp>

namespace llvm_ikos
{

  using namespace cfg_impl;
  using namespace llvm;
  using namespace std;

  /** Converts v to mpz_class. Assumes that v is signed */
  inline mpz_class toMpz (const APInt &v)
  {
    // Based on:
    // https://llvm.org/svn/llvm-project/polly/trunk/lib/Support/GICHelper.cpp
    // return v.getSExtValue ();
    
    APInt abs;
    abs = v.isNegative () ? v.abs () : v;
    
    const uint64_t *rawdata = abs.getRawData ();
    unsigned numWords = abs.getNumWords ();
    
    // TODO: Check if this is true for all platforms.
    mpz_class res;
    mpz_import(res.get_mpz_t (), numWords, 1, sizeof (uint64_t), 0, 0, rawdata);
    
    return v.isNegative () ? mpz_class(-res) : res;
  }

  struct SymExecBase {
  
    VariableFactory& m_vfac;

    SymExecBase(VariableFactory &vfac): m_vfac (vfac) { }

    bool isReadableVar (const llvm::Value &v)
    {
      if (!v.getType ()->isIntegerTy ()) return false;
      const ConstantInt*  c1 = dyn_cast<const ConstantInt> (&v);
      const ConstantExpr* c2 = dyn_cast<const ConstantExpr> (&v);
      return (!c1 && !c2);
    }

    varname_t symVar (const Value &v)
    {
      assert (isReadableVar (v));
      return m_vfac [v];
    }
  
    boost::optional<ZLinearExpression> lookup (const Value &v)
    {
      
      if (isa<ConstantPointerNull> (&v)) 
        return boost::optional<ZLinearExpression>(0);
      
      if (! (v.getType ()->isIntegerTy ()))
        return boost::optional<ZLinearExpression>();
      
      if (isReadableVar(v))
      {
        if (dyn_cast<const UndefValue> (&v))
          return boost::optional<ZLinearExpression>();        
        else
          return boost::optional<ZLinearExpression>(ZLinearExpression (symVar (v)));
      }
      
      if (const ConstantInt *c = dyn_cast<const ConstantInt> (&v))
      {
        if (c->getValue ().getMinSignedBits () > 64)
        {
          errs () << "Warning: " << toMpz (c->getValue ()).get_str ()  
                  << " too big for int64_t.\n";
        }
        else
          return boost::optional<ZLinearExpression>
              (ZLinearExpression (c->getValue ().getSExtValue ()));
      }
      
      return boost::optional<ZLinearExpression>();
    }
    
    bool isLogicalOp(const Instruction &I) const 
    {
      return (I.getOpcode() >= Instruction::And && 
              I.getOpcode() <= Instruction::Xor);
    }
    
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

    boost::optional<ZLinearConstraint> 
    gen_assertion (CmpInst &I, bool is_negated)
    {
      
      normalizeCmpInst(I);
      
      const Value& v0 = *I.getOperand (0);
      const Value& v1 = *I.getOperand (1);

      boost::optional<ZLinearExpression> op1 = lookup (v0);
      boost::optional<ZLinearExpression> op2 = lookup (v1);
      
      ZLinearConstraint res;
      
      if (op1 && op2) 
      {
        switch (I.getPredicate ())
        {
          case CmpInst::ICMP_EQ:
            ( (!is_negated) ? 
              res = ZLinearConstraint (*op1 == *op2) : 
              res = ZLinearConstraint (*op1 != *op2));
            break;
          case CmpInst::ICMP_NE:
            ( (!is_negated) ? 
              res = ZLinearConstraint (*op1 != *op2) : 
              res = ZLinearConstraint (*op1 == *op2));
            break;
          case CmpInst::ICMP_ULT:
          case CmpInst::ICMP_SLT:
            ( (!is_negated) ? 
              res = ZLinearConstraint (*op1 <= *op2 - 1) : 
              res = ZLinearConstraint (*op1 >= *op2));
            break; 
          case CmpInst::ICMP_ULE:
          case CmpInst::ICMP_SLE:
            ( (!is_negated) ? 
              res = ZLinearConstraint (*op1 <= *op2) : 
              res = ZLinearConstraint (*op1 >= *op2 + 1));
            break;
          default:  assert (false);
        }
        return boost::optional<ZLinearConstraint> (res);
      }
      else
        return boost::optional<ZLinearConstraint> ();
    }
  };

  struct SymExecVisitor : public InstVisitor<SymExecVisitor>, SymExecBase
  {
    basic_block_t& m_bb;
    
    SymExecVisitor (VariableFactory& vfac, basic_block_t & bb): 
        SymExecBase(vfac), m_bb(bb) {  }
        
    /// skip PHI nodes
    void visitPHINode (PHINode &I) {}
    
    void visitCmpInst (CmpInst &I) 
    {
    
      if (!isReadableVar (I)) return;
    
      varname_t lhs = symVar(I);

      bool covered = true;

      if (const Value *cond = dyn_cast<const Value> (&I))
      {
        Value::const_use_iterator it = cond->use_begin(), et = cond->use_end();
        for (; it!=et ; ++it)
        {
          const Instruction * User = dyn_cast<Instruction>(*it);
          
          if (const BranchInst *br = dyn_cast<const BranchInst> (User))
          { 
            // Comparison can be used as a "filter". We skip it here because
            // it will treated by SymExecCmpInstVisitor.
            if (br->isConditional () && br->getCondition() == cond) 
              continue;
          }    
          
          if (isa<const SelectInst> (User) ) 
          { // This case is (partially) covered by SymExecITEVisitor
            continue;
          }
          
          if (isa<const CastInst> (User) || isLogicalOp (*User)) 
          {
            // abstraction: source of imprecision
          }
          covered = false;
          break;
        }
      }
      if (!covered)
        m_bb.havoc(lhs);
    }    

    /// skip BranchInst
    void visitBranchInst (BranchInst &I) {}
    
    /// skip SelectInst
    void visitSelectInst(SelectInst &I) {}
  
    void visitBinaryOperator(BinaryOperator &I)
    {
      if (!isReadableVar (I)) return;
      
      varname_t lhs = symVar(I);
      
      switch (I.getOpcode ())
      {
        case BinaryOperator::Add:
        case BinaryOperator::Sub:
        case BinaryOperator::Mul:
        case BinaryOperator::UDiv:
        case BinaryOperator::SDiv:
        case BinaryOperator::Shl:
          doArithmetic (lhs, I);
          break;
          // case BinaryOperator::And:
          // case BinaryOperator::Or:
          // case BinaryOperator::Xor:
          // case BinaryOperator::AShr:
          // case BinaryOperator::LShr:
        default:
          m_bb.havoc(lhs);
          break;
      }
    }
    
    void doArithmetic (varname_t lhs, BinaryOperator &i)
    {
      const Value& v1 = *i.getOperand(0);
      const Value& v2 = *i.getOperand(1);
      
      boost::optional<ZLinearExpression> op1 = lookup (v1);
      boost::optional<ZLinearExpression> op2 = lookup (v2);
      
      if (!(op1 && op2)) return;
      
      switch(i.getOpcode())
      {
        case BinaryOperator::Add:
          m_bb.add (lhs, *op1, *op2);
          break;
        case BinaryOperator::Sub:
            if ((*op1).is_constant())            
            { // cfg does not support subtraction of a constant by a
              // variable because the ikos api for abstract domains
              // does not support it.
              m_bb.assign (lhs, ZLinearExpression ((*op1).constant ()));
              m_bb.sub (lhs, ZLinearExpression (lhs), *op2);
            }
            else
              m_bb.sub (lhs, *op1, *op2);
          break;
        case BinaryOperator::Mul:
          m_bb.mul (lhs, *op1, *op2);
          break;
        case BinaryOperator::SDiv:
          {
            if ((*op1).is_constant())            
            { // cfg does not support division of a constant by a
              // variable because the ikos api for abstract domains
              // does not support it.
              m_bb.assign (lhs, ZLinearExpression ((*op1).constant ()));
              m_bb.div (lhs, ZLinearExpression (lhs), *op2);
            }
            else
              m_bb.div (lhs, *op1, *op2);
          }
          break;
        case BinaryOperator::Shl:
          if ((*op2).is_constant())
          {
            ikos::z_number k = (*op2).constant ();
            int shift = boost::lexical_cast<int>(toStr (k));
            assert (shift >= 0);
            unsigned factor = 1;
            for (unsigned i = 0; i < (unsigned) shift; i++) 
              factor *= 2;
            m_bb.mul (lhs, *op1, ZLinearExpression (factor));            
          }
          break;
        default:
          m_bb.havoc(lhs);
          break;
      }
    }
    
    void visitTruncInst(TruncInst &I)              
    { doCast (I); }
    
    void visitZExtInst (ZExtInst &I)
    { doCast (I); }
    
    void visitSExtInst (SExtInst &I)
    { doCast (I); }
    
    void doCast(CastInst &I)
    {
      if (!isReadableVar (I)) return;

      varname_t dst = symVar (I);
      const Value& v = *I.getOperand (0); // the value to be casted

      boost::optional<ZLinearExpression> src = lookup (v);

      if (src)
        m_bb.assign(dst, *src);
      else
      {
        if (v.getType ()->isIntegerTy (1))
        {
          m_bb.assume ( ZLinearExpression (dst) >= ZLinearExpression (0) );
          m_bb.assume ( ZLinearExpression (dst) <= ZLinearExpression (1) );
        }
        else m_bb.havoc(dst);
      }
    }

    // void visitCallInst (CallInst &I) 
    // {
    //   // CallSite CS (&I);
    //   // const Function *fn = CS.getCalledFunction ();
    //   // if (!fn) return;      
    // }
    
    /// base case. if all else fails.
    void visitInstruction (Instruction &I)
    {
      if (!isReadableVar (I)) return;
      varname_t lhs = symVar(I);
      m_bb.havoc(lhs);
    }
    
  };

  struct SymExecPhiVisitor : public InstVisitor<SymExecPhiVisitor>, SymExecBase
  {
    // block where assignment/havoc will be inserted
    basic_block_t&    m_bb; 
    // incoming block of the PHI instruction
    const llvm::BasicBlock& m_inc_BB; 
    
    SymExecPhiVisitor (VariableFactory& vfac, 
                       basic_block_t& bb, 
                       const llvm::BasicBlock& inc_BB) : 
        SymExecBase(vfac), m_bb (bb), m_inc_BB(inc_BB)   {}
    
    void visitPHINode (PHINode &I) 
    {
      if (!isReadableVar (I)) return;
      
      const Value *LHS = dyn_cast<const Value>(&I);
      const Value &v = *I.getIncomingValueForBlock (&m_inc_BB);
      
      if (LHS == &v) return;
      
      varname_t lhs = symVar(I);
      boost::optional<ZLinearExpression> rhs = lookup(v);
      if (rhs) m_bb.assign(lhs, *rhs);
      else     m_bb.havoc(lhs);
    }
  };


  struct SymExecCmpInstVisitor : public InstVisitor<SymExecCmpInstVisitor>, SymExecBase
  {
    basic_block_t& m_bb;
    bool           m_is_negated;
    
    SymExecCmpInstVisitor (VariableFactory& vfac, 
                           basic_block_t& bb, 
                           bool is_negated):
        SymExecBase(vfac), m_bb (bb), m_is_negated(is_negated)   {}
    
    void visitCmpInst (CmpInst &I) 
    {
      
      boost::optional<ZLinearConstraint> cst = gen_assertion (I, m_is_negated);
      if (cst) m_bb.assume(*cst);
      
      if (isReadableVar (I))
      {
        varname_t lhs = symVar (I);
        if (m_is_negated)
          m_bb.assign (lhs, ZLinearExpression (0));
        else
          m_bb.assign (lhs, ZLinearExpression (1));
      }
    }
  };
  
  struct SymExecITEVisitor : public InstVisitor<SymExecITEVisitor>, SymExecBase
  {
    
    CfgBuilder&    m_builder;
    basic_block_t& m_bb;
    
    unsigned id;
    
    SymExecITEVisitor (VariableFactory& vfac, 
                       CfgBuilder&      builder,
                       basic_block_t&   bb ):
        SymExecBase(vfac), m_builder (builder), m_bb (bb), id (0)   
    { }
    
    void visitCmpInst (CmpInst &I) { }
  
    void visitSelectInst(SelectInst &I)
    {
      
      if (!isReadableVar (I)) return;
      
      varname_t lhs = symVar(I);
      
      const Value& cond = *I.getCondition ();
      boost::optional<ZLinearExpression> op0 = lookup (*I.getTrueValue ());
      boost::optional<ZLinearExpression> op1 = lookup (*I.getFalseValue ());
      
      if (!(op0 && op1)) return;
      
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
      
      // abstraction: source of imprecision
      m_bb.havoc(lhs);
    }
  };
  
  CfgBuilder::opt_basic_block_t CfgBuilder::lookup (const llvm::BasicBlock &B)  
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

  // return the new block inserted between src and dest if any
  CfgBuilder::opt_basic_block_t 
  CfgBuilder::execBr (llvm::BasicBlock &src, const llvm::BasicBlock &dst)
  {
    // the branch condition
    if (const BranchInst *br = dyn_cast<const BranchInst> (src.getTerminator ()))
    {
      if (br->isConditional ())
      {
        opt_basic_block_t Src = lookup(src);
        opt_basic_block_t Dst = lookup(dst);
        assert (Src && Dst);

        // Dummy BasicBlock 
        basic_block_label_t bb_id = llvm::BasicBlock::Create(m_func.getContext (),
                                                             create_bb_name ());
        
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
          SymExecCmpInstVisitor v (m_vfac, bb, br->getSuccessor (1) == &dst);
          if (llvm::Instruction *I = 
              dyn_cast<llvm::Instruction> (& const_cast<llvm::Value&>(c)))
            v.visit (I); 
          else
            errs () << "Warning: cannot generate guard from " << c << "\n";
        }
        return opt_basic_block_t(bb);
      }
      else add_edge (src,dst);
    }
    return opt_basic_block_t();    
  }

  void CfgBuilder::make_cfg()
  {
    //LOG ("ikos-verbose", errs () << "Begin building muzq CFG ... \n");

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
        SymExecVisitor v (m_vfac, *BB);
        v.visit (B);
      }
      else
      {
        opt_basic_block_t BB = lookup (B);
        assert (BB);

        // build an initial CFG block from bb but ignoring for now
        // branches, ite instructions and phi-nodes
        {
          SymExecVisitor v (m_vfac, *BB);
          v.visit (B);
        }
        
        for (const llvm::BasicBlock *dst : succs (*bb))
        {
          // move branch condition in bb to a new block inserted
          // between bb and dst
          opt_basic_block_t mid_bb = execBr (B, *dst);

          // phi nodes in dst are translated into assignments in the
          // predecessor
          {            
            SymExecPhiVisitor v (m_vfac, (mid_bb ? *mid_bb : *BB), B);
            v.visit (const_cast<llvm::BasicBlock &>(*dst));
          }
        }
        
        {
          SymExecITEVisitor v (m_vfac, *this, *BB);
          v.visit (B);
        }
      }
    }
    
    // unify multiple return blocks

    if (rets.size () == 1) m_cfg.set_exit (rets [0]->label ());
    else if (rets.size () > 1)
    {
      // Dummy BasicBlock 
      basic_block_label_t unified_ret_id = llvm::BasicBlock::Create(m_func.getContext (),
                                                                    create_bb_name ());

      basic_block_t &unified_ret = m_cfg.insert (unified_ret_id);

      for(unsigned int i=0; i<rets.size (); i++)
        *(rets [i]) >> unified_ret;
      m_cfg.set_exit (unified_ret.label ());
    }

    // Important to keep small the cfg
    m_cfg.simplify ();

    //LOG ("ikos-verbose", errs () << "Done muzq CFG. \n");    
    return ;
  }

} // end namespace llvm_ikos
