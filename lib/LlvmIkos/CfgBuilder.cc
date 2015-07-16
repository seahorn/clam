
/* 
 * Translate a LLVM function to a custom CFG understood by ikos-core.
 */

#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Pass.h"

#include <boost/lexical_cast.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include "boost/range/algorithm/set_algorithm.hpp"

#include "ikos_llvm/SymEval.hh"
#include "ikos_llvm/CfgBuilder.hh"
#include "ikos_llvm/Support/CFG.hh"
#include "ikos_llvm/Support/bignums.hh"

//TODO: translation of pointer arithmetic.

namespace llvm_ikos
{

  using namespace cfg_impl;
  using namespace llvm;
  using namespace std;

  VariableType getType (Type * ty)
  {
    if (ty->isIntegerTy ())
      return INT_TYPE;
    else if (ty->isPointerTy ())
      return PTR_TYPE;
    else
      return UNK_TYPE;
  }

  struct SymExecVisitor : 
      public InstVisitor<SymExecVisitor>, 
      private SymEval<VariableFactory, z_lin_exp_t>
  {
    basic_block_t& m_bb;
    MemAnalysis *m_mem;
    bool m_is_inter_proc;

    SymExecVisitor (VariableFactory& vfac, basic_block_t & bb,
                    MemAnalysis* mem, bool isInterProc): 
        SymEval<VariableFactory, z_lin_exp_t> (vfac, mem->getTrackLevel ()),
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
    
    void visitCmpInst (CmpInst &I) 
    {
    
      if (!isTracked (I)) return;
    
      varname_t lhs = symVar(I);

      bool covered = true;

      if (const Value *cond = dyn_cast<const Value> (&I))
      {
        for (auto &use: boost::make_iterator_range (cond->use_begin (),
                                                    cond->use_end ()))
        {
          const Instruction * User = dyn_cast<Instruction>(&use);
          
          if (const BranchInst *br = dyn_cast<const BranchInst> (User))
          { 
            // -- comparison can be used as a "filter". We skip it
            //    here because it will treated by SymExecCmpInstVisitor.
            if (br->isConditional () && br->getCondition() == cond) 
              continue;
          }    
          
          if (isa<const SelectInst> (User) ) 
          { // -- this case is covered by SymExecVisitor
            continue;
          }
          
          if (isa<const CastInst> (User) || isLogicalOp (*User)) 
          {
            // -- abstraction: source of imprecision
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
      
    void visitBinaryOperator(BinaryOperator &I)
    {
      if (!isTracked (I)) return;
      
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
      
      boost::optional<z_lin_exp_t> op1 = lookup (v1);
      boost::optional<z_lin_exp_t> op2 = lookup (v2);
      
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
          {
            if ((*op1).is_constant())            
            { // cfg does not support division of a constant by a
              // variable because the ikos api for abstract domains
              // does not support it.
              m_bb.assign (lhs, z_lin_exp_t ((*op1).constant ()));
              m_bb.div (lhs, z_lin_exp_t (lhs), *op2);
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
            m_bb.mul (lhs, *op1, z_lin_exp_t (factor));            
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
      if (!isTracked (I)) return;

      varname_t dst = symVar (I);
      const Value& v = *I.getOperand (0); // the value to be casted

      boost::optional<z_lin_exp_t> src = lookup (v);

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

    void visitLoadInst (LoadInst &I)
    {
      if (!isTracked (I)) return;
      
      int arr_idx = m_mem->getArrayId (*(I.getParent ()->getParent ()), 
                                       I.getPointerOperand ());
                                      
      if (arr_idx < 0) return;

      varname_t lhs = symVar (I);
      boost::optional<z_lin_exp_t> idx = lookup (*I.getPointerOperand ());
      if (!idx) return ;

      m_bb.array_load (lhs, symVar (arr_idx), *idx);
    }
    
    void visitStoreInst (StoreInst &I)
    {
      if (!isTracked (*I.getOperand (0))) return;

     
      boost::optional<z_lin_exp_t> idx = lookup (*I.getPointerOperand ());
      if (!idx) return;

      boost::optional<z_lin_exp_t> val = lookup (*I.getOperand (0));
      if (!val) return;

      int arr_idx = m_mem->getArrayId (*(I.getParent ()->getParent ()),
                                       I.getOperand (1)); 
                                      
      if (arr_idx < 0) return;

      // FIXME
      // This will ignore all the non-integer fields.
      // Maybe more appropriate is to create a different array for
      // each non-integer field.
      if (I.getOperand (0)->getType ()->isIntegerTy ())
        m_bb.array_store (symVar (arr_idx), *idx, *val, 
                          m_mem->isSingleton (arr_idx)); 
    }

    pair<varname_t, VariableType> normalizePar (Value& V)
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
        return make_pair (m_vfac [V], getType (V.getType ()));
    }

    void visitSelectInst(SelectInst &I)
    {
      
      if (!isTracked (I)) return;
      
      varname_t lhs = symVar(I);
      
      const Value& cond = *I.getCondition ();
      boost::optional<z_lin_exp_t> op0 = lookup (*I.getTrueValue ());
      boost::optional<z_lin_exp_t> op1 = lookup (*I.getFalseValue ());
      
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
      
      m_bb.select(lhs, symVar (cond),  *op0, *op1);
    }

    void visitReturnInst (ReturnInst &I)
    {
      if (!m_is_inter_proc) return;

      // -- skip return argument of main
      if (I.getParent ()->getParent ()->getName ().equals ("main")) 
        return;
      
      if (I.getNumOperands () > 0)
      {
        m_bb.ret (m_vfac [*(I.getOperand (0))], 
                  getType ((*I.getOperand (0)).getType ()));
        
      }
    }
    
    void visitCallInst (CallInst &I) 
    {
      CallSite CS (&I);

      Function * callee = CS.getCalledFunction ();

      // -- if no direct do nothing for now
      if (!callee) return;

      // -- skip if intrinsic
      if (callee->isIntrinsic ()) return;

      if (!m_is_inter_proc && 
          m_mem->getTrackLevel () >= MEM)
      {// -- havoc all modified nodes by the callee
        for (auto a : m_mem->getReadModArrays (I).second)
          m_bb.havoc (symVar (a));
        return;
      }

      if (m_is_inter_proc)
      {
        // -- add the callsite
        vector<pair<varname_t,VariableType> > actuals;
        for (auto &a : boost::make_iterator_range (CS.arg_begin(),
                                                   CS.arg_end()))
        { 
          Value *v = a.get();
          actuals.push_back (normalizePar (*v));
        }
        
        set<int> mods;
        if (m_mem->getTrackLevel () >= MEM)
        {
          // -- add the array actual parameters
          auto p = m_mem->getReadModArrays (I);
          auto reads = p.first;
          mods = p.second;
          set<int> all;      
          boost::set_union(reads, mods, std::inserter (all, all.end()));
          for (auto a : all)
            actuals.push_back (make_pair (symVar (a), ARR_TYPE));
        }
        
        if (getType (I.getType ()) != UNK_TYPE)
          m_bb.callsite (make_pair (m_vfac [I], getType (I.getType ())), 
                         m_vfac [*callee], actuals);
        else
          m_bb.callsite (m_vfac [*callee], actuals);
        
        // -- and havoc all modified nodes by the callee
        if (m_mem->getTrackLevel () >= MEM)
        {
          for (auto a : mods)
            m_bb.havoc (symVar (a));
        }
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
      const Value &v = *I.getIncomingValueForBlock (&m_inc_BB);
      
      if (LHS == &v) return;
      
      varname_t lhs = symVar(I);
      boost::optional<z_lin_exp_t> rhs = lookup(v);
      if (rhs) m_bb.assign(lhs, *rhs);
      else     m_bb.havoc(lhs);
    }
  };


  struct SymExecCmpInstVisitor : 
      public InstVisitor<SymExecCmpInstVisitor>, 
      private SymEval<VariableFactory, z_lin_exp_t>
  {
    basic_block_t& m_bb;
    bool m_is_negated;
    MemAnalysis *m_mem;    
    
    SymExecCmpInstVisitor (VariableFactory& vfac, 
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

    boost::optional<z_lin_cst_t> 
    gen_assertion (CmpInst &I, bool is_negated)
    {
      
      normalizeCmpInst(I);
      
      const Value& v0 = *I.getOperand (0);
      const Value& v1 = *I.getOperand (1);

      boost::optional<z_lin_exp_t> op1 = lookup (v0);
      boost::optional<z_lin_exp_t> op2 = lookup (v1);
      
      z_lin_cst_t res;
      
      if (op1 && op2) 
      {
        switch (I.getPredicate ())
        {
          case CmpInst::ICMP_EQ:
            ( (!is_negated) ? 
              res = z_lin_cst_t (*op1 == *op2) : 
              res = z_lin_cst_t (*op1 != *op2));
            break;
          case CmpInst::ICMP_NE:
            ( (!is_negated) ? 
              res = z_lin_cst_t (*op1 != *op2) : 
              res = z_lin_cst_t (*op1 == *op2));
            break;
          case CmpInst::ICMP_ULT:
          case CmpInst::ICMP_SLT:
            ( (!is_negated) ? 
              res = z_lin_cst_t (*op1 <= *op2 - 1) : 
              res = z_lin_cst_t (*op1 >= *op2));
            break; 
          case CmpInst::ICMP_ULE:
          case CmpInst::ICMP_SLE:
            ( (!is_negated) ? 
              res = z_lin_cst_t (*op1 <= *op2) : 
              res = z_lin_cst_t (*op1 >= *op2 + 1));
            break;
          default:  assert (false);
        }
        return boost::optional<z_lin_cst_t> (res);
      }
      else
        return boost::optional<z_lin_cst_t> ();
    }

    void visitCmpInst (CmpInst &I) 
    {
      
      boost::optional<z_lin_cst_t> cst = gen_assertion (I, m_is_negated);
      if (cst) m_bb.assume(*cst);
      
      if (isTracked (I))
      {
        varname_t lhs = symVar (I);
        if (m_is_negated)
          m_bb.assume (z_lin_exp_t (lhs) == z_lin_exp_t (0));
        else
          m_bb.assume (z_lin_exp_t (lhs) == z_lin_exp_t (1));
      }
    }
  };
  
  // struct SymExecITEVisitor : 
  //     public InstVisitor<SymExecITEVisitor>, 
  //     private SymEval<VariableFactory, z_lin_exp_t>
  // {
    
  //   CfgBuilder& m_builder;
  //   basic_block_t& m_bb;
  //   unsigned id;
  //   MemAnalysis *m_mem;
    
  //   SymExecITEVisitor (VariableFactory& vfac, 
  //                      CfgBuilder& builder, 
  //                      basic_block_t& bb, 
  //                      MemAnalysis* mem):
  //       SymEval<VariableFactory, z_lin_exp_t> (vfac, mem->getTrackLevel ()), 
  //       m_builder (builder), 
  //       m_bb (bb), id (0), m_mem (mem)
  //   { }
    
  //   void visitCmpInst (CmpInst &I) { }
  
  //   void visitSelectInst(SelectInst &I) { }
  // };

  CfgBuilder::CfgBuilder (Function &func, VariableFactory &vfac, 
                          MemAnalysis* mem, bool isInterProc): 
        m_func (func), 
        m_vfac (vfac), 
        m_id (0),
        m_cfg (&m_func.getEntryBlock (), (mem->getTrackLevel ())),
        m_mem (mem),
        m_is_inter_proc (isInterProc) { }    

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
        // FIXME: memory leak because the llvm basic block is not
        // linked to a builder.
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
          SymExecCmpInstVisitor v (m_vfac, bb, br->getSuccessor (1) == &dst, m_mem);
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
        SymExecVisitor v (m_vfac, *BB, m_mem, m_is_inter_proc);
        v.visit (B);
      }
      else
      {
        opt_basic_block_t BB = lookup (B);
        assert (BB);

        // -- build an initial CFG block from bb but ignoring for now
        //    branches, ite instructions and phi-nodes
        {
          SymExecVisitor v (m_vfac, *BB, m_mem, m_is_inter_proc);
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
        // {
        //   SymExecITEVisitor v (m_vfac, *this, *BB, m_mem);
        //   v.visit (B);
        // }
      }
    }
    
    // -- unify multiple return blocks

    if (rets.size () == 1) m_cfg.set_exit (rets [0]->label ());
    else if (rets.size () > 1)
    {
      // -- insert dummy BasicBlock 
      // FIXME: memory leak because the llvm basic block is not
      // linked to a builder.
      basic_block_label_t unified_ret_id = 
          llvm::BasicBlock::Create(m_func.getContext (),
                                   create_bb_name ());

      basic_block_t &unified_ret = m_cfg.insert (unified_ret_id);

      for(unsigned int i=0; i<rets.size (); i++)
        *(rets [i]) >> unified_ret;
      m_cfg.set_exit (unified_ret.label ());
    }

    // -- allocate the arrays
    if (m_mem->getTrackLevel () >= MEM)
    {
      basic_block_t & entry = m_cfg.get_node (m_cfg.entry ());
      for (auto node_id : m_mem->getAllocArrays (m_func))
      {
        entry.set_insert_point_front ();
        entry.array_init (m_vfac.get (node_id));
      }
      if (m_func.getName ().equals ("main"))
      {
        auto p = m_mem->getInOutArrays (m_func);
        auto in = p.first; auto out = p.second;
        set<int> all;      
        boost::set_union(in, out, std::inserter (all, all.end()));
        for (auto a: all)
        {
          entry.set_insert_point_front ();
          entry.array_init (m_vfac.get (a));
        }
      }
    }

    if (m_is_inter_proc)
    {
      // -- add function declaration
      if (!m_func.isVarArg ())
      {
        vector<pair<varname_t,VariableType> > params;
        for (llvm::Value &arg : boost::make_iterator_range (m_func.arg_begin (),
                                                            m_func.arg_end ()))
        params.push_back (make_pair (m_vfac [arg], getType (arg.getType ())));
        
        // -- add array formal parameters 
        if (m_mem->getTrackLevel () >= MEM && 
            (!m_func.getName ().equals ("main")))
        {
          auto p = m_mem->getInOutArrays (m_func);
          auto in = p.first; auto out = p.second;
          set<int> all;      
          boost::set_union(in, out, std::inserter (all, all.end()));
          for (auto a: all)
            params.push_back (make_pair (m_vfac.get (a), ARR_TYPE));
        }
        FunctionDecl<varname_t> decl (getType (m_func.getReturnType ()), 
                                      m_vfac[m_func], params);
        m_cfg.set_func_decl (decl);
      }
    }

    // Important to keep small the cfg
    m_cfg.simplify ();
    std::cout << m_cfg << "\n";
    return ;
  }

} // end namespace llvm_ikos
