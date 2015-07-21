#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Analysis/CallGraph.h"

#include "ikos_llvm/config.h"
#include "ikos_llvm/Transforms/InsertInvariants.hh"
#include "ikos_llvm/SymEval.hh"
#include "ikos_llvm/Support/AbstractDomains.hh"
#include "ikos_llvm/Support/bignums.hh"

extern llvm::cl::opt<llvm_ikos::IkosDomain> LlvmIkosDomain;

using namespace llvm;
using namespace llvm_ikos;

namespace domain_impl
{
  using namespace cfg_impl;
  using namespace ikos;

  // Numerical domains to represent linear arithmetic
  typedef interval_domain< z_number, varname_t > interval_domain_t;
#if IKOS_MINOR_VERSION >= 2
  typedef interval_congruence_domain< z_number, varname_t > ric_domain_t;
  typedef DBM< z_number, varname_t > dbm_domain_t;
  typedef octagon< z_number, varname_t > octagon_domain_t;
#endif 
  typedef ikos::linear_expression<z_number, varname_t> z_lin_exp_t;
  typedef ikos::linear_constraint<z_number, varname_t> z_lin_cst_t;
  typedef ikos::linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;

  template<typename AbsDomain>
  z_lin_cst_sys_t toLinCst (AbsDomain inv)
  {
#if IKOS_MINOR_VERSION >= 2
    return inv.to_linear_constraint_system ();
#else
    return intervals_traits::to_linear_constraint_system (inv);
#endif       
  }

} // end namespace

namespace llvm_ikos
{

  using namespace domain_impl;

  char llvm_ikos::InsertInvariants::ID = 0;

  struct CodeExpander
  {
    enum bin_op_t { ADD, SUB, MUL };
    
    Value* mk_bin_op (bin_op_t Op,
                      IRBuilder<>B, 
                      LLVMContext &ctx,                  
                      Value *LHS, Value *RHS, 
                      const Twine &Name = "")
    {
      assert (LHS->getType ()->isIntegerTy () && 
              RHS->getType ()->isIntegerTy ());
    
      Value *LHS1 = B.CreateZExtOrBitCast (LHS, Type::getInt64Ty (ctx)); 
      Value *RHS1 = B.CreateZExtOrBitCast (RHS, Type::getInt64Ty (ctx)); 

      switch (Op)
      {
        case ADD: return  B.CreateAdd ( LHS1, RHS1, Name);
        case SUB: return  B.CreateSub ( LHS1, RHS1, Name);
        case MUL: return  B.CreateMul ( LHS1, RHS1, Name);
        default:
          llvm::errs () << "Unreachable\n";
          assert (0); exit (1); 
      }
    }
       
    Value* mk_num (ikos::z_number n, LLVMContext &ctx)
    {
      Type * ty = Type::getInt64Ty (ctx); 
      std::string snum = toStr (n);
      return ConstantInt::get (ty, APInt (64, snum, 10));
    }
    
    Value* mk_var (varname_t v)
    {
      if (!v.get ()) return nullptr;
      return const_cast<Value*> (*(v.get ()));
    }
    
    Value* mk_bool (IRBuilder<> B, LLVMContext &ctx, bool val)
    {
      return ConstantInt::get (Type::getInt1Ty (ctx), (val) ? 1U : 0U);
    }

    //! Generate llvm bitecode from a set of linear constraints    
    Value* gen_code (z_lin_cst_sys_t csts, IRBuilder<> B, LLVMContext &ctx)
    {
      Value* res = mk_bool (B, ctx, true);
      for (auto cst: csts)
      { 
        Value* cst_code = gen_code (cst, B, ctx);
        if (cst_code)
          res = B.CreateAnd (res, cst_code);
      }
      return res;
    }
    
    // post: return a value of bool type (Int1Ty) that contains the
    // computation of cst
    Value* gen_code (z_lin_cst_t cst, IRBuilder<> B, LLVMContext &ctx)
    {
      if (cst.is_tautology ())     
      {
        return mk_bool (B, ctx, true);
      }
      
      if (cst.is_contradiction ()) 
      { 
        llvm::errs () << "Constraint should not be a contradiction.\n";
        assert (0); exit (1); 
      }
      
      auto e = cst.expression() - cst.expression().constant();
      Value * ee = mk_num ( ikos::z_number ("0"), ctx);

      for (auto t : e)
      {
        ikos::z_number n  = t.first;
        if (n == 0) 
          continue;

        varname_t v = t.second.name();

        if (Value * vv = mk_var (v))
        {
          if (n == 1) 
            ee = mk_bin_op (ADD, B, ctx, 
                            ee, vv);
          else if (n == -1) 
            ee = mk_bin_op (SUB, B, ctx, 
                            ee, vv);
          else
            ee = mk_bin_op (ADD, B, ctx, 
                            ee, 
                            mk_bin_op ( MUL, B, ctx, mk_num (n, ctx), vv));
        }
        else
          return nullptr;
      }
      
      ikos::z_number c = -cst.expression().constant();
      Value* cc = mk_num (c, ctx);

      if (cst.is_inequality ())
        return B.CreateICmpSLE (ee, cc);
      else if (cst.is_equality ())
        return B.CreateICmpEQ (ee, cc);        
      else 
        return B.CreateICmpNE (ee, cc);        
    }
  };


  //! Instrument llvm bitecode with invariants.
  //  To avoid a blow up in the code size we do it only for variables
  //  which appear on the lhs of load instructions.
  template<typename AbsDomain, typename SymEval>
  struct CodeExpanderInvs : 
      public InstVisitor<CodeExpanderInvs <AbsDomain, SymEval> >
  {
    IRBuilder<> m_B;
    LLVMContext &m_ctx;
    AbsDomain m_inv;
    Function* m_assumeFn;
    SymEval m_eval;
    CallGraph* m_cg;
    unsigned m_num_inserts;

    CodeExpanderInvs (Module& M, AbsDomain inv, SymEval eval, 
                      Function* assumeFn, CallGraph *cg):
        m_B (IRBuilder<> (M.getContext ())), 
        m_ctx (M.getContext ()), 
        m_inv (inv), m_assumeFn (assumeFn), m_eval (eval),
        m_cg (cg),
        m_num_inserts (0) 
    { }
    
    void visitLoadInst (LoadInst &I) 
    {
      m_B.SetInsertPoint (&I);

#if 0
      // Insert a non-relational invariant for lhs. 
      varname_t lhs = m_eval.symVar(I);
      set<varname_t> vs;
      for (auto c: toLinCst (m_inv))
      {
        for (auto v: c.variables ())
          vs.insert (v.name ());
      }
      vs.erase (lhs);
      domain_traits::forget (m_inv, vs.begin (), vs.end ());
#endif 
      auto csts = toLinCst (m_inv);

      // errs () << "Inserting invariants " << I << "=" << csts << "\n";

      CodeExpander g;
      Value* cond = g.gen_code (csts, m_B, m_ctx);
      if (cond)
      {
       CallInst *ci =  m_B.CreateCall (m_assumeFn, cond);
        if (m_cg)
          (*m_cg)[I.getParent()->getParent()]->addCalledFunction (CallSite (ci),
                                           (*m_cg)[ci->getCalledFunction ()]);

        m_num_inserts++;
      }
    }  

    /// base case. if all else fails.
    void visitInstruction (Instruction &I){}
    
    unsigned get_num_inserts () const 
    {
      return m_num_inserts; 
    }

  };


  bool InsertInvariants::runOnModule (llvm::Module &M)
  {
    m_ikos = &getAnalysis<LlvmIkos> ();
    
    LLVMContext &ctx = M.getContext ();
    AttrBuilder B;
    AttributeSet as = AttributeSet::get (ctx, 
                                         AttributeSet::FunctionIndex,
                                         B);
    m_assumeFn = dyn_cast<Function>
                       (M.getOrInsertFunction ("verifier.assume", 
                                               as,
                                               Type::getVoidTy (ctx),
                                               Type::getInt1Ty (ctx),
                                               NULL));
    

    CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass> ();
    if (CallGraph *cg = cgwp ? &cgwp->getCallGraph () : nullptr)
      cg->getOrInsertFunction (m_assumeFn);

    bool change=false;
    for (auto &f : M) 
      change |= runOnFunction (f); 
    
    return change;
  }

  bool InsertInvariants::runOnFunction (llvm::Function &F)
  {
    typedef typename llvm_ikos::SymEval <VariableFactory, z_lin_exp_t> sym_eval_t;
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass> ();
    CallGraph* cg = cgwp ? &cgwp->getCallGraph () : nullptr;

    VariableFactory &vfac = m_ikos->getVariableFactory ();

    // --- Insert instantiated array invariants in the bitecode.
    LLVMContext &ctx = F.getParent ()->getContext ();
    IRBuilder<> Builder (ctx);

    bool change = false;
    for (auto &B : F)
    {

      //num_abs_domain_t should be LlvmIkosDomain
      typedef interval_domain_t num_abs_domain_t;

// #if IKOS_MINOR_VERSION >= 2
//       typedef dbm_domain_t num_abs_domain_t;
// #else
//       typedef interval_domain_t num_abs_domain_t;
// #endif 

      num_abs_domain_t inv = num_abs_domain_t::top ();
      auto csts = m_ikos->getPost (&B);
      for (auto cst: csts)
        inv += csts;

      CodeExpanderInvs <num_abs_domain_t, 
                        sym_eval_t> t (*(F.getParent ()), 
                                       inv, 
                                       sym_eval_t (vfac, m_ikos->getTrackLevel ()),
                                       m_assumeFn,
                                       cg);
      t.visit (&B);
      change |= (t.get_num_inserts () > 0);
    }
    
    return change;
  }

  void InsertInvariants::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();
    AU.addRequired<llvm_ikos::LlvmIkos>();

    AU.addRequired<llvm::CallGraphWrapperPass> ();
    AU.addPreserved<llvm::CallGraphWrapperPass> ();
  } 

} // end namespace llvm_ikos

static llvm::RegisterPass<llvm_ikos::InsertInvariants> 
X ("insert-ikos-invs",
   "Insert invariants inferred by Ikos", 
   false, false);
   


