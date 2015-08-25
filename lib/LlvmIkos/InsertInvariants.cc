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
#include "ikos_llvm/AbstractDomainsImpl.hh"
#include "ikos_llvm/Support/AbstractDomains.hh"
#include "ikos_llvm/Support/bignums.hh"

/* 
 * Instrument program by inserting invariants computed by Ikos. The
 * invariants are inserted as assume instructions into the LLVM
 * bitcode. The insertion happens at most once per basic block and
 * only if the block has a load instruction.
 */

extern llvm::cl::opt<llvm_ikos::IkosDomain> LlvmIkosDomain;

using namespace llvm;
using namespace llvm_ikos;

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
                      const Twine &Name)
    {
      assert (LHS->getType ()->isIntegerTy () && 
              RHS->getType ()->isIntegerTy ());
    
      Value *LHS1 = B.CreateZExtOrBitCast (LHS, Type::getInt64Ty (ctx), Name); 
      Value *RHS1 = B.CreateZExtOrBitCast (RHS, Type::getInt64Ty (ctx), Name); 

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
    bool gen_code (z_lin_cst_sys_t csts, IRBuilder<> B, LLVMContext &ctx,
                   Function* assumeFn, CallGraph* cg, Function* insertFun,
                   const Twine &Name = "")
    {
      bool change = false;
      for (auto cst: csts)
      { 
        Value* cst_code = gen_code (cst, B, ctx, Name);
        if (cst_code) {
          CallInst *ci =  B.CreateCall (assumeFn, cst_code);
          change = true;
          if (cg)
            (*cg)[insertFun]->addCalledFunction (CallSite (ci),
                                                 (*cg)[ci->getCalledFunction ()]);
        }
      }
      return change;
    }
    
    // post: return a value of bool type (Int1Ty) that contains the
    // computation of cst
    Value* gen_code (z_lin_cst_t cst, IRBuilder<> B, LLVMContext &ctx, 
                     const Twine &Name)
    {
      if (cst.is_tautology ())     
      {
        return mk_bool (B, ctx, true);
      }
      
      if (cst.is_contradiction ()) 
      { 
        return mk_bool (B, ctx, false);
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
                            ee, vv, Name);
          else if (n == -1) 
            ee = mk_bin_op (SUB, B, ctx, 
                            ee, vv, Name);
          else
            ee = mk_bin_op (ADD, B, ctx, 
                            ee, 
                            mk_bin_op ( MUL, B, ctx, mk_num (n, ctx), vv, Name), 
                            Name);
        }
        else
          return nullptr;
      }
      
      ikos::z_number c = -cst.expression().constant();
      Value* cc = mk_num (c, ctx);

      if (cst.is_inequality ())
        return B.CreateICmpSLE (ee, cc, Name);
      else if (cst.is_equality ())
        return B.CreateICmpEQ (ee, cc, Name);        
      else 
        return B.CreateICmpNE (ee, cc, Name);        
    }
  };

//// XXX If multiple load's the same assumptions will be inserted
////      multiple times
////
//   //! Instrument llvm bitecode with invariants.
//   //  To avoid a blow up in the code size we do it only for variables
//   //  which appear on the lhs of load instructions.
//   template<typename AbsDomain, typename SymEval>
//   struct CodeExpanderInvs : 
//       public InstVisitor<CodeExpanderInvs <AbsDomain, SymEval> >
//   {
//     IRBuilder<> m_B;
//     LLVMContext &m_ctx;
//     AbsDomain m_inv;
//     Function* m_assumeFn;
//     SymEval m_eval;
//     CallGraph* m_cg;
//     bool m_modified;

//     bool IsModified () const { 
//       return m_modified; 
//     }

//     CodeExpanderInvs (Module& M, AbsDomain inv, SymEval eval, 
//                       Function* assumeFn, CallGraph *cg):
//         m_B (IRBuilder<> (M.getContext ())), 
//         m_ctx (M.getContext ()), 
//         m_inv (inv), m_assumeFn (assumeFn), m_eval (eval),
//         m_cg (cg),
//         m_modified (false)
//     { }
    
//     void visitLoadInst (LoadInst &I) 
//     {
//       // To insert after the instruction
//       m_B.SetInsertPoint (&I);
//       auto insertPoint = m_B.GetInsertPoint ();
//       m_B.SetInsertPoint (++insertPoint);

// #if 0
//       // Insert a non-relational invariant for lhs. 
//       varname_t lhs = m_eval.symVar(I);
//       set<varname_t> vs;
//       for (auto c: toLinCst (m_inv))
//       {
//         for (auto v: c.variables ())
//           vs.insert (v.name ());
//       }
//       vs.erase (lhs);
//       domain_traits::forget (m_inv, vs.begin (), vs.end ());
// #endif 
//       auto csts = toLinCst (m_inv);

//       // errs () << "Inserting invariants " << I << "=" << csts << "\n";

//       CodeExpander g;
//       m_modified = g.gen_code (csts, m_B, m_ctx, 
//                                m_assumeFn, m_cg, I.getParent()->getParent());
//     }  

//     /// base case. if all else fails.
//     void visitInstruction (Instruction &I){}
    
//   };


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

    // XXX This is probably too conservative
    sym_eval_t s (vfac, m_ikos->getTrackLevel ());
    set<varname_t> phi_vs;
    for (auto &B : F)
      for (auto &I: B) {
        if (PHINode * PI = dyn_cast<PHINode>(&I)) 
          phi_vs.insert (s.symVar (I));
      }
    

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
      
      bool has_load = false;
      for (auto &I: B) {
        if (isa<LoadInst>(&I))
          has_load = true;
      }

      if (has_load) {
        num_abs_domain_t inv = num_abs_domain_t::top ();

        /// XXX use the invariants the hold at the exit of the block
        /// is very tricky due to the mismatch between the LLVM CFG
        /// and the IKOS CFG. This does not happen if we use the
        /// invariants that hold at the entry. To avoid these problems
        /// we could use the invariants and the entry and then
        /// propagate until the desired program point. However, this
        /// propagation is not implemented.
        inv += m_ikos->getPost (&B);

        /*
          If we just insert at the end of the block all the invariants
          returned by getPost we would get something like this:
 
            bb:
              i = phi (0, entry) (i', bb2)
              br ... bb2 ...
            bb2:
              i' = i+1
              assume (i'>=1 && i'<=9);
              assume (i>=1 && i <=9);  <--- this is not true the first iteration! 
              br bb

          `assume (i >=1 && i <=9)` is added because ikos actually
           analyzes a different program:

             bb: 
               i=0
               goto bb2
             bb2:
               i'=i+1
               i=i'  <---
             goto bb

           but `assume (i >=1 && i <=9)` actually holds when the phi
           node is executed.
        */

        // --- Forget variables that are set through a phi node
        //// XXX this is not enough because we can have bb -> bb1 -> bb2
        //// such that TI is in bb and the phi node is in bb2.
        // sym_eval_t s (vfac, m_ikos->getTrackLevel ());
        // set<varname_t> phi_vs;
        // TerminatorInst* TI = B.getTerminator ();
        // for (unsigned i=0, e = TI->getNumSuccessors(); i!=e; ++i) {
        //   for (auto &I: *(TI->getSuccessor(i))) {
        //     if (PHINode * PI = dyn_cast<PHINode>(&I)) {
        //       phi_vs.insert (s.symVar (I));
        //     }
        //   }
        // }
        domain_traits::forget (inv, phi_vs.begin (), phi_vs.end ());

        // --- Insert just before the terminator of the block
        Builder.SetInsertPoint (B.getTerminator ());

        auto csts = toLinCst (inv);

        CodeExpander g;
        change |= g.gen_code (csts, Builder, ctx, m_assumeFn, cg, &F, "ikos_");
      }

      //// XXX this might insert assumptions that only hold at the
      //// exit of the block in the middle of the block.
      // num_abs_domain_t inv = num_abs_domain_t::top ();
      // inv += m_ikos->getPost (&B);
      // CodeExpanderInvs <num_abs_domain_t, 
      //                   sym_eval_t> t (*(F.getParent ()), 
      //                                  inv, 
      //                                  sym_eval_t (vfac, m_ikos->getTrackLevel ()),
      //                                  m_assumeFn,
      //                                  cg);
      // t.visit (&B);
      // change |= t.IsModified ();
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
