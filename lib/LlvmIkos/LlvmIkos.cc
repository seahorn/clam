#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "ikos_llvm/config.h"
#include "ikos_llvm/LlvmIkos.hh"
#include "ikos_llvm/SymEval.hh"
#include "ikos_llvm/Support/AbstractDomains.hh"
#if IKOS_MINOR_VERSION >= 2
#include <ikos/domains/term/term_util.hpp>
#include <ikos/domains/term_equiv.hpp>
#endif 
#include "ikos_llvm/Support/bignums.hh"

#include <ikos/analysis/FwdAnalyzer.hpp>

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

using namespace llvm;
using namespace llvm_ikos;

llvm::cl::opt<bool>
LlvmIkosPrintAns ("ikos-answer", llvm::cl::desc ("Print Ikos invariants"),
             llvm::cl::init (false));

llvm::cl::opt<IkosDomain>
LlvmIkosDomain("ikos-dom",
       llvm::cl::desc ("Ikos abstract domain used to infer invariants"),
       llvm::cl::values 
       (clEnumValN (INTERVALS, "int",
                    "Classical interval domain (default)"),
#if IKOS_MINOR_VERSION >= 2
        clEnumValN (INTERVALS_CONGRUENCES, "cong",
                    "Reduced product of intervals with congruences"),
        clEnumValN (ZONES , "zones",
                    "Difference-Bounds Matrix (or Zones) domain"),
        clEnumValN (OCTAGONS, "oct",
                   "Octagon domain"),
        clEnumValN (TERMS, "term",
                    "Term-enriched interval domain."),
#endif 
        clEnumValEnd),
       llvm::cl::init (INTERVALS));

llvm::cl::opt<bool>
LlvmIkosLive("ikos-live", 
        llvm::cl::desc("Run Ikos with live ranges"),
        llvm::cl::init (false));

llvm::cl::opt<enum TrackedPrecision>
LlvmIkosTrackLev("ikos-track-lvl",
   llvm::cl::desc ("Track level for Cfg and abstract domains"),
   cl::values (clEnumValN (REG, "reg", "Primitive registers only"),
               clEnumValN (PTR, "ptr", "REG + pointers"),
               clEnumValN (MEM, "mem", "PTR + memory content"),
               clEnumValEnd),
   cl::init (TrackedPrecision::REG));

llvm::cl::opt<bool>
LlvmIkosInterProc ("ikos-inter-proc",
             cl::desc ("Build inter-procedural Cfg"), 
             cl::init (false));


namespace domain_impl
{
  using namespace cfg_impl;
  using namespace ikos;

  // Numerical domains
  typedef interval_domain< z_number, varname_t > interval_domain_t;
#if IKOS_MINOR_VERSION >= 2
  // scalar versions
  typedef interval_congruence_domain< z_number, varname_t > ric_domain_t;
  typedef DBM< z_number, varname_t > dbm_domain_t;
  typedef octagon< z_number, varname_t > octagon_domain_t;
  //typedef ikos::term::TDomInfo<z_number, varname_t, interval_domain_t> idom_info;
  typedef interval_domain< z_number, ikos::term::StrVarAlloc_col::varname_t > str_interval_dom_t;
  typedef ikos::term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef anti_unif<idom_info>::anti_unif_t term_domain_t;  
  // array versions
  typedef array_smashing<interval_domain_t,z_number,varname_t> arr_interval_domain_t;
  typedef array_smashing<ric_domain_t,z_number,varname_t> arr_ric_domain_t;
  typedef array_smashing<dbm_domain_t,z_number,varname_t> arr_dbm_domain_t;
  typedef array_smashing<octagon_domain_t,z_number,varname_t> arr_octagon_domain_t;
  typedef array_smashing<term_domain_t,z_number,varname_t> arr_term_domain_t;
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

  using namespace analyzer;
  using namespace domain_impl;

  char llvm_ikos::LlvmIkos::ID = 0;

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


  //! Instrument llvm bitecode with certain invariants.
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
    unsigned m_num_inserts;

    CodeExpanderInvs (Module& M, AbsDomain inv, SymEval eval):
        m_B (IRBuilder<> (M.getContext ())), 
        m_ctx (M.getContext ()), 
        m_inv (inv), m_assumeFn (nullptr), m_eval (eval), 
        m_num_inserts (0) 
    { 
      AttrBuilder B;
      AttributeSet as = AttributeSet::get (m_ctx, 
                                           AttributeSet::FunctionIndex,
                                           B);
      m_assumeFn = dyn_cast<Function>
          (M.getOrInsertFunction ("verifier.assume", 
                                  as,
                                  Type::getVoidTy (m_ctx),
                                  Type::getInt1Ty (m_ctx),
                                  NULL));
    }              
    
    void visitLoadInst (LoadInst &I) 
    {
      m_B.SetInsertPoint (&I);

      // FIX: neither of these options are good.
#if 1
      // JN: This just keep a non-relational invariant for lhs. If
      // commented this code then we will insert too many redundant
      // invariants (those related to registers). Ideally, we would
      // like to keep all relational invariants among lhs of load
      // instructions.
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

      CodeExpander g;
      Value* cond = g.gen_code (csts, m_B, m_ctx);
      if (cond)
      {
        m_B.CreateCall (m_assumeFn, cond);
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


  bool LlvmIkos::runOnModule (llvm::Module &M)
  {
    // -- initialize from cli options
    m_absdom = LlvmIkosDomain;
    m_runlive = LlvmIkosLive;

#ifdef HAVE_DSA
    m_mem = MemAnalysis (&getAnalysis<SteensgaardDataStructures> (),
                         LlvmIkosTrackLev);
#endif     

    bool change=false;
    for (auto &f : M) 
      change |= runOnFunction (f); 
    
    if (LlvmIkosPrintAns) dump (M);
    return change;
  }

  bool LlvmIkos::runOnFunction (llvm::Function &F)
  {
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    VariableFactory vfac; 
    cfg_t cfg = CfgBuilder (F, vfac, &m_mem, LlvmIkosInterProc)();

    bool change=false;
    switch (m_absdom)
    {
      case INTERVALS:  
#if IKOS_MINOR_VERSION >= 2
          change = (LlvmIkosTrackLev >= MEM ? 
                    runOnCfg <arr_interval_domain_t> (cfg, F, vfac) : 
                    runOnCfg <interval_domain_t> (cfg, F, vfac)) ; 
#else
          change = runOnCfg <interval_domain_t> (cfg, F, vfac); 
#endif 
        break;
#if IKOS_MINOR_VERSION >= 2
      case INTERVALS_CONGRUENCES: 
         change = (LlvmIkosTrackLev >= MEM ? 
                   runOnCfg <arr_ric_domain_t> (cfg, F, vfac) : 
                   runOnCfg <ric_domain_t> (cfg, F, vfac)) ; 
        break;
      case ZONES: 
         change = (LlvmIkosTrackLev >= MEM ? 
                   runOnCfg <arr_dbm_domain_t> (cfg, F, vfac) :  
                   runOnCfg <dbm_domain_t> (cfg, F, vfac)) ; 
        break;
      case OCTAGONS: 
        change = (LlvmIkosTrackLev >= MEM ? 
                  runOnCfg <arr_octagon_domain_t> (cfg, F, vfac) : 
                  runOnCfg <octagon_domain_t> (cfg, F, vfac)) ; 
        break;
      case TERMS:
        change = (LlvmIkosTrackLev >= MEM ? 
                  runOnCfg <arr_term_domain_t> (cfg, F, vfac) : 
                  runOnCfg <term_domain_t> (cfg, F, vfac)) ; 
        break;
#endif
      default: assert(false && "Unsupported abstract domain");
    }
    return change;
  }

  template<typename AbsDomain>
  bool LlvmIkos::runOnCfg (cfg_t& cfg, llvm::Function &F, VariableFactory &vfac)
  {
    typedef typename NumFwdAnalyzer <cfg_t,AbsDomain,VariableFactory>::type analyzer_t;
    typedef typename llvm_ikos::SymEval <VariableFactory, z_lin_exp_t> sym_eval_t;

    analyzer_t analyzer (cfg, vfac, m_runlive);
    analyzer.Run (AbsDomain::top());

    // --- Translate internal abstract representation to integer
    //     linear constraints.
    for (auto &B : F)
    {
      AbsDomain pre = analyzer [&B];
      const llvm::BasicBlock *BB = &B;
      if (pre.is_bottom ())
        m_inv_map.insert (make_pair (BB, mkFALSE ()));
      else if (pre.is_top ())
        m_inv_map.insert (make_pair (BB, mkTRUE ()));        
      else
        m_inv_map.insert (make_pair (BB, toLinCst (pre)));
    }

    if (m_mem.getTrackLevel () < MEM)
      return false;

    // --- Insert instantiated array invariants in the bitecode.
    LLVMContext &ctx = F.getParent ()->getContext ();
    IRBuilder<> Builder (ctx);

    bool change = false;
    for (auto &B : F)
    {
      AbsDomain post = analyzer.get_post (&B);
      if (post.is_bottom () || post.is_top ()) continue;

      CodeExpanderInvs <AbsDomain, sym_eval_t> t (*(F.getParent ()), 
                                                  post, 
                                                  sym_eval_t (vfac, &m_mem));
      t.visit (&B);
      change |= (t.get_num_inserts () > 0);
    }
    
    return change;
  }


  // Write to standard output the invariants 
  void LlvmIkos::dump (llvm::Module &M) const
  {
    for (auto &F : M)
    {
      if (F.isDeclaration () || F.empty ()) continue;
      
        errs () << "\nFunction " << F.getName () << "\n";
        for (auto &B : F)
        {
          const llvm::BasicBlock * BB = &B;
          errs () << "\t" << BB->getName () << ": ";
          auto inv = this->operator[] (BB);
          errs () << inv << "\n";
        }
        errs () <<  "\n";
    }
  }

  void LlvmIkos::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();
#ifdef HAVE_DSA
    AU.addRequiredTransitive<llvm::SteensgaardDataStructures> ();
    AU.addRequired<llvm::DataLayoutPass>();
    AU.addRequired<llvm::UnifyFunctionExitNodes> ();
#endif 
  } 

} // end namespace llvm_ikos

static llvm::RegisterPass<llvm_ikos::LlvmIkos> 
X ("llvm-ikos",
   "Infer invariants using Ikos", 
   false, false);
   


