#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Support/CommandLine.h"

#include "crab_llvm/config.h"
#include "crab_llvm/Transforms/InsertInvariants.hh"
#include "crab_llvm/SymEval.hh"
#include "crab_llvm/AbstractDomainsImpl.hh"
#include "crab_llvm/Support/AbstractDomains.hh"
#include "crab_llvm/CfgBuilder.hh"

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

#include "crab/analysis/InvTable_traits.hpp"
#include "crab/analysis/AbsTransformer.hpp"
#include "crab/analysis/InterDS.hpp"

#include "boost/lexical_cast.hpp"

/* 
 * Instrument LLVM bitecode by inserting invariants computed by
 * crab. The invariants are inserted as special verifier.assume
 * instructions.
 */

using namespace llvm;
using namespace crab_llvm;


static llvm::cl::opt<bool>
LlvmCrabInsertEntries ("crab-add-invariants-at-entries", 
                       llvm::cl::desc ("Instrument basic block entries with invariants"),
                       llvm::cl::init (false));

static llvm::cl::opt<bool>
LlvmCrabInsertLoads ("crab-add-invariants-after-loads", 
                     llvm::cl::desc ("Instrument load instructions with invariants"),
                     llvm::cl::init (false));

namespace crab_llvm
{

  using namespace domain_impl;

  char crab_llvm::InsertInvariants::ID = 0;

  // helper
  inline bool reads_memory (const llvm::BasicBlock& B) {
    for (auto &I: B) 
      if (isa<LoadInst>(&I))
        return true;
    return false;
  }

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
      std::string snum = boost::lexical_cast<std::string> ((int)n);
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
                   Function* assumeFn, CallGraph* cg, const Function* insertFun,
                   const Twine &Name = "")
    {
      bool change = false;
      for (auto cst: csts)
      { 
        Value* cst_code = gen_code (cst, B, ctx, Name);
        if (cst_code) {
          CallInst *ci =  B.CreateCall (assumeFn, cst_code);

          // errs () << "Added " << *ci << " with " << *cst_code << "\n";

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
        return mk_bool (B, ctx, true);
      
      if (cst.is_contradiction ()) 
        return mk_bool (B, ctx, false);
      
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
          // cst can contain pointer variables representing their offsets.
          // We ignore them for now.
          if (!vv->getType()->isIntegerTy ())
            return nullptr;

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


  //! Instrument basic block entries.
  bool InsertInvariants::instrument_entries (z_lin_cst_sys_t csts, 
                                             llvm::BasicBlock* bb, 
                                             LLVMContext &ctx,
                                             CallGraph* cg) {
    IRBuilder<> Builder (ctx);
    Builder.SetInsertPoint (bb->getFirstNonPHI ());
    CodeExpander g;
    return g.gen_code (csts, Builder, ctx, m_assumeFn, cg, 
                       bb->getParent (), "crab_");
  }

  //! Instrument all load instructions in a basic block.
  //
  // The instrumentation is a bit involved because Crab gives us
  // invariants that hold either at the entry or at the exit of a
  // basic block but not at each program point. Thus, we need to take
  // the invariants that hold at the entry and propagate them locally
  // across the statements of the basic block. This will redo some
  // work but it's more efficient than storing all invariants at each
  // program point.
  template<typename AbsDomain>
  bool InsertInvariants::instrument_loads (AbsDomain pre, 
                                           basic_block_t& bb, 
                                           LLVMContext &ctx,
                                           CallGraph* cg) {
    typedef crab::analyzer::NumAbsTransformer 
        <AbsDomain,
         crab::analyzer::SummaryTable<cfg_t, AbsDomain>,
         crab::analyzer::CallCtxTable<cfg_t, AbsDomain> > num_abs_tr_t; 

    IRBuilder<> Builder (ctx);
    bool change=false;

    // FIXME: it will propagate forward pre through the basic block
    //        but ignoring callsites
    num_abs_tr_t vis (pre, nullptr, nullptr); 
    for (auto &s: bb) {

      s.accept (&vis); //propagate the invariant one statement forward

      const llvm::LoadInst* I = nullptr;
      z_lin_cst_t::variable_set_t load_vs;
      if (s.isArrRead ()) { 
        const ArrayLoad <z_number, varname_t>* load_stmt = 
            static_cast< const ArrayLoad <z_number, varname_t> *> (&s);
        if (boost::optional<const llvm::Value *> v = load_stmt->lhs ().name().get ()) {
          I = dyn_cast<const llvm::LoadInst> (*v);
          load_vs += (load_stmt->lhs ().name ());
        }
      }
      else if (s.isPtrRead ()) { 
        const PtrLoad <z_number, varname_t>* load_stmt = 
            static_cast< const PtrLoad <z_number, varname_t> *> (&s);
        if (boost::optional<const llvm::Value *> v = load_stmt->lhs ().get ()) {
          load_vs += (load_stmt->lhs ());
          I = dyn_cast<const llvm::LoadInst> (*v);
        }
      }
      
      if (!I)
        continue;

      AbsDomain inv = vis.inv ();
      
      // -- Remove array shadow variables otherwise llvm will
      //    get choked
      VariableFactory &vfac = m_crab->getVariableFactory ();
      auto shadows = vfac.get_shadow_vars ();
      crab::domain_traits::forget (inv, shadows.begin(), shadows.end());            
      
      // -- Filter out all constraints that do not use x.
      z_lin_cst_sys_t rel_csts;
      for (auto cst: inv.to_linear_constraint_system ()) {
        auto vs = cst.variables();
        if (!(vs & load_vs).empty ())
          rel_csts += cst;
      }
      
      // -- Insert assume's the next after I
      Builder.SetInsertPoint (const_cast<llvm::LoadInst*> (I));
      llvm::BasicBlock* InsertBlk = Builder.GetInsertBlock ();
      llvm::BasicBlock::iterator InsertPt = Builder.GetInsertPoint ();
      InsertPt++; // this is ok because LoadInstr cannot be terminators.
      Builder.SetInsertPoint (InsertBlk, InsertPt);
      CodeExpander g;
      change |= g.gen_code (rel_csts, Builder, ctx, m_assumeFn, cg, 
                            I->getParent()->getParent (), "crab_");
    }
  }

  bool InsertInvariants::runOnModule (llvm::Module &M)
  {
    if (!LlvmCrabInsertEntries && 
        !LlvmCrabInsertLoads)
      return false;

    m_crab = &getAnalysis<CrabLlvm> ();
    
    LLVMContext& ctx = M.getContext ();
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
    
#ifdef HAVE_DSA
    m_mem = MemAnalysis (&getAnalysis<SteensgaardDataStructures> (),
                         m_crab->getTrackLevel ());
#endif     

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
    if (F.isDeclaration () || F.empty () || F.isVarArg ()) 
      return false;

    CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass> ();
    CallGraph* cg = cgwp ? &cgwp->getCallGraph () : nullptr;

    VariableFactory &vfac = m_crab->getVariableFactory ();

    // FIXME: only used if InsertLoads is enabled
    CfgBuilder builder (F, vfac, &m_mem, false);
    cfg_t &cfg = builder.makeCfg ();
 
    bool change = false;
    for (auto &B : F) {

      if (LlvmCrabInsertEntries) {
        // --- Instrument basic block entry
        auto csts = m_crab->getPre (&B, false /*remove shadows*/);
        change |= instrument_entries (csts, &B, F.getContext(), cg);
      }

      if (LlvmCrabInsertLoads) {
        // --- We only instrument Load instructions
        if (reads_memory (B)) {
          // FIXME: choose as abstract domain m_crab->getAbsDomain()
          typedef crab::analyzer::inv_tbl_traits <arr_interval_domain_t, z_lin_cst_sys_t> inv_tbl_t;
          arr_interval_domain_t pre = 
              inv_tbl_t::unmarshall (m_crab->getPre (&B, true /*keep shadows*/));
          change |= instrument_loads (pre, cfg.get_node (&B), F.getContext (), cg);
        }
      }
    }
    return change;
  }

  void InsertInvariants::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();
    AU.addRequired<crab_llvm::CrabLlvm>();
#ifdef HAVE_DSA
    AU.addRequiredTransitive<llvm::SteensgaardDataStructures> ();
#endif 
    AU.addRequired<llvm::DataLayoutPass>();
    AU.addRequired<llvm::UnifyFunctionExitNodes> ();
    AU.addRequired<llvm::CallGraphWrapperPass> ();
    AU.addPreserved<llvm::CallGraphWrapperPass> ();
  } 

} // end namespace 

static llvm::RegisterPass<crab_llvm::InsertInvariants> 
X ("insert-crab-invs",
   "Insert invariants inferred by crab", 
   false, false);
