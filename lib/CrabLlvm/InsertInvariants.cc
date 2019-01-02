#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/Statistic.h"

#include "crab_llvm/config.h"
#include "crab_llvm/wrapper_domain.hh"
#include "crab_llvm/Transforms/InsertInvariants.hh"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/CrabLlvm.hh"
#include "crab/analysis/abs_transformer.hpp"

/* 
 * Instrument LLVM bitecode by inserting invariants computed by
 * crab. The invariants are inserted as special verifier.assume
 * instructions.
 */

using namespace llvm;
using namespace crab_llvm;
using namespace crab::cfg;

enum InsertInvsLoc { NONE, BLOCK_ENTRY, AFTER_LOAD, ALL};
static cl::opt<InsertInvsLoc>
InsertInvs("crab-add-invariants", 
     cl::desc("Instrument code with invariants"),
     cl::values(
	 clEnumValN (NONE, "none", "None"),
         clEnumValN (BLOCK_ENTRY, "block-entry",
                     "Add invariants that hold at each block entry"),
         clEnumValN (AFTER_LOAD, "after-load",
                     "Add invariants that hold after each load instruction"),
         clEnumValN (ALL, "all",
                    "Add invariants at all above locations")),
     cl::init (NONE));
            
#define DEBUG_TYPE "crab-insert-invars"

STATISTIC(NumInstrBlocks, "Number of blocks instrumented with invariants");
STATISTIC(NumInstrLoads, "Number of load inst instrumented with invariants");

namespace crab_llvm {

  char crab_llvm::InsertInvariants::ID = 0;

  // helper
  inline bool reads_memory (const llvm::BasicBlock& B) {
    for (auto &I: B) 
      if (isa<LoadInst>(&I))
        return true;
    return false;
  }

  struct CodeExpander {
    enum bin_op_t { ADD, SUB, MUL };
    
    Value* mk_bin_op (bin_op_t Op,
                      IRBuilder<>B, 
                      LLVMContext &ctx,                  
                      Value *LHS, Value *RHS, 
                      const Twine &Name) {
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
          llvm_unreachable("Code expander only understands add/sub/mul ops");
      }
    }
       
    Value* mk_num (number_t n, LLVMContext &ctx) {
      Type * ty = Type::getInt64Ty (ctx); 
      return ConstantInt::get (ty, APInt (64, n.get_str(), 10));
    }
    
    Value* mk_var (varname_t v) {
      if (!v.get ()) return nullptr;
      return const_cast<Value*> (*(v.get ()));
    }
    
    Value* mk_bool (IRBuilder<> B, LLVMContext &ctx, bool val) {
      return ConstantInt::get (Type::getInt1Ty (ctx), (val) ? 1U : 0U);
    }

    //! Generate llvm bitecode from a set of linear constraints    
    //  TODO: generate bitecode from disjunctive linear constraints.
    bool gen_code (lin_cst_sys_t csts, IRBuilder<> B, LLVMContext &ctx,
                   Function* assumeFn, CallGraph* cg,
		   const Function* insertFun, const Twine &Name = "") {
      bool change = false;
      for (auto cst: csts) {
        Value* cst_code = gen_code (cst, B, ctx, Name);
        if (cst_code) {
          CallInst *ci =  B.CreateCall (assumeFn, cst_code);
          // errs () << "Added " << *ci << " with " << *cst_code << "\n";
          change = true;
          if (cg) {
            (*cg)[insertFun]->addCalledFunction
	      (CallSite (ci), (*cg)[ci->getCalledFunction ()]);
	  }
	       
        }
      }
      return change;
    }
    
    // post: return a value of bool type (Int1Ty) that contains the
    // computation of cst
    Value* gen_code (lin_cst_t cst, IRBuilder<> B, LLVMContext &ctx, 
                     const Twine &Name) {
      if (cst.is_tautology ())     
        return mk_bool (B, ctx, true);
      
      if (cst.is_contradiction ()) 
        return mk_bool (B, ctx, false);
      
      auto e = cst.expression() - cst.expression().constant();
      Value * ee = mk_num (number_t("0"), ctx);

      for (auto t : e) {
        number_t n  = t.first;
        if (n == 0) continue; 

        varname_t v = t.second.name();

        if (Value * vv = mk_var (v)) {
          // cst can contain pointer variables representing their offsets.
          // We ignore them for now.
          if (!vv->getType()->isIntegerTy ())
            return nullptr;

          if (n == 1) {
            ee = mk_bin_op(ADD, B, ctx, ee, vv, Name);
	  } else if (n == -1) {
            ee = mk_bin_op(SUB, B, ctx, ee, vv, Name);
	  } else {
            ee = mk_bin_op(ADD, B, ctx, ee, 
			   mk_bin_op(MUL, B, ctx, mk_num (n, ctx), vv, Name), 
			   Name);
	  }
        }
        else
          return nullptr;
      }
      
      number_t c = -cst.expression().constant();
      Value* cc = mk_num (c, ctx);

      if (cst.is_inequality ()) {
        return B.CreateICmpSLE (ee, cc, Name);
      } else if (cst.is_equality ()) {
        return B.CreateICmpEQ (ee, cc, Name);        
      } else {
        return B.CreateICmpNE (ee, cc, Name);
      }
    }
  };


  //! Instrument basic block entries.
  bool InsertInvariants::
  instrument_entries (lin_cst_sys_t csts, llvm::BasicBlock* bb, 
		      LLVMContext &ctx, CallGraph* cg) {

    // If the block is an exit we do not instrument it.
    const ReturnInst *ret = dyn_cast<const ReturnInst> (bb->getTerminator ());
    if (ret) return false;

    IRBuilder<> Builder (ctx);
    Builder.SetInsertPoint (bb->getFirstNonPHI ());
    CodeExpander g;
    NumInstrBlocks++;
    return g.gen_code (csts, Builder, ctx, m_assumeFn, cg, 
                       bb->getParent (), "crab_");
  }

  //! Instrument all load instructions in a basic block.
  //
  // The instrumentation is a bit involved because Crab gives us
  // invariants that hold either at the entry or at the exit of a
  // basic block but not at each program point. Thus, we need to take
  // the invariants that hold at the entry and propagate (rebuild)
  // them locally across the statements of the basic block. This will
  // redo some work but it's more efficient than storing all
  // invariants at each program point.
  template<typename AbsDomain>
  bool InsertInvariants::
  instrument_loads(AbsDomain inv, basic_block_t& bb, LLVMContext &ctx, CallGraph* cg) {
    // -- it will propagate forward inv through the basic block
    //    but ignoring callsites    
    typedef crab::analyzer::intra_abs_transformer<AbsDomain> abs_tr_t; 
    typedef array_load_stmt<number_t,varname_t> array_load_stmt_t;
    typedef ptr_load_stmt<number_t,varname_t> ptr_load_stmt_t;    
    
    IRBuilder<> Builder (ctx);
    bool change=false;
    abs_tr_t vis (&inv);
    
    for (auto &s: bb) {
      s.accept (&vis); //propagate the invariant one statement forward
      const LoadInst* I = nullptr;
      lin_cst_t::variable_set_t load_vs;
      if (s.is_arr_read()) { 
        const array_load_stmt_t* load_stmt = static_cast<const array_load_stmt_t*>(&s);
        if (boost::optional<const Value *> v = load_stmt->lhs().name().get()) {
          I = dyn_cast<const LoadInst>(*v);
          load_vs += load_stmt->lhs();
        }
      }
      else if (s.is_ptr_read()) { 
        const ptr_load_stmt_t* load_stmt = static_cast<const ptr_load_stmt_t*>(&s); 
        if (boost::optional<const Value *> v = load_stmt->lhs().name().get()) {
          load_vs += load_stmt->lhs();
          I = dyn_cast<const LoadInst>(*v);
        }
      }
      
      if (!I) continue;

      CrabLlvmPass* crab = &getAnalysis<CrabLlvmPass>();

      if (inv.is_top ()) continue;
      // -- Filter out all constraints that do not use x.
      lin_cst_sys_t rel_csts;
      for (auto cst: inv.to_linear_constraint_system ()) {
        auto vs = cst.variables();
        if (!(vs & load_vs).empty ())
          rel_csts += cst;
      }

      // -- Insert assume's the next after I
      Builder.SetInsertPoint (const_cast<LoadInst*> (I));
      llvm::BasicBlock* InsertBlk = Builder.GetInsertBlock ();
      llvm::BasicBlock::iterator InsertPt = Builder.GetInsertPoint ();
      InsertPt++; // this is ok because LoadInstr cannot be terminators.
      Builder.SetInsertPoint (InsertBlk, InsertPt);
      CodeExpander g;
      NumInstrLoads++;
      change |= g.gen_code (rel_csts, Builder, ctx, m_assumeFn, cg, 
                            I->getParent()->getParent (), "crab_");
    }
    return change;
  }

  bool InsertInvariants::runOnModule (Module &M) {
    if (InsertInvs == NONE) return false;

    LLVMContext& ctx = M.getContext ();
    AttrBuilder B;
    AttributeList as = AttributeList::get (ctx, 
					   AttributeList::FunctionIndex,
					   B);
    m_assumeFn = dyn_cast<Function>
                       (M.getOrInsertFunction ("verifier.assume", 
                                               as,
                                               Type::getVoidTy (ctx),
                                               Type::getInt1Ty (ctx)));
    
    CallGraphWrapperPass *cgwp =getAnalysisIfAvailable<CallGraphWrapperPass>();
    if (CallGraph *cg = cgwp ? &cgwp->getCallGraph () : nullptr)
      cg->getOrInsertFunction (m_assumeFn);

    bool change=false;
    for (auto &f : M) {
      change |= runOnFunction (f); 
    }
    
    return change;
  }

  #define INSTR_LOAD(DOM,PRE,BB,CTX,CG,RES)  \
  DOM inv;                                   \
  getAbsDomWrappee (PRE, inv);               \
  RES |= instrument_loads (inv, BB, CTX, CG);
   
  bool InsertInvariants::runOnFunction (Function &F)
  {
    if (F.isDeclaration () || F.empty () || F.isVarArg ()) 
      return false;

    CrabLlvmPass* crab = &getAnalysis<CrabLlvmPass> ();

    if (!crab->has_cfg(F))
      return false;
      
    cfg_ref_t cfg = crab->get_cfg(F);

    CallGraphWrapperPass *cgwp =getAnalysisIfAvailable<CallGraphWrapperPass>();
    CallGraph* cg = cgwp ? &cgwp->getCallGraph () : nullptr;

    bool change = false;
    for (auto &B : F) {

      // -- if the block has an unreachable instruction we skip it.
      bool skip_block = false;
      for (auto &I: B) {
	if (isa<UnreachableInst>(I)) {
	  skip_block=true;
	  break;
	}
      }
      if (skip_block) continue;
      
      if (InsertInvs == BLOCK_ENTRY || InsertInvs == ALL) {
        // --- Instrument basic block entry
        auto pre = crab->get_pre(&B, false /*remove shadows*/);
	if (!pre) {
	  continue;
	}
        auto csts = pre->to_linear_constraints ();
        change |= instrument_entries (csts, &B, F.getContext(), cg);
      }

      if (InsertInvs == AFTER_LOAD || InsertInvs == ALL) {
        // --- We only instrument Load instructions
        if (reads_memory (B)) {

          auto pre = crab->get_pre(&B, true /*keep shadows*/);
	  if (!pre) {
	    continue;
	  }
          // --- Figure out the type of the wrappee
          switch (pre->getId ()) {
	    #ifdef HAVE_ALL_DOMAINS  
            case GenericAbsDomWrapper::ric: {
              INSTR_LOAD(ric_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }
            case GenericAbsDomWrapper::term_intv: {
              INSTR_LOAD(term_int_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }	      
            #endif 	    
            case GenericAbsDomWrapper::intv:{
              INSTR_LOAD(interval_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg,change);
              break;
            }
            case GenericAbsDomWrapper::split_dbm: {
              INSTR_LOAD(split_dbm_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }
            case GenericAbsDomWrapper::term_dis_intv: {
              INSTR_LOAD(term_dis_int_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }
            case GenericAbsDomWrapper::boxes: {
              INSTR_LOAD(boxes_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }
            case GenericAbsDomWrapper::oct: {
              INSTR_LOAD(oct_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }
            case GenericAbsDomWrapper::pk: {
              INSTR_LOAD(pk_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }
            case GenericAbsDomWrapper::num: {
              INSTR_LOAD(num_domain_t, pre,
			 cfg.get_node(&B), F.getContext(), cg, change);
              break;
            }
	      
            default :
              report_fatal_error("abstract domain not supported by --crab-add-invariants");
          }
        }
      }
    }
    return change;
  }

  void InsertInvariants::getAnalysisUsage (AnalysisUsage &AU) const {
    AU.setPreservesAll ();
    AU.addRequired<crab_llvm::CrabLlvmPass>();
    AU.addRequired<UnifyFunctionExitNodes> ();
    AU.addRequired<CallGraphWrapperPass> ();
    AU.addPreserved<CallGraphWrapperPass> ();
  } 

} // end namespace 

static RegisterPass<crab_llvm::InsertInvariants> 
X ("insert-crab-invs", "Instrument bitcode with invariants inferred by crab", 
   false, false); 
   
