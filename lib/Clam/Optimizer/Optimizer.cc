#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

#include "clam/config.h"
#include "clam/CfgBuilder.hh"
#include "clam/Clam.hh"
#include "clam/Transforms/Optimizer.hh"

#include "crab/analysis/abs_transformer.hpp"
#include "crab/numbers/wrapint.hpp"
#include "crab/config.h"
#include "crab/support/debug.hpp"

#include <type_traits>

using namespace llvm;

/**
 * Optimize LLVM bitcode by using invariants produced by Clam.
 * 
 * This module translates Crab linear constraints into LLVM bitcode.
 * There are two issues that makes the code a bit more
 * complicated. First, Clam only keeps invariants at the entry or exit
 * of each basic block. Therefore, depending on user options we need
 * to re-run the abstract interpreter (only up to the end of a block)
 * to produce invariants at each location. Second, most Clam numerical
 * abstract domains use signed mathematical integers. However, LLVM
 * uses machine arithmetic. Therefore, there is a filtering process to
 * make sure we only generate LLVM bitcode for linear constraints
 * whose validity still holds on machine arithmetic.
 **/

/* Begin LLVM pass options */
static cl::opt<clam::InvariantsLocation>
InvLoc("crab-opt-add-invariants",
    cl::desc("Instrument code with (linear) invariants at specific location"),
    cl::values(clEnumValN(clam::InvariantsLocation::NONE,
			  "none", "Do not add invariants"),
	       clEnumValN(clam::InvariantsLocation::BLOCK,
			  "block-entry",
			  "Add invariants at the entry of each basic block"),
	       clEnumValN(clam::InvariantsLocation::LOOP_HEADER,
			  "loop-header",
			  "Add invariants only at loop headers"),
	       clEnumValN(clam::InvariantsLocation::LOAD_INST,
			  "after-load",
			  "Add invariants after each load instruction"),
	       clEnumValN(clam::InvariantsLocation::ALL,
			  "all",
			  "Add invariants at all locations (very verbose)")),
       cl::init(clam::InvariantsLocation::NONE));

llvm::cl::opt<bool>
RemoveDeadCode("crab-opt-dce",
	 llvm::cl::desc("DCE using Crab invariants"),
	 llvm::cl::init(true));

llvm::cl::opt<bool>
ReplaceWithConstants("crab-opt-replace-with-constants",
	 llvm::cl::desc("Replace values with constants inferred by Crab"),
	 llvm::cl::init(false));
/* End LLVM pass options */

#define DEBUG_TYPE "crab-opt"

STATISTIC(NumDeadBlocks, "Number of dead blocks");
STATISTIC(NumDeadEdges, "Number of dead edges");
STATISTIC(NumInstrBlocks, "Number of blocks instrumented with invariants");
STATISTIC(NumInstrLoads, "Number of load inst instrumented with invariants");

namespace {

using namespace clam;  
using namespace crab::cfg;

static bool readMemory(const llvm::BasicBlock &B) {
  return std::any_of(B.begin(), B.end(),
		     [](const Instruction &I) {
		       return isa<LoadInst>(I);
		     });
}
static bool hasUnreachable(const llvm::BasicBlock &B) {
  return std::any_of(B.begin(), B.end(),
		     [](const Instruction &I) {
		       return isa<UnreachableInst>(I);
		     });
}
  
static bool requireDominatorTree(InvariantsLocation val) {
  return (val == InvariantsLocation::BLOCK ||
	  val == InvariantsLocation::LOOP_HEADER ||
	  val == InvariantsLocation::ALL);
}

static bool requireLoopInfo(InvariantsLocation val) {
  return val == InvariantsLocation::LOOP_HEADER;
}


// Convert a Crab expression into LLVM bitcode
class CodeExpander {
private:  
  enum bin_op_t { ADD, SUB, MUL };

  static Value *mkBinOp(bin_op_t Op, IRBuilder<> &B, Value *LHS, Value *RHS,
			const Twine &Name) {
    assert(LHS->getType()->isIntegerTy() && RHS->getType()->isIntegerTy());
    switch (Op) {
    case ADD:
      return B.CreateAdd(LHS, RHS, Name);
    case SUB:
      return B.CreateSub(LHS, RHS, Name);
    case MUL:
      return B.CreateMul(LHS, RHS, Name);
    }
  }

  static Value *mkNum(number_t n, IntegerType *ty, LLVMContext &ctx) {
    return ConstantInt::get(ty, n.get_str(), 10);
  }

  static Value *mkVar(varname_t v) {
    return (v.get() ? const_cast<Value *>(*(v.get())) : nullptr);
  }

  static Value *mkBool(LLVMContext &ctx, bool val) {
    return ConstantInt::get(Type::getInt1Ty(ctx), (val) ? 1U : 0U);
  }

  static IntegerType *getIntType(varname_t var) {
    if (!var.get()) {
      return nullptr;
    } else {
      Type *Ty = (const_cast<Value *>(*(var.get())))->getType();
      if (IntegerType *ITy = dyn_cast<IntegerType>(Ty)) {
        return ITy;
      }
    }
    return nullptr;
  }

  // static Value* normalizeCst(Value *Cst) {
  //   Value *ICmpLHS;
  //   ConstantInt *ICmpRHS;
  //   Value *X;
  //   ICmpInst::Predicate Pred;
  //   if (match(Cst, m_ICmp(Pred, m_Value(ICmpLHS), m_ConstantInt(ICmpRHS)))) {
  //     if (Pred == ICmpInst::ICMP_SLE) {
  // 	if (ICmpRHS->isNegative() && match(ICmpLHS, m_Neg(m_Value(X)))) {
  // 	  // Rewrite sub 0, %x <= -k into x >= k
  // 	  Value *normCst = new ICmpInst(cast<Instruction>(Cst),
  // 					ICmpInst::ICMP_SGE, X,
  // 					ConstantInt::get(X->getType(),
  // 							 ICmpRHS->getValue().abs()),
  // 					Cst->getName());
  // 	  cast<Instruction>(Cst)->replaceAllUsesWith(normCst);
  // 	  return normCst;
  // 	}
  //     }
  //   }
  //   return Cst;
  // }
  
  static bool mayOverflow (const number_t &n, crab::wrapint::bitwidth_t b) {
    auto max = crab::wrapint::get_signed_max(b).get_signed_bignum();
    auto min = crab::wrapint::get_signed_min(b).get_signed_bignum();
    return (n < min || n > max);
  };

  // post: return a value of bool type(Int1Ty) that contains the
  // computation of cst
  static Value *genCode(const lin_cst_t &cst, IRBuilder<> &B, LLVMContext &ctx,
			DominatorTree *DT, const Twine &Name) {
    
    if (cst.is_tautology()) {
      return nullptr; // mkBool(ctx, true);
    }
    if (cst.is_contradiction()) {
      return mkBool(ctx, false);
    }
    // translate only expressions of LLVM integer type
    IntegerType *ty = nullptr;
    for (auto v : cst.variables()) {
      if (!ty) {
        ty = getIntType(v.name());
      } else {
        if (ty != getIntType(v.name())) {
          ty = nullptr;
          break;
        }
      }
    }

    if (!ty) {
      return nullptr;
    }

    // more sanity checks before we insert the invariants
    for (auto t : cst.expression()) {
      varname_t v = t.second.name();
      if (Value *vv = mkVar(v)) {
        // cst can contain pointer variables representing their offsets.
        // We ignore them for now.
        if (!vv->getType()->isIntegerTy()) {
          return nullptr;
        }
        if (Instruction *Def = dyn_cast<Instruction>(vv)) {
          // check definition of invariant variable dominates the
          // block where the invariant will be inserted.
          BasicBlock *User = B.GetInsertBlock();
          if (Def->getParent() == User && isa<PHINode>(Def)) {
            // definition is a PHI node and its user is in the same
            // basic block.  Since we only insert invariants after the
            // last PHI node, we are ok.
            continue;
          } else if (DT && !(DT->dominates(Def, User))) {
            // llvm::errs() << *Def << " does not dominate its use at block "
            //             << User->getName() << "\n";
            return nullptr;
          }
        }
      } else {
        return nullptr;
      }
    }

    auto e = cst.expression() - cst.expression().constant();
    Value *ee = mkNum(number_t("0"), ty, ctx);
    for (auto t : e) {
      number_t n = t.first;
      if (n == 0) {
        continue;
      }

      // HACK: skip constraints with coefficients that do not fit into ty
      if (mayOverflow(n, ty->getBitWidth())) {
	return nullptr;
      }
      
      varname_t v = t.second.name();
      Value *vv = mkVar(v);
      assert(vv);
      assert(vv->getType()->isIntegerTy());
      if (n == 1) {
        ee = mkBinOp(ADD, B, ee, vv, Name);
      } else if (n == -1) {
        ee = mkBinOp(SUB, B, ee, vv, Name);
      } else {
        ee = mkBinOp(ADD, B, ee,
		     mkBinOp(MUL, B, mkNum(n, ty, ctx), vv, Name), Name);
      }
      assert(ee);
    }

    number_t c = -cst.expression().constant();
    // HACK: skip constraints whose rhs that do not fit into ty
    if (mayOverflow(c, ty->getBitWidth())) {
      return nullptr;
    }
    
    Value *cc = mkNum(c, ty, ctx);
    if (cst.is_inequality()) {
      return B.CreateICmpSLE(ee, cc, Name);
    } else if (cst.is_equality()) {
      return B.CreateICmpEQ(ee, cc, Name);
    } else {
      return B.CreateICmpNE(ee, cc, Name);
    }
  }
public:
  /** Generate llvm bitcode from a set of linear constraints.
   *
   * Given {x >= y, z <= 5, ...} it produces the LLVM bitcode:
   *   b1 := ICmp x geq y
   *   llvm.assume(b1)
   *   b2 := ICmp z leq 5
   *   llvm.assume(b2)
   *   ...
   **/
  bool genCode(const lin_cst_sys_t &csts, IRBuilder<> &B, LLVMContext &ctx,
	       Function *assumeFn, CallGraph *cg, DominatorTree *DT,
	       const Function *insertFun, const Twine &Name = "") const {
    bool change = false;
    for (auto cst : csts) {
      if (Value *cst_code = genCode(cst, B, ctx, DT, Name)) {
        CallInst *ci = B.CreateCall(assumeFn, cst_code);
        change = true;
        if (cg) {
          (*cg)[insertFun]->
	    addCalledFunction(ci, (*cg)[ci->getCalledFunction()]);
        }
      }
    }
    return change;
  }
  
};

// Generate bitcode for the value of v if it is a constant
Constant *getConstantInt(CfgBuilder *clamCfgBuilder, clam_abstract_domain inv, const Value &v) {
  if (v.getType()->isIntegerTy()) {
    llvm::Optional<var_t> lhs = clamCfgBuilder->getCrabVariable(v);
    if (lhs.hasValue()) {
      auto interval = inv[lhs.getValue()];
      if (boost::optional<number_t> constant_opt = interval.singleton()) {
	if ((*constant_opt).fits_int64()) {
	  return ConstantInt::get(v.getType(), (int64_t)(*constant_opt), 10);	      
	}
      } else {
      CRAB_LOG("clam-opt",
	       crab::outs() << "Found crab variable " << lhs.getValue();
	       llvm::errs() << " for " << v << " ";
	       crab::outs() << "but " << interval << " is not a constant\n";);
      }
    } else {
      CRAB_LOG("clam-opt",
	       llvm::errs() << "Not found crab variable for " << v << "\n";);
    }
  }
  return nullptr;
}
  
/*
 * Reconstruct the invariants that hold after each statement. 
 *
 * For memory reasons, Crab only gives us invariants that hold either
 * at the entry or at the exit of a basic block but not at each
 * program point. Thus, we need to take the invariants that hold at
 * the entry and propagate (rebuild) them locally across the
 * statements of the basic block. This will redo some work but it's
 * more efficient than storing all invariants at each program point.
 * 
 * Op must implement two operations:
 * 
 *  bool Skip(const statement_t &)
 *  void Process(const statement_t &s, const clam_abstract_domain &inv_after_s)
 */ 
template<class Op>  
bool GenericInstrumentStatement(clam_abstract_domain inv, basic_block_t &bb, Op op) {
  // Forward propagation through the basic block but ignoring
  // callsites. This might be imprecise if the analysis was
  // inter-procedural because we cannot reconstruct all the context
  // that the inter-procedural analysis had during the analysis.
  using abs_tr_t = crab::analyzer::intra_abs_transformer<basic_block_t,
                                                         clam_abstract_domain>;
  abs_tr_t vis(inv);
  bool change = false;
  for (auto &s : bb) {
    s.accept(&vis); // propagate the invariant one statement forward
    const clam_abstract_domain &next_inv = vis.get_abs_value();
    if (next_inv.is_top())
      continue;
    if (op.Skip(s))
      continue;
    op.Process(s, next_inv);
    change = true;
  }
  return change;
}



// Convert a Crab statement to a LoadInst.
Instruction* getLoadInst(const statement_t &s) {
  using array_load_t =
    array_load_stmt<basic_block_label_t, number_t, varname_t>;
  using load_from_ref_t =
    load_from_ref_stmt<basic_block_label_t, number_t, varname_t>;

  if (s.is_arr_read()) {
    const array_load_t *load_stmt = static_cast<const array_load_t *>(&s);
    if (auto v = load_stmt->lhs().name().get()) {
      if (auto LI = dyn_cast<const LoadInst>(*v)) {
	return const_cast<LoadInst*>(LI);
      }
    }
  } else if (s.is_ref_load()) {
    const load_from_ref_t *load_stmt =
      static_cast<const load_from_ref_t *>(&s);
    if (auto v = load_stmt->lhs().name().get()) {
      if (auto LI = dyn_cast<const LoadInst>(*v)) {
	return const_cast<LoadInst*>(LI);
      } 
    }
  } 
  return nullptr;
}
  
class InstrumentLoadStmt {
  IRBuilder<> &m_IB;
  Instruction  *m_LI;
  Function  *m_assumeFn;
  CallGraph *m_cg;
public:
  InstrumentLoadStmt(IRBuilder<> &IB, Function *assumeFn, CallGraph *cg)
    : m_IB(IB), m_LI(nullptr), m_assumeFn(assumeFn), m_cg(cg) {}
  
  bool Skip(const statement_t &s) {
    m_LI = getLoadInst(s);
    return !m_LI;
  }
  
  void Process(const statement_t &s, const clam_abstract_domain &inv) {
    if (!m_LI) return; 
    
    // Filter out all irrelevant constraints
    lin_cst_sys_t rel_csts;
    std::set<var_t> rel_vars;
    rel_vars.insert(s.get_live().defs_begin(), s.get_live().defs_end());
    for (auto cst : inv.to_linear_constraint_system()) {
      std::vector<var_t> v_intersect;
      std::set_intersection(cst.variables().begin(), cst.variables().end(),
                            rel_vars.begin(), rel_vars.end(),
                            std::back_inserter(v_intersect));
      if (!v_intersect.empty()) {
        rel_csts += cst;
      }
    }

    // Insert an assume instruction after the load instruction (m_LI)
    m_IB.SetInsertPoint(m_LI);
    llvm::BasicBlock *InsertBlk = m_IB.GetInsertBlock();
    llvm::BasicBlock::iterator InsertPt = m_IB.GetInsertPoint();
    InsertPt++; // this is ok because LoadInstr cannot be terminators.
    m_IB.SetInsertPoint(InsertBlk, InsertPt);
    NumInstrLoads++;
    CodeExpander g;    
    g.genCode(rel_csts, m_IB, m_IB.getContext(), m_assumeFn, m_cg, nullptr,
	      m_LI->getParent()->getParent(), "crab_");

    // reset internal state
    m_LI = nullptr;
  }
};
  
class ConstantReplaceStmt {
  CfgBuilder *m_clamCfgBuilder;
public:
  ConstantReplaceStmt(CfgBuilder *clamCfgBuilder)
    : m_clamCfgBuilder(clamCfgBuilder) {}
  
  bool Skip(const statement_t &s) {
    if (s.is_callsite() || s.is_intrinsic() ||
	s.is_assume() || s.is_assert() ||
	s.is_havoc() ||
	s.is_int_cast()) {
      return true;
    }
    if (s.get_live().num_defs() != 1) {
      return true;
    }
    return false;
  }
  
  void Process(const statement_t &s, const clam_abstract_domain &inv) {
    assert(s.get_live().num_defs() == 1);
    if (auto v = (*(s.get_live().defs_begin())).name().get()) {
      if (const Instruction *I = dyn_cast<const Instruction>(*v)) {
	if (Constant *C = getConstantInt(m_clamCfgBuilder, inv, *I)) {
	  const_cast<Instruction*>(I)->replaceAllUsesWith(C);
	}
      }
    }
  }
};
  
} // end namespace 

namespace clam {
  
// Instrument basic block entries with a sequence of assume instructions
static bool instrumentBlock(lin_cst_sys_t csts, llvm::BasicBlock *bb,
			    LLVMContext &ctx, CallGraph *cg, DominatorTree *DT,
			    Function *assumeFn) {

  // If the block is an exit we do not instrument it.
  const ReturnInst *ret = dyn_cast<const ReturnInst>(bb->getTerminator());
  if (ret)
    return false;

  IRBuilder<> Builder(ctx);
  Builder.SetInsertPoint(bb->getFirstNonPHI());
  CodeExpander g;
  NumInstrBlocks++;
  bool res = g.genCode(csts, Builder, ctx, assumeFn, cg, DT, bb->getParent(),
                        "crab_");
  return res;
}

// Instrument LoadInst with a sequence of assume instructions  
static bool instrumentLoadInst(clam_abstract_domain inv, basic_block_t &bb,
			       LLVMContext &ctx, CallGraph *cg,
			       Function *assumeFn) {
  IRBuilder<> Builder(ctx);
  InstrumentLoadStmt ILS(Builder, assumeFn, cg);
  return GenericInstrumentStatement(inv, bb, ILS);
}

// Do constant replacement  
static bool constantReplacement(CfgBuilder *clamCfgBuilder,
				clam_abstract_domain inv,
				basic_block_t &bb) {
  ConstantReplaceStmt CRS(clamCfgBuilder);
  return GenericInstrumentStatement(inv, bb, CRS);
}

// Identify whether B is dead and which B's successor edges are dead.
static bool markDeadBlocksAndEdges(ClamGlobalAnalysis  &clam,
				   BasicBlock &B,
				   std::vector<BasicBlock*> &deadBlocks,
				   std::vector<std::pair<BasicBlock*,BasicBlock*>> &deadEdges) {
  Function &F = *(B.getParent());
  // Mark dead successor edges
  for (BasicBlock *Succ : successors(&B)) {
    if (!clam.hasFeasibleEdge(&B, Succ)) {
      CRAB_LOG("clam-opt",
	       llvm::errs() << "clam-opt detected dead"
	       << " edge between block "
	       << B.getName () << " and " << Succ->getName()
	       << " in function " << F.getName() << "\n";);
      deadEdges.push_back({&B, Succ});
    }
  }

  // Mark whether the block is dead
  llvm::Optional<clam_abstract_domain> pre = 
    clam.getPre(&B, false /*do not keep ghost variables*/);    
  if (pre.hasValue()) {
    if (pre.getValue().is_bottom()) {
      CRAB_LOG("clam-opt",
	       llvm::errs() << "clam-opt detected dead block "
	       << B.getName () << " in function "
	       << F.getName() << "\n";);
      deadBlocks.push_back(&B);
      return true;
    }
  }
  return false;
}

// Remove a block  
static void removeDeadBlock(BasicBlock *BB, LLVMContext &ctx) {
  ++NumDeadBlocks;
  // Loop through all of our successors and make sure they know that one
  // of their predecessors is going away.
  for (BasicBlock *Succ : successors(BB)) {
    Succ->removePredecessor(BB);
  }
  // Zap all the instructions in the block.
  while (!BB->empty()) {
    Instruction &I = BB->back();
    // If this instruction is used, replace uses with an arbitrary value.
    // Because control flow can't get here, we don't care what we replace the
    // value with.  Note that since this block is unreachable, and all values
    // contained within it must dominate their uses, that all uses will
    // eventually be removed (they are themselves dead).
    if (!I.use_empty()) {
      I.replaceAllUsesWith(UndefValue::get(I.getType()));
    }
    BB->getInstList().pop_back();
  }
  // Add unreachable terminator
  BB->getInstList().push_back(new UnreachableInst(ctx));
}

// Remove an edge  
static void removeDeadEdge(BasicBlock *BB, BasicBlock *Succ) {
  ++NumDeadEdges;

  Instruction *TI = BB->getTerminator();
  if (BranchInst *BI = dyn_cast<BranchInst>(TI)) {
    if (BI->isConditional()) {
      // Replace conditional branch with an unconditional one.
      BasicBlock *OnlySucc = nullptr;
      for (unsigned i = 0, e = BI->getNumSuccessors(); i < e; ++i) {
        if (BI->getSuccessor(i) == Succ) {
          continue;
        } else if (!OnlySucc) {
          OnlySucc = BI->getSuccessor(i);
        } else {
          OnlySucc = nullptr;
        }
      }
      if (!OnlySucc)
        return;

      BranchInst *NewTI = BranchInst::Create(OnlySucc, TI);
      TI->replaceAllUsesWith(NewTI);
      TI->eraseFromParent();

      // Fix phi nodes of the successor.
      std::vector<std::pair<PHINode *, unsigned>> PHIValues;
      for (PHINode &PHI : Succ->phis()) {
        for (unsigned i = 0, e = PHI.getNumIncomingValues(); i < e; ++i) {
          if (PHI.getIncomingBlock(i) == BB) {
            PHIValues.push_back({&PHI, i});
          }
        }
      }

      for (auto &p : PHIValues) {
        PHINode *PHI = p.first;
        unsigned i = p.second;
        PHI->removeIncomingValue(i);
      }
    }
  }
}
   
Optimizer::Optimizer(ClamGlobalAnalysis  &clam,
		     llvm::CallGraph *callgraph,
		     std::function<llvm::DominatorTree*(llvm::Function*)> DT,
		     std::function<llvm::LoopInfo*(llvm::Function*)> LI,
		     InvariantsLocation addInvariants,
		     bool removeDeadCode,
		     bool replaceWithConstants)
  : m_clam(clam)
  , m_cg(callgraph)
  , m_dt(DT)
  , m_li(LI)
  , m_invLoc(addInvariants)
  , m_removeDeadCode(removeDeadCode)
  , m_replaceWithConstants(replaceWithConstants)
  , m_assumeFn(nullptr) {}

  
bool Optimizer::runOnModule(Module &M) {
  if (m_invLoc == InvariantsLocation::NONE &&
      !m_removeDeadCode &&
      !m_replaceWithConstants) {
    return false;
  }

  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
		  << "Starting clam optimizer based on invariants.\n";);
  
  LLVMContext &ctx = M.getContext();
  AttrBuilder B(ctx);
  B.addAttribute(Attribute::NoUnwind);
  B.addAttribute(Attribute::NoRecurse);
  B.addAttribute(Attribute::OptimizeNone);
  B.addAttribute(Attribute::NoInline);
  // LLVM removed all calls to verifier.assume if marked as ReadNone
  // or ReadOnly even if we mark it as OptimizeNone.
  B.addAttribute(Attribute::InaccessibleMemOnly);  
  AttributeList as = AttributeList::get(ctx, AttributeList::FunctionIndex, B);
  m_assumeFn = dyn_cast<Function>(M.getOrInsertFunction("verifier.assume", as,
                                                        Type::getVoidTy(ctx),
                                                        Type::getInt1Ty(ctx))
                                      .getCallee());
  if (m_cg) {
    m_cg->getOrInsertFunction(m_assumeFn);
  }
  
  bool change = false;
  for (auto &f : M) {
    change |= runOnFunction(f);
  }

  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
		  << "Finished clam optimizer based on invariants.\n";);    
  return change;
}

bool Optimizer::runOnFunction(Function &F) {
  if (F.empty() || F.isVarArg()) {
    return false;
  }

  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
		  << "Started clam optimizer for " << F.getName().str() << ".\n";);
  
  if (!m_clam.getCfgBuilderMan().hasCfg(F)) {
    // This shouldn't happen
    llvm::errs() << "Crab CFG not found for " << F.getName() << "\n";
    return false;
  }

  cfg_ref_t cfg = m_clam.getCfgBuilderMan().getCfg(F);
  DominatorTree *dt = m_dt(&F); // it can be nullptr
  LLVMContext &ctx = F.getContext();
  std::vector<BasicBlock *> DeadBlocks;
  std::vector<std::pair<BasicBlock *, BasicBlock *>> DeadEdges;
  bool change = false;  
  for (auto &B : F) {
    if (hasUnreachable(B)) {
      continue;
    }

    CRAB_LOG("clam-opt2",
	     llvm::errs() << "clam-opt processing " << B.getName() << "\n";);
    
    if (m_removeDeadCode &&
	markDeadBlocksAndEdges(m_clam, B, DeadBlocks, DeadEdges)) {
      // do not keep processing the block because it is dead
      continue;
    }
    
    if (m_invLoc == InvariantsLocation::BLOCK ||
	m_invLoc == InvariantsLocation::LOOP_HEADER || 
	m_invLoc == InvariantsLocation::ALL) {
      const bool keep_ghost = false;
      llvm::Optional<clam_abstract_domain> pre = m_clam.getPre(&B, keep_ghost);
      if (pre.hasValue()) {
	if (m_invLoc == InvariantsLocation::BLOCK ||
	    m_invLoc == InvariantsLocation::ALL) {
	  auto csts = pre.getValue().to_linear_constraint_system();
	  change |= instrumentBlock(csts, &B, F.getContext(), m_cg, dt, m_assumeFn);
	} else {
	  assert(m_invLoc == InvariantsLocation::LOOP_HEADER);
	  LoopInfo *LI = m_li(&F); // it can be nullptr
	  if (LI && LI->isLoopHeader(&B)) {
	    auto csts = pre.getValue().to_linear_constraint_system();
	    change |= instrumentBlock(csts, &B, F.getContext(), m_cg, dt, m_assumeFn);
	  }
	}
      }
    }
    
    if ((m_invLoc == InvariantsLocation::LOAD_INST ||
	 m_invLoc == InvariantsLocation::ALL) && readMemory(B)) {
      const bool keep_ghost = true;
      llvm::Optional<clam_abstract_domain> pre = m_clam.getPre(&B, keep_ghost);
      if (pre.hasValue()) {
	auto cfg_builder_ptr = m_clam.getCfgBuilderMan().getCfgBuilder(F);
	assert(cfg_builder_ptr);
	basic_block_label_t bb_label = cfg_builder_ptr->getCrabBasicBlock(&B);
	change |= instrumentLoadInst(pre.getValue(), cfg.get_node(bb_label),
				     F.getContext(), m_cg, m_assumeFn);
      }
    }

    if (m_replaceWithConstants) {
      const bool keep_ghost = false;
      llvm::Optional<clam_abstract_domain> pre = m_clam.getPre(&B, keep_ghost);
      if (pre.hasValue()) {
	auto cfg_builder_ptr = m_clam.getCfgBuilderMan().getCfgBuilder(F);
	assert(cfg_builder_ptr);	
	basic_block_label_t bb_label = cfg_builder_ptr->getCrabBasicBlock(&B);
	change |= constantReplacement(cfg_builder_ptr, pre.getValue(), cfg.get_node(bb_label));
      }
    }
  }

  change = (!DeadEdges.empty() || !DeadBlocks.empty());
  
  // The actual removal of edges and blocks
  while (!DeadEdges.empty()) {
    std::pair<BasicBlock *, BasicBlock *> E = DeadEdges.back();
    DeadEdges.pop_back();
    removeDeadEdge(E.first, E.second);
  }

  
  while (!DeadBlocks.empty()) {
    BasicBlock *B = DeadBlocks.back();
    DeadBlocks.pop_back();
    removeDeadBlock(B, ctx);
  }

  CRAB_VERBOSE_IF(1, crab::get_msg_stream()
		  << "Finished clam optimizer for " << F.getName().str() << ".\n";);
  return change;
}
  
/* Pass code starts here */
  
OptimizerPass::OptimizerPass(ClamGlobalAnalysis *clam):
  ModulePass(ID), m_impl(nullptr), m_clam(clam) {}
  
bool OptimizerPass::runOnModule(Module &M) {
  if (InvLoc == InvariantsLocation::NONE &&
      !RemoveDeadCode &&
      !ReplaceWithConstants) {
    return false;
  }
  
  // Get module's callgraph
  CallGraph *cg = nullptr;
  if (CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass>()) {
    cg = &cgwp->getCallGraph();
  }

  // Get clam
  //ClamPass &clam = getAnalysis<ClamPass>();
  if (!m_clam) {
    m_clam = &(getAnalysis<ClamPass>().getClamGlobalAnalysis());
  }
  
  // Collect all the dominator tree and loop info in maps
  DenseMap<Function*, DominatorTree*> dt_map;
  DenseMap<Function*, LoopInfo*> li_map;
  for (auto &F: M) {
    if (F.empty()) continue;
    if (requireDominatorTree(InvLoc))
      dt_map[&F] = &(getAnalysis<DominatorTreeWrapperPass>(F).getDomTree());
    if (requireLoopInfo(InvLoc))    
      li_map[&F] = &(getAnalysis<LoopInfoWrapperPass>(F).getLoopInfo());
  }

  m_impl.reset(new Optimizer(*m_clam, cg,
			     [&dt_map](Function *F) {
			       auto it = dt_map.find(F);
			       return (it != dt_map.end() ? it->second : nullptr);
			     },
			     [&li_map](Function *F) {
			       auto it = li_map.find(F);
			       return (it != li_map.end() ? it->second : nullptr);
			     },
			     InvLoc, RemoveDeadCode, ReplaceWithConstants));
  return m_impl->runOnModule(M);
}

void OptimizerPass::getAnalysisUsage(AnalysisUsage &AU) const {
  if (!m_clam) {
    AU.addRequired<clam::ClamPass>();
  }
  AU.addRequired<UnifyFunctionExitNodesLegacyPass>();
  AU.addRequired<CallGraphWrapperPass>();
  AU.addPreserved<CallGraphWrapperPass>();
  if (requireDominatorTree(InvLoc)) {
    AU.addRequired<DominatorTreeWrapperPass>();
  }
  if (requireLoopInfo(InvLoc)) {
    AU.addRequired<LoopInfoWrapperPass>();
  }
}

char clam::OptimizerPass::ID = 0;
llvm::Pass *createOptimizerPass(ClamGlobalAnalysis *clam) {
  return new OptimizerPass(clam);
}
 
} // namespace clam

static RegisterPass<clam::OptimizerPass>
X("crab-opt",
  "Optimize LLVM bitcode using invariants inferred by crab",
  false, false);
