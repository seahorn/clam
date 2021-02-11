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

#include "crab/config.h"
#include "crab/support/debug.hpp"

#include <type_traits>

using namespace llvm;

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

namespace clam {
  
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

  Value *mkBinOp(bin_op_t Op, IRBuilder<> B, Value *LHS, Value *RHS,
		 const Twine &Name) {
    assert(LHS->getType()->isIntegerTy() && RHS->getType()->isIntegerTy());
    switch (Op) {
    case ADD:
      return B.CreateAdd(LHS, RHS, Name);
    case SUB:
      return B.CreateSub(LHS, RHS, Name);
    case MUL:
      return B.CreateMul(LHS, RHS, Name);
    default:;
    }
  }

  Value *mkNum(number_t n, IntegerType *ty, LLVMContext &ctx) {
    return ConstantInt::get(ty, n.get_str(), 10);
  }

  Value *mkVar(varname_t v) {
    return (v.get() ? const_cast<Value *>(*(v.get())) : nullptr);
  }

  Value *mkBool(LLVMContext &ctx, bool val) {
    return ConstantInt::get(Type::getInt1Ty(ctx), (val) ? 1U : 0U);
  }

  IntegerType *getIntType(varname_t var) {
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
  
  // post: return a value of bool type(Int1Ty) that contains the
  // computation of cst
  Value *genCode(lin_cst_t cst, IRBuilder<> B, LLVMContext &ctx,
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
      if (n == 0)
        continue;
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
    }

    number_t c = -cst.expression().constant();
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
  bool genCode(lin_cst_sys_t csts, IRBuilder<> B, LLVMContext &ctx,
	       Function *assumeFn, CallGraph *cg, DominatorTree *DT,
	       const Function *insertFun, const Twine &Name = "") {
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

//! Instrument basic block entries.
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

// Instrument all load instructions in a basic block.
//
// The instrumentation is a bit involved because Crab gives us
// invariants that hold either at the entry or at the exit of a
// basic block but not at each program point. Thus, we need to take
// the invariants that hold at the entry and propagate (rebuild)
// them locally across the statements of the basic block. This will
// redo some work but it's more efficient than storing all
// invariants at each program point.
static bool instrumentLoadInst(clam_abstract_domain inv, basic_block_t &bb,
			       LLVMContext &ctx, CallGraph *cg,
			       Function *assumeFn) {
  // -- Forward propagation of inv through the basic block but
  //    ignoring callsites. This might be imprecise if the analysis
  //    was inter-procedural because we cannot reconstruct all the
  //    context that the inter-procedural analysis had during the
  //    analysis.
  using abs_tr_t = crab::analyzer::intra_abs_transformer<basic_block_t,
                                                         clam_abstract_domain>;

  // -- Crab memory load statements
  using array_load_t =
      array_load_stmt<basic_block_label_t, number_t, varname_t>;
  using load_from_ref_t =
      load_from_ref_stmt<basic_block_label_t, number_t, varname_t>;
  using load_from_arr_ref_t =
      load_from_arr_ref_stmt<basic_block_label_t, number_t, varname_t>;

  IRBuilder<> Builder(ctx);
  bool change = false;
  abs_tr_t vis(inv);

  for (auto &s : bb) {
    s.accept(&vis); // propagate the invariant one statement forward
    const LoadInst *I = nullptr;
    std::set<var_t> load_vs;
    if (s.is_arr_read()) {
      const array_load_t *load_stmt = static_cast<const array_load_t *>(&s);
      if (auto v = load_stmt->lhs().name().get()) {
        I = dyn_cast<const LoadInst>(*v);
        load_vs.insert(load_stmt->lhs());
      }
    } else if (s.is_ref_load()) {
      const load_from_ref_t *load_stmt =
          static_cast<const load_from_ref_t *>(&s);
      if (auto v = load_stmt->lhs().name().get()) {
        I = dyn_cast<const LoadInst>(*v);
        load_vs.insert(load_stmt->lhs());
      }
    } else if (s.is_ref_arr_load()) {
      const load_from_arr_ref_t *load_stmt =
          static_cast<const load_from_arr_ref_t *>(&s);
      if (auto v = load_stmt->lhs().name().get()) {
        I = dyn_cast<const LoadInst>(*v);
        load_vs.insert(load_stmt->lhs());
      }
    }

    if (!I)
      continue;

    const clam_abstract_domain &next_inv = vis.get_abs_value();
    if (next_inv.is_top())
      continue;
    // -- Filter out all constraints that do not use x.
    lin_cst_sys_t rel_csts;
    for (auto cst : next_inv.to_linear_constraint_system()) {
      std::vector<var_t> v_intersect;
      std::set_intersection(cst.variables().begin(), cst.variables().end(),
                            load_vs.begin(), load_vs.end(),
                            std::back_inserter(v_intersect));
      if (!v_intersect.empty()) {
        rel_csts += cst;
      }
    }

    // -- Insert assume's the next after I
    Builder.SetInsertPoint(const_cast<LoadInst *>(I));
    llvm::BasicBlock *InsertBlk = Builder.GetInsertBlock();
    llvm::BasicBlock::iterator InsertPt = Builder.GetInsertPoint();
    InsertPt++; // this is ok because LoadInstr cannot be terminators.
    Builder.SetInsertPoint(InsertBlk, InsertPt);
    CodeExpander g;
    NumInstrLoads++;
    change |= g.genCode(rel_csts, Builder, ctx, assumeFn, cg, nullptr,
                         I->getParent()->getParent(), "crab_");
  }
  return change;
}

static Constant *getConstantInt(CfgBuilderPtr clamCfgBuilder, clam_abstract_domain inv, Value &v) {
  if (v.getType()->isIntegerTy()) {
    llvm::Optional<var_t> lhs = clamCfgBuilder->getCrabVariable(&v);
    if (lhs.hasValue()) {
      auto interval = inv[lhs.getValue()];
      if (boost::optional<number_t> constant_opt = interval.singleton()) {
	if ((*constant_opt).fits_int64()) {
	  return ConstantInt::get(v.getType(), (int64_t)(*constant_opt), 10);	      
	}
      }
    }
  }
  return nullptr;
}
  
static void constantReplacement(ClamGlobalAnalysis &clam, clam_abstract_domain inv, BasicBlock &B) {
  CfgBuilderPtr clamCfgBuilder = clam.getCfgBuilderMan().getCfgBuilder(*B.getParent());
  for (auto &I: B) {
    if (LoadInst *LI = dyn_cast<LoadInst>(&I)) {
      if (Constant *C = getConstantInt(clamCfgBuilder, inv, *LI)) {
	LI->replaceAllUsesWith(C);
      }
    } 
  }
}
  
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

// Identify whether B is dead and which B's successor edges are dead.
static void markDeadBlocksAndEdges(ClamGlobalAnalysis  &clam,
				   BasicBlock &B,
				   std::vector<BasicBlock*> &deadBlocks,
				   std::vector<std::pair<BasicBlock*,BasicBlock*>> &deadEdges) {
  Function &F = *(B.getParent());
  // Mark dead successor edges
  for (BasicBlock *Succ : successors(&B)) {
    if (!clam.hasFeasibleEdge(&B, Succ)) {
      CRAB_LOG("clam-opt",
	       llvm::errs() << "Detected dead "
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
	       llvm::errs() << "Detected dead block "
	       << B.getName () << " in function "
	       << F.getName() << "\n";);
      deadBlocks.push_back(&B);
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
  AttrBuilder B;
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

    if (m_removeDeadCode) {
      markDeadBlocksAndEdges(m_clam, B, DeadBlocks, DeadEdges);
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
	basic_block_label_t bb_label = cfg_builder_ptr->getCrabBasicBlock(&B);
	change |= instrumentLoadInst(pre.getValue(), cfg.get_node(bb_label),
				     F.getContext(), m_cg, m_assumeFn);
      }
    }

    if (m_replaceWithConstants) {
      const bool keep_ghost = false;
      llvm::Optional<clam_abstract_domain> post = m_clam.getPost(&B, keep_ghost);
      if (post.hasValue()) {
	constantReplacement(m_clam, post.getValue(), B);
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
		  << "Finished dce and other optimizations using invariants for "
		  << F.getName().str() << ".\n";);
  
  return change;
}
  
/* Pass code starts here */
  
OptimizerPass::OptimizerPass():
  ModulePass(ID), m_impl(nullptr) {}
  
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
  ClamPass &clam = getAnalysis<ClamPass>();

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

  m_impl.reset(new Optimizer(clam.getClamGlobalAnalysis(), cg,
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
  AU.addRequired<clam::ClamPass>();
  AU.addRequired<UnifyFunctionExitNodes>();
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
llvm::Pass *createOptimizerPass() {
  return new OptimizerPass();
}
 
} // namespace clam

static RegisterPass<clam::OptimizerPass>
X("crab-opt",
  "Optimize LLVM bitcode using invariants inferred by crab",
  false, false);
