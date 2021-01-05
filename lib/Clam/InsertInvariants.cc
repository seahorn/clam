#include "llvm/ADT/Statistic.h"
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

#include "clam/CfgBuilder.hh"
#include "clam/Clam.hh"
#include "clam/Transforms/InsertInvariants.hh"
#include "clam/config.h"
#include "crab/analysis/abs_transformer.hpp"

/*
 * Instrument LLVM bitcode by inserting invariants computed by Crab.
 *
 * If the invariant at a given block is false then this pass removes
 * directly the unreachable block. Otherwise, non-false invariants are
 * inserted as special verifier.assume instructions.
 *
 */

using namespace llvm;
using namespace clam;
using namespace crab::cfg;

enum InvariantsLocation { NONE, DEAD_CODE, PER_BLOCK, PER_LOOP, PER_LOAD, ALL };
static cl::opt<InvariantsLocation> InvLoc(
    "crab-add-invariants",
    cl::desc("Instrument code with invariants at specific location"),
    cl::values(
        clEnumValN(NONE, "none", "This pass is a non-op"),
        clEnumValN(DEAD_CODE, "dead-code",
                   "Perform dead code elimination (DCE) by removing "
                   "unreachable blocks and edges"),
        clEnumValN(PER_BLOCK, "block-entry",
                   "DCE + add invariants at the entry of each basic block"),
        clEnumValN(PER_LOOP, "loop-header",
                   "DCE + add invariants only at loop headers"),
        clEnumValN(PER_LOAD, "after-load",
                   "DCE + add invariants after each load instruction"),
        clEnumValN(ALL, "all",
                   "DCE + add invariants at all locations (very verbose)")),
    cl::init(NONE));

#define DEBUG_TYPE "crab-insert-invars"

STATISTIC(NumDeadBlocks, "Number of dead blocks");
STATISTIC(NumDeadEdges, "Number of dead edges");
STATISTIC(NumInstrBlocks, "Number of blocks instrumented with invariants");
STATISTIC(NumInstrLoads, "Number of load inst instrumented with invariants");

namespace clam {

char clam::InsertInvariants::ID = 0;

// helper
inline bool reads_memory(const llvm::BasicBlock &B) {
  for (auto &I : B) {
    if (isa<LoadInst>(&I)) {
      return true;
    }
  }
  return false;
}

struct CodeExpander {
  enum bin_op_t { ADD, SUB, MUL };

  Value *mk_bin_op(bin_op_t Op, IRBuilder<> B, Value *LHS, Value *RHS,
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
      ;
    }
  }

  Value *mk_num(number_t n, IntegerType *ty, LLVMContext &ctx) {
    return ConstantInt::get(ty, n.get_str(), 10);
  }

  IntegerType *get_int_type(varname_t var) {
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

  Value *mk_var(varname_t v) {
    if (!v.get()) {
      return nullptr;
    } else {
      return const_cast<Value *>(*(v.get()));
    }
  }

  Value *mk_bool(LLVMContext &ctx, bool val) {
    return ConstantInt::get(Type::getInt1Ty(ctx), (val) ? 1U : 0U);
  }

  //! Generate llvm bitcode from a set of linear constraints
  //  TODO: generate bitcode from disjunctive linear constraints.
  bool gen_code(lin_cst_sys_t csts, IRBuilder<> B, LLVMContext &ctx,
                Function *assumeFn, CallGraph *cg, DominatorTree *DT,
                const Function *insertFun, const Twine &Name = "") {
    bool change = false;
    for (auto cst : csts) {
      if (Value *cst_code = gen_code(cst, B, ctx, DT, Name)) {
        CallInst *ci = B.CreateCall(assumeFn, cst_code);
        // errs() << "Added " << *ci << " with " << *cst_code << "\n";
        change = true;
        if (cg) {
          (*cg)[insertFun]->addCalledFunction(ci,
                                              (*cg)[ci->getCalledFunction()]);
        }
      }
    }
    return change;
  }

  // post: return a value of bool type(Int1Ty) that contains the
  // computation of cst
  Value *gen_code(lin_cst_t cst, IRBuilder<> B, LLVMContext &ctx,
                  DominatorTree *DT, const Twine &Name) {
    if (cst.is_tautology()) {
      return nullptr; // mk_bool(ctx, true);
    }

    if (cst.is_contradiction()) {
      return mk_bool(ctx, false);
    }

    // translate only expressions of LLVM integer type
    IntegerType *ty = nullptr;
    for (auto v : cst.variables()) {
      if (!ty) {
        ty = get_int_type(v.name());
      } else {
        if (ty != get_int_type(v.name())) {
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
      if (Value *vv = mk_var(v)) {
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
    Value *ee = mk_num(number_t("0"), ty, ctx);
    for (auto t : e) {
      number_t n = t.first;
      if (n == 0)
        continue;
      varname_t v = t.second.name();
      Value *vv = mk_var(v);
      assert(vv);
      assert(vv->getType()->isIntegerTy());
      if (n == 1) {
        ee = mk_bin_op(ADD, B, ee, vv, Name);
      } else if (n == -1) {
        ee = mk_bin_op(SUB, B, ee, vv, Name);
      } else {
        ee = mk_bin_op(ADD, B, ee,
                       mk_bin_op(MUL, B, mk_num(n, ty, ctx), vv, Name), Name);
      }
    }

    number_t c = -cst.expression().constant();
    Value *cc = mk_num(c, ty, ctx);
    if (cst.is_inequality()) {
      return B.CreateICmpSLE(ee, cc, Name);
    } else if (cst.is_equality()) {
      return B.CreateICmpEQ(ee, cc, Name);
    } else {
      return B.CreateICmpNE(ee, cc, Name);
    }
  }
};

//! Instrument basic block entries.
static bool instrument_block(lin_cst_sys_t csts, llvm::BasicBlock *bb,
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
  bool res = g.gen_code(csts, Builder, ctx, assumeFn, cg, DT, bb->getParent(),
                        "crab_");
  return res;
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
static bool instrument_loads(clam_abstract_domain inv, basic_block_t &bb,
                             LLVMContext &ctx, CallGraph *cg,
                             Function *assumeFn) {
  // -- Forward propagation of inv through the basic block but ignoring
  // callsites
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
    change |= g.gen_code(rel_csts, Builder, ctx, assumeFn, cg, nullptr,
                         I->getParent()->getParent(), "crab_");
  }
  return change;
}

static void removeUnreachableBlock(BasicBlock *BB, LLVMContext &ctx) {
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

static void removeInfeasibleEdge(BasicBlock *BB, BasicBlock *Succ) {
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

bool InsertInvariants::runOnModule(Module &M) {
  if (InvLoc == NONE)
    return false;

  LLVMContext &ctx = M.getContext();
  AttrBuilder B;
  AttributeList as = AttributeList::get(ctx, AttributeList::FunctionIndex, B);
  m_assumeFn = dyn_cast<Function>(M.getOrInsertFunction("verifier.assume", as,
                                                        Type::getVoidTy(ctx),
                                                        Type::getInt1Ty(ctx))
                                      .getCallee());

  CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass>();
  if (CallGraph *cg = cgwp ? &cgwp->getCallGraph() : nullptr)
    cg->getOrInsertFunction(m_assumeFn);

  bool change = false;
  for (auto &f : M) {
    change |= runOnFunction(f);
  }

  return change;
}

bool InsertInvariants::runOnFunction(Function &F) {
  if (InvLoc == NONE)
    return false;

  if (F.isDeclaration() || F.empty() || F.isVarArg())
    return false;

  ClamPass *crab = &getAnalysis<ClamPass>();

  if (!crab->hasCfg(F))
    return false;

  bool change = false;

  cfg_ref_t cfg = crab->getCfg(F);
  CallGraphWrapperPass *cgwp = getAnalysisIfAvailable<CallGraphWrapperPass>();
  CallGraph *cg = cgwp ? &cgwp->getCallGraph() : nullptr;
  DominatorTree *DT =
      ((InvLoc == PER_BLOCK || InvLoc == PER_LOOP || InvLoc == ALL)
           ? &(getAnalysis<DominatorTreeWrapperPass>(F).getDomTree())
           : nullptr);
  LLVMContext &ctx = F.getContext();
  std::vector<BasicBlock *> UnreachableBlocks;
  std::vector<std::pair<BasicBlock *, BasicBlock *>> InfeasibleEdges;
  for (auto &B : F) {

    // -- if the block has an unreachable instruction we skip it.
    bool alread_dead_block = false;
    for (auto &I : B) {
      if (isa<UnreachableInst>(I)) {
        alread_dead_block = true;
        break;
      }
    }
    if (alread_dead_block)
      continue;

    auto cfg_builder_ptr = crab->getCfgBuilderMan().getCfgBuilder(F);

    llvm::Optional<clam_abstract_domain> pre =
        crab->getPre(&B, false /*keep shadows*/);
    if (pre.hasValue()) {
      ///////
      /// First, we do dead code elimination.
      ///////

      if (pre.getValue().is_bottom()) {
        UnreachableBlocks.push_back(&B);
        continue;
      } else {
        for (BasicBlock *Succ : successors(&B)) {
          if (!crab->hasFeasibleEdge(&B, Succ)) {
            InfeasibleEdges.push_back({&B, Succ});
          }
        }
      }

      if (InvLoc == DEAD_CODE) {
        continue;
      }

      ///////
      /// Second, we insert the non-bottom invariants.
      //////

      // --- Instrument basic block with invariants
      if (InvLoc == PER_BLOCK || InvLoc == ALL) {
        auto csts = pre.getValue().to_linear_constraint_system();
        change |=
            instrument_block(csts, &B, F.getContext(), cg, DT, m_assumeFn);
      } else if (InvLoc == PER_LOOP) {
        LoopInfo &LI = getAnalysis<LoopInfoWrapperPass>(F).getLoopInfo();
        if (LI.isLoopHeader(&B)) {
          auto csts = pre.getValue().to_linear_constraint_system();
          change |=
              instrument_block(csts, &B, F.getContext(), cg, DT, m_assumeFn);
        }
      }
    }

    if (InvLoc == PER_LOAD || InvLoc == ALL) {
      // --- Instrument Load instructions
      if (reads_memory(B)) {
        llvm::Optional<clam_abstract_domain> pre =
            crab->getPre(&B, true /*keep shadows*/);
        if (!pre.hasValue())
          continue;

        basic_block_label_t bb_label = cfg_builder_ptr->getCrabBasicBlock(&B);

        change |= instrument_loads(pre.getValue(), cfg.get_node(bb_label),
                                   F.getContext(), cg, m_assumeFn);
      }
    }
  }

  while (!InfeasibleEdges.empty()) {
    std::pair<BasicBlock *, BasicBlock *> E = InfeasibleEdges.back();
    InfeasibleEdges.pop_back();
    removeInfeasibleEdge(E.first, E.second);
  }

  while (!UnreachableBlocks.empty()) {
    BasicBlock *B = UnreachableBlocks.back();
    UnreachableBlocks.pop_back();
    removeUnreachableBlock(B, ctx);
  }

  return change;
}

void InsertInvariants::getAnalysisUsage(AnalysisUsage &AU) const {
  // AU.setPreservesAll();
  AU.addRequired<clam::ClamPass>();
  AU.addRequired<UnifyFunctionExitNodes>();
  AU.addRequired<CallGraphWrapperPass>();
  AU.addPreserved<CallGraphWrapperPass>();
  if (InvLoc == PER_BLOCK || InvLoc == PER_LOOP || InvLoc == ALL) {
    AU.addRequired<DominatorTreeWrapperPass>();
  }
  if (InvLoc == PER_LOOP) {
    AU.addRequired<LoopInfoWrapperPass>();
  }
}

} // namespace clam

static RegisterPass<clam::InsertInvariants>
    X("insert-crab-invs", "Instrument bitcode with invariants inferred by crab",
      false, false);
