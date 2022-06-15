#pragma once

/* Optimize LLVM bitcode using invariants inferred by Clam. */

#include "llvm/Pass.h"
#include <memory>

namespace llvm {
class Function;
class Module;
class CallGraph;
class DominatorTree;
class LoopInfo;
} // namespace llvm

namespace clam {
class ClamGlobalAnalysis;
} // namespace clam

namespace clam {
enum class InvariantsLocation {
    NONE,
    BLOCK,       /* insert invariants at block entries */
    LOOP_HEADER, /* insert invariants at loop headers */
    LOAD_INST,   /* insert invariants after LoadInst */
    ALL          /* insert invariants after each instruction*/
};

class Optimizer {
  clam::ClamGlobalAnalysis &m_clam;
  llvm::CallGraph *m_cg;
  std::function<llvm::DominatorTree*(llvm::Function*)> m_dt;
  std::function<llvm::LoopInfo*(llvm::Function*)> m_li;
  InvariantsLocation m_invLoc;
  bool m_removeDeadCode;
  bool m_replaceWithConstants;
  llvm::Function *m_assumeFn;
  bool runOnFunction(llvm::Function &F);
public:

  /** 
   *  Optimize bitcode by using the invariants produced by Clam.
   *
   *  - If addInvariants != InvariantsLocation::NONE then add
   *    invariants as a sequence of LLVM comparison instructions (one
   *    per linear constraint) followed by a verifier.assume
   *    instruction. (Clam can promote verifier.assume to llvm.assume)
   *
   *  - If removeDeadCode then it removes dead blocks and edges.
   *
   *  - If replaceWithConstants then it replaces certain values (e.g.,
   *    left-hand-side of LoadInst) with constants.
  */
  Optimizer(clam::ClamGlobalAnalysis  &clam,
	    llvm::CallGraph *callgraph,
	    std::function<llvm::DominatorTree*(llvm::Function*)> DT,
	    std::function<llvm::LoopInfo*(llvm::Function*)> LI,
	    InvariantsLocation addInvariants,
	    bool removeDeadCode,
	    bool replaceWithConstants);
  bool runOnModule(llvm::Module &M);
};
  
class OptimizerPass : public llvm::ModulePass {
  std::unique_ptr<Optimizer> m_impl;
  clam::ClamGlobalAnalysis *m_clam;
public:
  static char ID;
  OptimizerPass(ClamGlobalAnalysis *clam = nullptr);
  virtual bool runOnModule(llvm::Module &M) override ;
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  virtual llvm::StringRef getPassName() const override{
    return "Clam: use invariants to optimize LLVM bitcode";
  }
};
} // end namespace clam
