#pragma once

/*
 * Use Crab invariants to remove dead code and insert the invariants
 * in the LLVM bitcode via verifier.assume instructions.
 *
 * The inserted invariants consist of a sequence of LLVM comparison
 * instructions (one per linear constraint) followed by a
 * verifier.assume instruction.
 */

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
    DEAD_CODE, /* remove dead blocks */
    BLOCK,     /* dead code + insert invariants at block entries */
    LOOP_HEADER, /* dead code + insert invariants at loop headers */
    LOAD_INST, /* dead code + insert invariants after LoadInst */
    ALL        /* dead code + insert invariants after each instruction*/
};

class InsertInvariants {
  clam::ClamGlobalAnalysis &m_clam;
  llvm::CallGraph *m_cg;
  std::function<llvm::DominatorTree*(llvm::Function*)> m_dt;
  std::function<llvm::LoopInfo*(llvm::Function*)> m_li;
  InvariantsLocation m_loc;
  llvm::Function *m_assumeFn;
  bool runOnFunction(llvm::Function &F);
public:
  InsertInvariants(clam::ClamGlobalAnalysis  &clam,
		   llvm::CallGraph *callgraph,
		   std::function<llvm::DominatorTree*(llvm::Function*)> DT,
		   std::function<llvm::LoopInfo*(llvm::Function*)> LI,
		   InvariantsLocation loc);
  bool runOnModule(llvm::Module &M);
};
  
class InsertInvariantsPass : public llvm::ModulePass {
  std::unique_ptr<InsertInvariants> m_impl;
public:
  static char ID;
  InsertInvariantsPass();
  virtual bool runOnModule(llvm::Module &M) override ;
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  virtual llvm::StringRef getPassName() const override{
    return "Clam: insert invariants as verifier.assume instructions";
  }
};
} // end namespace clam
