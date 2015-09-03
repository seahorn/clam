#include "Transforms/RemoveUnreachableBlocksPass.hh"
#include "llvm/Transforms/Utils/Local.h"

using namespace llvm;

namespace crab_llvm
{
  char RemoveUnreachableBlocksPass::ID = 0;
  
  bool RemoveUnreachableBlocksPass::runOnFunction (Function &F)
  {return removeUnreachableBlocks (F);}
  
  void RemoveUnreachableBlocksPass::getAnalysisUsage (AnalysisUsage &AU) const {}
}
