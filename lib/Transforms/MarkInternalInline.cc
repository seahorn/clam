#include "Transforms/MarkInternalInline.hh"

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"

using namespace llvm;

namespace crab_llvm {
  char MarkInternalInline::ID = 0;  
}
