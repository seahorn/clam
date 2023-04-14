#pragma once

/// Extra support for llvm CFG

#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/CFG.h"

namespace clam {
inline llvm::iterator_range<llvm::succ_iterator> succs(llvm::BasicBlock &bb) {
  return llvm::make_range(succ_begin(&bb), succ_end(&bb));
}

inline llvm::iterator_range<llvm::const_succ_iterator>
succs(const llvm::BasicBlock &bb) {
  return llvm::make_range(succ_begin(&bb), succ_end(&bb));
}

inline llvm::iterator_range<llvm::pred_iterator> preds(llvm::BasicBlock &bb) {
  return llvm::make_range(pred_begin(&bb), pred_end(&bb));
}

inline llvm::iterator_range<llvm::const_pred_iterator>
preds(const llvm::BasicBlock &bb) {
  return llvm::make_range(pred_begin(&bb), pred_end(&bb));
}
} // namespace clam
