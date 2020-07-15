#include "CfgBuilderShadowMem.hh"
#include "SeaDsaHeapAbstractionDsaToRegion.hh"

#include "clam/Support/Debug.hh"
#include "llvm/ADT/Optional.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "seadsa/Graph.hh"
#include "seadsa/ShadowMem.hh"

namespace clam {

using namespace llvm;

const llvm::StringRef memTag = "shadow.mem";
const llvm::StringRef memDefTag = "shadow.mem.def";
const llvm::StringRef memUseTag = "shadow.mem.use";
const llvm::StringRef memPhiTag = "shadow.mem.phi";

// Helper to return the shadow Value *defined* and *used* by a StoreInst
std::pair<Value *, Value *> getShadowMemDefAndUse(StoreInst &I,
                                                  const seadsa::ShadowMem &sm) {
  auto it = I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  seadsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == seadsa::ShadowMemInstOp::STORE);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  return sm.getShadowMemVars(ci);
}

// Helper to return the shadow Value *used* by a LoadInst
Value &getShadowMemUse(LoadInst &I, const seadsa::ShadowMem &sm) {
  auto it = I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  seadsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == seadsa::ShadowMemInstOp::LOAD);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  Value *useV = sm.getShadowMemVars(ci).second;
  assert(useV);
  return *useV;
}

// Same definition than ShadowMem.cc
static Value *getUniqueScalar(const seadsa::Cell &c) {
  const seadsa::Node *n = c.getNode();
  if (n && c.getOffset() == 0) {
    Value *v = const_cast<llvm::Value *>(n->getUniqueScalar());
    // -- a unique scalar is a single-cell global variable. We might be
    // -- able to extend this to single-cell local pointers, but these
    // -- are probably not very common.
    if (auto *gv = llvm::dyn_cast_or_null<llvm::GlobalVariable>(v))
      if (gv->getType()->getElementType()->isSingleValueType())
        return v;
  }
  return nullptr;
}

const Value *getShadowMemUniqueScalar(StoreInst &I,
                                      const seadsa::ShadowMem &sm) {
  auto it = I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  seadsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == seadsa::ShadowMemInstOp::STORE);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  return getUniqueScalar(cell);
}

const Value *getShadowMemUniqueScalar(LoadInst &I,
                                      const seadsa::ShadowMem &sm) {
  auto it = I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  seadsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == seadsa::ShadowMemInstOp::LOAD);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  return getUniqueScalar(cell);
}

// We cannot identify a shadow phi node by looking at the metadata.
// We start from an incoming value and look at its users, stopping at
// the first definition.
llvm::Optional<seadsa::Cell> getShadowMemCell(const llvm::PHINode &phi,
                                              const llvm::Value &incVal,
                                              const seadsa::ShadowMem &sm) {
  DenseSet<const Value *> visited;
  SmallVector<const Value *, 8> worklist = {&incVal};
  while (!worklist.empty()) {
    const Value *current = worklist.pop_back_val();
    assert(current);
    if (visited.count(current) > 0)
      continue;

    visited.insert(current);

    if (const CallInst *CI = dyn_cast<CallInst>(current)) {
      if (const MDNode *meta = CI->getMetadata(memDefTag)) {
        auto cellOpt = sm.getShadowMemCell(*CI);
        if (cellOpt.hasValue()) {
          return cellOpt.getValue();
        }
        return llvm::None;
      }
    }

    if (auto *phi = dyn_cast<PHINode>(current)) {
      for (const Value *v : llvm::reverse(phi->incoming_values()))
        worklist.push_back(v);
      continue;
    }
  }
  return llvm::None;
}

Region getShadowRegion(const seadsa::Cell &c, const llvm::DataLayout &dl,
                       const seadsa::ShadowMem &sm) {

  auto cellIdOpt = sm.getCellId(c);
  if (cellIdOpt.hasValue()) {
    auto cellId = cellIdOpt.getValue();
    auto ri = DsaToRegion(c, dl,
                          /* these should be user flags */
                          false, false, false);
    if (ri.getType() != UNTYPED_REGION) {
      return Region(cellId, ri, getUniqueScalar(c));
    }
  }
  return Region();
}

Region getShadowRegion(llvm::CallInst &shadowInst, const llvm::DataLayout &dl,
                       const seadsa::ShadowMem &sm) {

  auto cellOpt = sm.getShadowMemCell(shadowInst);
  if (cellOpt.hasValue()) {
    seadsa::Cell c = cellOpt.getValue();
    return getShadowRegion(c, dl, sm);
  }
  return Region();
}

// Find shadow mem instruction from a verifier.int_initializer or
// verifier.zero_initializer call.
CallInst *getShadowCIFromGvInitializer(const seadsa::ShadowMem &sm,
                                       llvm::Instruction &gvInitInst,
                                       llvm::Value &v) {

  auto main = gvInitInst.getParent()->getParent();
  if (!main->getName().equals("main")) {
    return nullptr;
  }
  auto entry = gvInitInst.getParent();
  if (entry != &(main->getEntryBlock())) {
    return nullptr;
  }

  for (auto &I : *entry) {
    if (llvm::CallInst *shadowCI = llvm::dyn_cast<llvm::CallInst>(&I)) {
      seadsa::ShadowMemInstOp op = sm.getShadowMemOp(*shadowCI);
      CallSite CS(shadowCI);
      if ( // shadow.mem.global.init used for non-scalar global variables
          (op == seadsa::ShadowMemInstOp::GLOBAL_INIT &&
           (&v == CS.getArgument(2)->stripPointerCasts())) ||
          ( // shadow.mem.arg.init used for function inputs including
            // scalar global variables.
              op == seadsa::ShadowMemInstOp::ARG_INIT &&
              (&v == CS.getArgument(1)->stripPointerCasts()))) {
        return shadowCI;
      }
    }
  }
  return nullptr;
}

Region getShadowRegionFromGvInitializer(const seadsa::ShadowMem &sm,
                                        const llvm::DataLayout &dl,
                                        llvm::Instruction &gvInitInst,
                                        llvm::Value &v) {

  if (CallInst *shadowCI = getShadowCIFromGvInitializer(sm, gvInitInst, v)) {
    return getShadowRegion(*shadowCI, dl, sm);
  } else {
    return Region();
  }
}

Region getShadowRegionFromLoadOrStore(const seadsa::ShadowMem &sm,
                                      const llvm::DataLayout &dl,
                                      llvm::Instruction &loadOrStore) {
  Region res;
  auto it = loadOrStore.getIterator();
  --it;
  if (llvm::CallInst *shadowCI = llvm::dyn_cast<llvm::CallInst>(&*it)) {
    seadsa::ShadowMemInstOp op = sm.getShadowMemOp(*shadowCI);
    switch (op) {
    case seadsa::ShadowMemInstOp::LOAD:
    case seadsa::ShadowMemInstOp::STORE:
      res = getShadowRegion(*shadowCI, dl, sm);
      break;
    default:;
      // CLAM_ERROR("unreachable");
    }
  }
  return res;
}

} // end namespace clam
