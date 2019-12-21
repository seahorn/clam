#include "CfgBuilderShadowMem.hh"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/Optional.h"

#include "sea_dsa/ShadowMem.hh"
#include "sea_dsa/Graph.hh"

namespace clam {

using namespace llvm;

// Helper to return the shadow Value *defined* and *used* by a StoreInst
std::pair<Value*, Value*>
getShadowMemDefAndUse(StoreInst &I, const sea_dsa::ShadowMem& sm){
  auto it =  I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  sea_dsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == sea_dsa::ShadowMemInstOp::STORE);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  return sm.getShadowMemVars(ci);
}

// Helper to return the shadow Value *used* by a LoadInst
Value& getShadowMemUse(LoadInst &I, const sea_dsa::ShadowMem& sm){
  auto it =  I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  sea_dsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == sea_dsa::ShadowMemInstOp::LOAD);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  Value *useV = sm.getShadowMemVars(ci).second;
  assert(useV);
  return *useV;
}

const Value* getShadowMemUniqueScalar(StoreInst &I, const sea_dsa::ShadowMem &sm) {
  auto it =  I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  sea_dsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == sea_dsa::ShadowMemInstOp::STORE);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  return cell.getNode()->getUniqueScalar();
}

const Value* getShadowMemUniqueScalar(LoadInst &I, const sea_dsa::ShadowMem &sm) {
  auto it =  I.getIterator();
  --it;
  CallInst &ci = *(cast<CallInst>(&*it));
  sea_dsa::ShadowMemInstOp op = sm.getShadowMemOp(ci);
  assert(op == sea_dsa::ShadowMemInstOp::LOAD);
  auto cell_opt = sm.getShadowMemCell(ci);
  assert(cell_opt.hasValue());
  auto cell = cell_opt.getValue();
  return cell.getNode()->getUniqueScalar();
}

bool isShadowMemPHINode(llvm::PHINode &PN) {
  return (PN.getMetadata("shadow.mem.phi"));
}

llvm::Optional<sea_dsa::Cell> getShadowMemCell(const llvm::PHINode &PHI,
					       const sea_dsa::ShadowMem &sm) {
  // TODO
  return llvm::None;
}
} // end namespace clam
