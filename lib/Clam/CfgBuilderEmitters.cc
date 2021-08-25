#include "CfgBuilderEmitters.hh"

#include "clam/Support/Debug.hh"

using namespace llvm;

namespace clam {

const statement_t *insertCrabIRWithEmitter::make_ref(
    Instruction &I, CrabIREmitterVec &propertyEmitters, basic_block_t &bb,
    const TargetLibraryInfo &tli, var_t lhs_ref, var_t region,
    var_or_cst_t size, crab::tag as) {
  CrabMakeRefOps s(lhs_ref, region, size, as, bb);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitBeforeAlloc(I, tli, s);
  }
  const statement_t *res = bb.make_ref(lhs_ref, region, size, as);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitAfterAlloc(I, tli, s);
  }
  return res;
}

const statement_t *insertCrabIRWithEmitter::remove_ref(
    Instruction &I, CrabIREmitterVec &propertyEmitters, basic_block_t &bb,
    const TargetLibraryInfo &tli, var_t region, var_t ref) {
  CrabRemoveRefOps s(region, ref, bb);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitBeforeFree(I, tli, s);
  }
  const statement_t *res = bb.remove_ref(region, ref);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitAfterFree(I, tli, s);
  }
  return res;
}

const statement_t *insertCrabIRWithEmitter::gep_ref(
    Instruction &I, CrabIREmitterVec &propertyEmitters, basic_block_t &bb,
    var_t lhs_ref, var_t lhs_region, var_t rhs_ref, var_t rhs_region,
    lin_exp_t offset) {
  CrabGepRefOps s(lhs_ref, lhs_region, rhs_ref, rhs_region, offset, bb);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitBeforeGep(I, s);
  }
  const statement_t *res =
      bb.gep_ref(lhs_ref, lhs_region, rhs_ref, rhs_region, offset);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitAfterGep(I, s);
  }
  return res;
}

const statement_t *insertCrabIRWithEmitter::load_from_ref(
    LoadInst &I, CrabIREmitterVec &propertyEmitters, basic_block_t &bb,
    var_t lhs, var_t ref, var_t region) {
  CrabLoadRefOps s(lhs, region, ref, bb);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitBeforeLoad(I, s);
  }
  const statement_t *res = bb.load_from_ref(lhs, ref, region);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitAfterLoad(I, s);
  }
  return res;
}

const statement_t *insertCrabIRWithEmitter::store_to_ref(
    StoreInst &I, CrabIREmitterVec &propertyEmitters, basic_block_t &bb,
    var_t ref, var_t region, var_or_cst_t val) {

  CrabStoreRefOps s(ref, region, val, bb);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitBeforeStore(I, s);
  }
  const statement_t *res = bb.store_to_ref(ref, region, val);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitAfterStore(I, s);
  }
  return res;
}

const statement_t *insertCrabIRWithEmitter::select_ref(
    SelectInst &I, CrabIREmitterVec &propertyEmitters, basic_block_t &bb,
    var_t lhs_ref, var_t lhs_region, var_t cond,
    CrabSelectRefOps::opt_pair_var_t op1,
    CrabSelectRefOps::opt_pair_var_t op2) {

  CrabSelectRefOps s(lhs_ref, lhs_region, cond, op1, op2, bb);
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitBeforeRefSelect(I, s);
  }
  const statement_t *res = nullptr;
  if (op1.hasValue() && !op2.hasValue()) {
    var_t op1_ref = op1.getValue().first;
    var_t op1_region = op1.getValue().second;
    res = bb.select_ref_null_false_value(lhs_ref, lhs_region, cond, op1_ref,
                                         op1_region);
  } else if (!op1.hasValue() && op2.hasValue()) {
    var_t op2_ref = op2.getValue().first;
    var_t op2_region = op2.getValue().second;
    res = bb.select_ref_null_true_value(lhs_ref, lhs_region, cond, op2_ref,
                                        op2_region);
  } else if (op1.hasValue() && op2.hasValue()) {
    var_t op1_ref = op1.getValue().first;
    var_t op1_region = op1.getValue().second;
    var_t op2_ref = op2.getValue().first;
    var_t op2_region = op2.getValue().second;
    res = bb.select_ref(lhs_ref, lhs_region, cond, op1_ref, op1_region, op2_ref,
                        op2_region);
  } else { // unreachable
    CLAM_ERROR("both select operands cannot be null");
  }
  for (unsigned i = 0, sz = propertyEmitters.size(); i < sz; ++i) {
    propertyEmitters[i]->visitAfterRefSelect(I, s);
  }
  return res;
}

} // end namespace clam
