#pragma once

#include "clam/CrabIREmitter.hh"

#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/IR/Instructions.h"

#include <memory>

/**
 * Hide the calls to the emitters before and after adding a Crab
 * statement.
 */
namespace clam {

class insertCrabIRWithEmitter {
public:
  // Insert make_ref statement in bb
  static const statement_t *
  make_ref(llvm::Instruction &I, CrabIREmitterVec &propertyEmitters,
           basic_block_t &bb, const llvm::TargetLibraryInfo &tli, var_t lhs_ref,
           var_t region, var_or_cst_t size, crab::tag as);

  // Insert remove_ref statement in bb
  static const statement_t *remove_ref(llvm::Instruction &I,
                                       CrabIREmitterVec &propertyEmitters,
                                       basic_block_t &bb,
                                       const llvm::TargetLibraryInfo &tli,
                                       var_t region, var_t ref);

  // Insert gep_ref statement in bb
  static const statement_t *gep_ref(llvm::Instruction &I,
                                    CrabIREmitterVec &propertyEmitters,
                                    basic_block_t &bb, var_t lhs_ref,
                                    var_t lhs_region, var_t rhs_ref,
                                    var_t rhs_region, lin_exp_t offset = number_t(0));

  // Insert load_from_ref statement in bb
  static const statement_t *load_from_ref(llvm::LoadInst &I,
                                          CrabIREmitterVec &propertyEmitters,
                                          basic_block_t &bb, var_t lhs,
                                          var_t ref, var_t region);

  // Insert store_to_ref statement in bb
  static const statement_t *store_to_ref(llvm::StoreInst &I,
                                         CrabIREmitterVec &propertyEmitters,
                                         basic_block_t &bb, var_t ref,
                                         var_t region, var_or_cst_t val);

  // Insert select_ref statement in bb
  static const statement_t *select_ref(llvm::SelectInst &I,
                                       CrabIREmitterVec &propertyEmitters,
                                       basic_block_t &bb, var_t lhs_ref,
                                       var_t lhs_region, var_t cond,
                                       CrabSelectRefOps::opt_pair_var_t op1,
                                       CrabSelectRefOps::opt_pair_var_t op2);
};

} // namespace clam
