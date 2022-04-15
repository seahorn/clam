#pragma once

#include "clam/crab/crab_lang.hh"

namespace clam {

enum class CrabBuilderPrecision {
  NUM,           // only integers and boolean
  SINGLETON_MEM, // NUM + singleton memory objects
  MEM            // NUM + all memory objects
};

/** User-definable parameters to build a Crab CFG **/
struct CrabBuilderParams {
  // Level of abstraction of the CFG
  CrabBuilderPrecision precision_level;
  // Perform dead code elimination, cfg simplifications, etc.  If
  // enabled, this option might make harder to map back from CrabIR to
  // LLVM IR.
  bool simplify;
  // translate precisely calls
  bool interprocedural;
  // Lower singleton aliases (e.g., globals) to scalar ones
  bool lower_singleton_aliases;
  // Remove useless havoc operations
  bool include_useless_havoc;
  // Translate bignums (> 64), otherwise operations with big numbers
  // are havoced.
  bool enable_bignums;
  // Lower unsigned comparison operators into signed one
  // The translation is made under crab IR
  bool lower_unsigned_icmp;
  // Avoid adding boolean crabIR statements in some cases (e.g.,
  // bool_assume/bool_assert) because Crab is, in general, not very
  // good at boolean reasoning.
  bool avoid_boolean;
  // Remove llvm.OP.with.overflow.* with the corresponding OP without
  // overflow checking. Thus, this transformation should be applied
  // only if the operation is known to not overflow.
  bool lower_arithmetic_with_overflow_intrinsics;
  // Add make_ref for global values (e.g., global variables, functions etc)
  bool allocate_global_values;
  /// Add reasonable assumptions about pointers (e.g., allocas and
  /// globals cannot be null, external functions do not return
  /// dangling pointers, etc.)
  bool add_pointer_assumptions;
  /// Zero-initialization of stack arrays.
  /// Note that this is not C semantics.
  bool zero_initialize_stack_arrays;
  /// Emit crabIR to perform null-dereference checks
  bool add_null_checks;
  /// Emit crabIR to perform use-after-free checks  
  bool add_uaf_checks;
  /// Emit crabIR to perform buffer-overflow checks at all memory
  /// accesses.
  bool add_bounds_checks;
  /// Emit crabIR to reason about sea_is_dereferenceable
  bool add_is_deref;
  /// Add above checks only on certain heap shapes
  bool check_only_typed_regions;
  bool check_only_noncyclic_regions;
  //// --- printing options
  // print the cfg after it has been built
  bool print_cfg;
  // print the cfg to dot format after it has been built
  bool dot_cfg;

  CrabBuilderParams()
      : precision_level(CrabBuilderPrecision::NUM), simplify(false),
        interprocedural(true), lower_singleton_aliases(false),
        include_useless_havoc(true), enable_bignums(false),
        lower_unsigned_icmp(false), avoid_boolean(true),
	lower_arithmetic_with_overflow_intrinsics(false),
	allocate_global_values(true),
        add_pointer_assumptions(true),
	zero_initialize_stack_arrays(false),
	add_null_checks(false),
        add_uaf_checks(false), add_bounds_checks(false), add_is_deref(false),
        check_only_typed_regions(false), check_only_noncyclic_regions(false),
        print_cfg(false), dot_cfg(false) {}

  bool trackMemory() const {
    return precision_level == CrabBuilderPrecision::MEM;
  }

  bool trackOnlySingletonMemory() const {
    return precision_level == CrabBuilderPrecision::SINGLETON_MEM;
  }

  bool addPointerAssumptions() const {
    return trackMemory() && add_pointer_assumptions;
  }

  bool allocateGlobals() const {
    return trackMemory() && allocate_global_values;
  }
  
  /* Set lower unsigned cmp into signed for Crab programs */
  void lowerUnsignedICmpIntoSigned() {
    if (!lower_unsigned_icmp) {
      lower_unsigned_icmp = true;
      avoid_boolean = false;
    }
  }

  /* Set the level of abstraction for Crab programs */
  void setPrecision(CrabBuilderPrecision val) {
    precision_level = val;
  }

  void write(llvm::raw_ostream &o) const;
};

} // end namespace clam
