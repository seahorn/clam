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
  // Avoid adding boolean crabIR statements in some cases (e.g.,
  // bool_assume/bool_assert) because Crab is, in general, not very
  // good at boolean reasoning.
  bool avoid_boolean;
  /// Add reasonable assumptions about pointers (e.g., allocas and
  /// globals cannot be null, external functions do not return
  /// dangling pointers, etc.)
  bool add_pointer_assumptions;
  /// Emit crabIR to perform null-dereference checks
  bool add_null_checks;
  /// Emit crabIR to perform use-after-free checks  
  bool add_uaf_checks;
  /// Emit crabIR to perform buffer-overflow checks
  bool add_bounds_checks;
  /// Add above checks only on certain heap shapes
  bool check_only_typed_regions;
  bool check_only_noncyclic_regions;
  //// --- printing options
  // print the cfg after it has been built
  bool print_cfg;
  // print the cfg to dot format after it has been built
  bool dot_cfg;

  CrabBuilderParams()
      : precision_level(CrabBuilderPrecision::NUM),
	simplify(false),
        interprocedural(true),
	lower_singleton_aliases(false),
        include_useless_havoc(true),
	avoid_boolean(true),
	enable_bignums(false),
        add_pointer_assumptions(true),
	add_null_checks(false),
	add_uaf_checks(false),
	add_bounds_checks(false),
	check_only_typed_regions(false),
	check_only_noncyclic_regions(false),
	print_cfg(false),
	dot_cfg(false) {}

  bool trackMemory() const {
    return precision_level == CrabBuilderPrecision::MEM;
  }

  bool trackOnlySingletonMemory() const {
    return precision_level == CrabBuilderPrecision::SINGLETON_MEM;
  }

  bool addPointerAssumptions() const {
    return trackMemory() && add_pointer_assumptions;
  }

  /* Set the level of abstraction for Crab programs */
  void setPrecision(CrabBuilderPrecision val) {
    precision_level = val;
  }

  void write(llvm::raw_ostream &o) const;
};

} // end namespace clam
