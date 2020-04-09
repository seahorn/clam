#pragma once

#include "clam/crab/crab_cfg.hh"

namespace clam {

/** User-definable parameters to build a Crab CFG **/
struct CrabBuilderParams {
  // Level of abstraction of the CFG
  crab::cfg::tracked_precision precision_level;
  // Perform dead code elimination, cfg simplifications, etc
  bool simplify;
  // translate precisely calls
  bool interprocedural;
  // Lower singleton aliases (e.g., globals) to scalar ones
  bool lower_singleton_aliases;
  // Translate memory operations in SSA form
  bool memory_ssa;
  // Remove useless havoc operations 
  bool include_useless_havoc;
  // Translation tuned for array smashing to be both sound and more
  // precise.
  bool use_array_smashing;
  // Translate bignums (> 64), otherwise operations with big numbers
  // are havoced.
  bool enable_bignums;
  //// --- printing options
  // print the cfg after it has been built
  bool print_cfg;
  
  CrabBuilderParams():
    precision_level(crab::cfg::NUM)
    , simplify(false)
    , interprocedural(true)
    , lower_singleton_aliases(false)
    , memory_ssa(false)
    , include_useless_havoc(true)
    , use_array_smashing(true)
    , enable_bignums(false)
    , print_cfg(false) {}
  
  CrabBuilderParams(crab::cfg::tracked_precision _precision_level,
		    bool _simplify, bool _interprocedural, bool _lower_singleton_aliases,
		    bool _memory_ssa, 
		    bool _include_useless_havoc, bool _use_array_smashing,
		    bool _enable_bignums,
		    bool _print_cfg):
    precision_level(_precision_level)
    , simplify(_simplify)
    , interprocedural(_interprocedural)
    , lower_singleton_aliases(_lower_singleton_aliases)
    , memory_ssa(_memory_ssa)
    , include_useless_havoc(_include_useless_havoc)
    , use_array_smashing(_use_array_smashing) 
    , enable_bignums(_enable_bignums)
    , print_cfg(_print_cfg) {}
  
  bool track_pointers() const {
    return precision_level == crab::cfg::PTR;
  }
  
  /* Represent only booleans and integers */
  void set_num_precision() {
    precision_level = crab::cfg::NUM;
  }
  
  /* Represent booleans, integers, and pointers */
  void set_pointer_precision() {
    precision_level = crab::cfg::PTR;
  }
  
  /* Represent booleans, integers, and arrays of those types */
  void set_array_precision() {
    precision_level = crab::cfg::ARR;
  }

  /* Produce all Crab CFGs in Memory SSA form */
  void enable_memory_ssa() {
    memory_ssa = true;
  }
  
  void write(llvm::raw_ostream &o) const;
};

} // end namespace clam

