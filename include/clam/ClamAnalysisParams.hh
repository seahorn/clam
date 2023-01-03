#pragma once

#include "clam/CrabDomain.hh"
#include <llvm/Support/raw_ostream.h>
#include <climits>
#include <string>

namespace clam {
////
// Kind of checker
////
enum class CheckerKind { NOCHECKS = 0, ASSERTION = 1 };

/**
 * Class to set analysis options
 **/
struct AnalysisParams {
  // The abstract domain
  CrabDomain::Type dom;
  // Run experimental backward analysis (only intra-procedural)
  bool run_backward;
  // Simplify CFG by removing dead variables
  bool run_liveness;
  /* begin inter-procedural analysis */
  bool run_inter;
  unsigned int max_calling_contexts;
  bool analyze_recursive_functions;
  bool exact_summary_reuse;
  bool inter_entry_main;
  /* end inter-procedural analysis */
  /* begin fixpoint parameters */
  unsigned relational_threshold;
  unsigned widening_delay;
  unsigned narrowing_iters;
  unsigned widening_jumpset;
  /* end fixpoint parameters */  
  bool stats;
  /* begin pretty-printing */
  bool print_invars;
  bool print_preconds;  /*unused*/
  bool print_unjustified_assumptions;
  bool print_summaries; /*unused*/
  // print variables of interest that might affect assertions
  bool print_voi; 
  /* end pretty printing */
  /* Print CrabIR + optional invariants to file */
  std::string output_crabir;
  /* Print invariants to json format */
  std::string output_json;
  /* keep invariants for clients */
  bool store_invariants;
  /* internal option */
  bool keep_shadow_vars;
  /* run checker after analysis has finished */
  CheckerKind check;
  unsigned check_verbose;

  AnalysisParams()
      : dom(CrabDomain::INTERVALS),
	run_backward(false), run_liveness(false),
        run_inter(false), max_calling_contexts(UINT_MAX),
        analyze_recursive_functions(false), exact_summary_reuse(true),
	inter_entry_main(false), 
        relational_threshold(10000), widening_delay(1), narrowing_iters(10),
        widening_jumpset(0), stats(false), print_invars(false), 
        print_preconds(false), print_unjustified_assumptions(false),
        print_summaries(false), print_voi(false),
	output_crabir(""), output_json(""), store_invariants(true), keep_shadow_vars(false),
        check(CheckerKind::NOCHECKS), check_verbose(0) {}

  void write(llvm::raw_ostream &o) const;  
};
} // end namespace clam
