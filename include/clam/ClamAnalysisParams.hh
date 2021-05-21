#pragma once

#include <array>
#include <climits>
#include <llvm/ADT/StringRef.h>
#include <string>

#include "clam/CrabDomain.hh"

namespace clam {
////
// Kind of checker
////
enum class CheckerKind { NOCHECKS = 0, ASSERTION = 1 };

/**
 * Class to set analysis options
 **/
struct AnalysisParams {
  CrabDomain::Type dom;
  bool run_backward;
  bool run_liveness;
  /* begin inter-procedural analysis */
  bool run_inter;
  unsigned int max_calling_contexts;
  bool analyze_recursive_functions;
  bool exact_summary_reuse;
  bool inter_entry_main;
  /* end inter-procedural analysis */
  unsigned relational_threshold;
  unsigned widening_delay;
  unsigned narrowing_iters;
  unsigned widening_jumpset;
  bool stats;
  bool print_invars;
  bool print_preconds;  /*unused*/  
  bool print_unjustified_assumptions;
  bool print_summaries; /*unused*/
  bool store_invariants;
  bool keep_shadow_vars;
  CheckerKind check;
  unsigned check_verbose;

  AnalysisParams()
      : dom(CrabDomain::INTERVALS), run_backward(false), run_liveness(false),
        run_inter(false), max_calling_contexts(UINT_MAX),
        analyze_recursive_functions(false), exact_summary_reuse(true),
	inter_entry_main(false), 
        relational_threshold(10000), widening_delay(1), narrowing_iters(10),
        widening_jumpset(0), stats(false), print_invars(false),
        print_preconds(false), print_unjustified_assumptions(false),
        print_summaries(false), store_invariants(true), keep_shadow_vars(false),
        check(CheckerKind::NOCHECKS), check_verbose(0) {}
};
} // end namespace clam
