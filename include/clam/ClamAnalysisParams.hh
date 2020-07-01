#pragma once

#include <climits>
#include <string>

namespace clam {

////
// Base numerical domains for user options. To be synchronized with
// the domains defined in crab/crab_domains.hh
////
enum CrabDomain {
  INTERVALS,
  INTERVALS_CONGRUENCES,
  WRAPPED_INTERVALS,  
  BOXES,
  DIS_INTERVALS,
  ZONES_SPLIT_DBM,
  TERMS_INTERVALS,
  TERMS_DIS_INTERVALS,
  TERMS_ZONES, 
  //(#live vars<threshold ? TERMS_INTERVALSxZONES_SPLIT_DBM, INTERVALS)
  ADAPT_TERMS_ZONES,
  OCT,
  PK
};

////
// Kind of checker
////
enum assert_check_kind_t { NOCHECKS = 0, ASSERTION = 1 /*, NULLITY = 2*/ };

/**
 * Class to set analysis options
 **/
struct AnalysisParams {
  CrabDomain dom;
  bool run_backward;
  bool run_liveness;
  bool run_inter;
  unsigned int max_calling_contexts;
  unsigned relational_threshold;
  unsigned widening_delay;
  unsigned narrowing_iters;
  unsigned widening_jumpset;
  bool stats;
  bool print_invars;
  bool print_preconds;
  bool print_unjustified_assumptions;
  bool print_summaries;
  bool store_invariants;
  bool keep_shadow_vars;
  assert_check_kind_t check;
  unsigned check_verbose;

  AnalysisParams()
      : dom(INTERVALS),
        run_backward(false), run_liveness(false), run_inter(false),
        max_calling_contexts(UINT_MAX),
        relational_threshold(10000), widening_delay(1), narrowing_iters(10),
        widening_jumpset(0), stats(false), print_invars(false),
        print_preconds(false), print_unjustified_assumptions(false),
        print_summaries(false), store_invariants(true), keep_shadow_vars(false),
        check(NOCHECKS), check_verbose(0) {
  }

  std::string abs_dom_to_str() const;

};

} // end namespace clam
