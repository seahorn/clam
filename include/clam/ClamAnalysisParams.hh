#pragma once

#include "clam/config.h"

#include <climits>
#include <string>

namespace clam {

////
// Base numerical domains for user options
////
enum CrabDomain
  {   INTERVALS
      , INTERVALS_CONGRUENCES
      , BOXES
      , DIS_INTERVALS
      /*, ZONES_SPARSE_DBM*/
      , ZONES_SPLIT_DBM
      , TERMS_INTERVALS
      , TERMS_DIS_INTERVALS
      //TERMS_INTERVALS x  ZONES_SPLIT_DBM
      , TERMS_ZONES 
      //(#live vars<threshold ? TERMS_INTERVALSxZONES_SPLIT_DBM, INTERVALS)
      , ADAPT_TERMS_ZONES 
      , OCT
      , PK
      , WRAPPED_INTERVALS
  };
  
////
// Kind of checker
////
enum assert_check_kind_t
  { NOCHECKS = 0,
    ASSERTION = 1,
    NULLITY = 2
  };
  
  
/**
* Class to set analysis options
 **/
struct AnalysisParams {
  CrabDomain dom;
#ifndef TOP_DOWN_INTER_ANALYSIS  
  CrabDomain sum_dom; 
#endif   
  bool run_backward;
  bool run_liveness;
  bool run_inter;
#ifdef TOP_DOWN_INTER_ANALYSIS    
  unsigned int max_calling_contexts;
#endif   
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
#ifndef TOP_DOWN_INTER_ANALYSIS        
      sum_dom(ZONES_SPLIT_DBM),
#endif       
      run_backward(false), run_liveness(false),
      run_inter(false),
#ifdef TOP_DOWN_INTER_ANALYSIS        
      max_calling_contexts(UINT_MAX),
#endif       
      relational_threshold(10000),
      widening_delay(1), narrowing_iters(10), widening_jumpset(0),
      stats(false),
      print_invars(false), print_preconds(false),
      print_unjustified_assumptions(false), print_summaries(false),
      store_invariants(true), keep_shadow_vars(false),
      check(NOCHECKS), check_verbose(0) { }
  
  std::string abs_dom_to_str() const;

#ifndef TOP_DOWN_INTER_ANALYSIS  
  std::string sum_abs_dom_to_str() const;
#endif
};

} // end namespace clam
