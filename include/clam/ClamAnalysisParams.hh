#pragma once

#include <array>
#include <climits>
#include <llvm/ADT/StringRef.h>
#include <string>

namespace clam {
// Simulating an extensible c++ enum
namespace CrabDomain {
class Type {
private:
  uint16_t m_value;
  llvm::StringRef m_name;
  llvm::StringRef m_description;
  bool m_is_relational;
  bool m_is_disjunctive;

public:
  constexpr Type(uint16_t value, llvm::StringRef name,
                 llvm::StringRef description, bool is_relational,
                 bool is_disjunctive)
      : m_value(value), m_name(name), m_description(description),
        m_is_relational(is_relational), m_is_disjunctive(is_disjunctive) {}
  constexpr Type()
      : m_value(1), m_name("intervals"), m_description("intervals"),
        m_is_relational(false), m_is_disjunctive(false) {}
  ~Type() = default;
  constexpr Type(const Type &) = default;
  constexpr Type &operator=(const Type &) = default;
  constexpr operator uint16_t() const { return m_value; }
  constexpr uint16_t value() const { return m_value; }
  llvm::StringRef name() const { return m_name; }
  llvm::StringRef desc() const { return m_description; }
  bool isRelational() const { return m_is_relational; }
  bool isDisjunctive() const { return m_is_disjunctive; }
};
/**
 * Base numerical domains for user options. To be synchronized with
 * the domains defined in crab/crab_domains.hh
 *
 * To add a new domain value:
 *
 * namespace CrabDomain {
 *   constexpr Type NEW_DOMAIN(12, ...);
 * }
 **/

constexpr Type INTERVALS(1, "int", "intervals", false, false);
constexpr Type INTERVALS_CONGRUENCES(2, "ric", "intervals with congruences",
                                     false, false);
constexpr Type WRAPPED_INTERVALS(3, "w-int", "wrapped intervals", false, false);
constexpr Type BOXES(4, "boxes", "boxes based on LDDs", false, true);
constexpr Type DIS_INTERVALS(5, "dis-int", "cheap, non-overlapping intervals",
                             false, true);
constexpr Type ZONES_SPLIT_DBM(6, "zones",
                               "zones base on split normal form DBMs", true,
                               false);
constexpr Type TERMS_INTERVALS(7, "term-int", "terms+intervals", true, false);
constexpr Type TERMS_DIS_INTERVALS(8, "term-dis-int",
                                   "terms+non-overlapping intervals", true,
                                   true);
constexpr Type TERMS_ZONES(9, "rtz", "terms+zones", true, false);
constexpr Type OCT(10, "oct", "octagons", true, false);
constexpr Type PK(11, "pk", "polyhedra", true, false);
constexpr std::array<Type, 11> List = {INTERVALS,
                                       INTERVALS_CONGRUENCES,
                                       WRAPPED_INTERVALS,
                                       BOXES,
                                       DIS_INTERVALS,
                                       ZONES_SPLIT_DBM,
                                       TERMS_INTERVALS,
                                       TERMS_DIS_INTERVALS,
                                       TERMS_ZONES,
                                       OCT,
                                       PK};
} // end namespace CrabDomain

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
  CheckerKind check;
  unsigned check_verbose;

  AnalysisParams()
      : dom(CrabDomain::INTERVALS), run_backward(false), run_liveness(false),
        run_inter(false), max_calling_contexts(UINT_MAX),
        relational_threshold(10000), widening_delay(1), narrowing_iters(10),
        widening_jumpset(0), stats(false), print_invars(false),
        print_preconds(false), print_unjustified_assumptions(false),
        print_summaries(false), store_invariants(true), keep_shadow_vars(false),
        check(CheckerKind::NOCHECKS), check_verbose(0) {}
};

} // end namespace clam
