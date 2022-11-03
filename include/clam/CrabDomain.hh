#pragma once

#include <array>
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
 *   constexpr Type NEW_DOMAIN(15, ...);
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
                               "zones using DBMs in Split Normal Form", true,
                               false);
constexpr Type OCT_SPLIT_DBM(7, "soct",
			     "octagons using DBMs in Split Normal Form", true,
			     false);
constexpr Type TERMS_INTERVALS(8, "term-int", "terms+intervals", true, false);
constexpr Type TERMS_DIS_INTERVALS(9, "term-dis-int",
                                   "terms+non-overlapping intervals", true,
                                   true);
constexpr Type TERMS_ZONES(10, "rtz", "terms+zones", true, false);
constexpr Type OCT(11, "oct", "octagons from Apron or Elina", true, false);
constexpr Type PK(12, "pk", "polyhedra from Apron or Elina", true, false);
constexpr Type PK_PPLITE(13, "pk-pplite", "polyhedra from PPLite", true, false);  
constexpr Type SIGN_CONSTANTS(14, "sign-const", "sign+constants", false, false);  
constexpr std::array<Type, 14> List = {INTERVALS,
                                       INTERVALS_CONGRUENCES,
                                       WRAPPED_INTERVALS,
                                       BOXES,
                                       DIS_INTERVALS,
                                       ZONES_SPLIT_DBM,
				       OCT_SPLIT_DBM,
                                       TERMS_INTERVALS,
                                       TERMS_DIS_INTERVALS,
                                       TERMS_ZONES,
                                       OCT,
                                       PK,
				       PK_PPLITE,
				       SIGN_CONSTANTS};
} // end namespace CrabDomain
} // end namespace clam
