#pragma once

#include <crab/numbers/bignums.hpp>
#include <crab/types/linear_constraints.hpp>
#include <crab/types/variable.hpp>
#include <crab/types/varname_factory.hpp>

#include <cstdint>
#include <map>
#include <string>
#include <unordered_map>
#include <memory>

namespace crab {  
template <> class variable_name_traits<std::string> {
public:
  static std::string to_string(std::string v) { return v; }
};
} //end namespace crab

namespace clam {
namespace json {

/** Use strings as variable names **/
using variable_factory_t = crab::var_factory_impl::str_variable_factory;
using varname_t = typename variable_factory_t::varname_t;
using number_t = ikos::z_number;
using linear_constraint_t = ikos::linear_constraint<number_t, varname_t>;
using linear_constraint_system_t =
    ikos::linear_constraint_system<number_t, varname_t>;
using invariants_cache_t =
  std::unordered_map<std::string, std::shared_ptr<linear_constraint_system_t>>;
  
// Simple container
struct parse_options {
  // Fresh variable factory to create variables from the parsed JSON file.
  variable_factory_t m_vfac;
  // Type for the variables: we use the same type for all variables.
  crab::variable_type m_var_type;
  // Whether we want to compare semantically invariants
  bool m_semantic_diff;

  parse_options(bool semantic_diff)
      : m_var_type(crab::variable_type(crab::variable_type_kind::INT_TYPE, 32)),
        m_semantic_diff(semantic_diff) {}
  parse_options(const parse_options &other) = delete;
  parse_options &operator=(const parse_options &other) = delete;
};

// Simple container
struct analysis_results {
  using invariant_map = std::map<std::string, std::shared_ptr<linear_constraint_system_t>>;
  using str_invariant_map = std::map<std::string, std::string>;  
  std::string function_name;
  std::string date;
  std::string clam_version;
  std::string analysis_time;
  std::string domain;
  invariant_map invariants;
  str_invariant_map str_invariants;  
  std::uint64_t safe_checks;
  std::uint64_t warning_checks;
  std::uint64_t error_checks;

  analysis_results()
      : function_name(""), date(""), clam_version(""), analysis_time(""),
        domain(""), safe_checks(0), warning_checks(0), error_checks(0) {}
};

// Simple non-copyable container
struct global_analysis_results {
  std::map<std::string, std::unique_ptr<analysis_results>> m_results;
  global_analysis_results() {}
  global_analysis_results(const global_analysis_results &other) = delete;
  global_analysis_results &
  operator=(const global_analysis_results &other) = delete;

  bool contains(const std::string &function_name) const {
    return m_results.find(function_name) != m_results.end();
  }

  // precondition: contains(function_name) returns true
  const analysis_results &get(const std::string &function_name) const {
    return *(m_results.at(function_name));
  }
};

std::unique_ptr<global_analysis_results>
read_json(std::string json_filename, clam::json::parse_options &clam_opts,
	  // Cache to avoid converting the same string to linear_constraint_system_t over and over.
	  // if null then no caching.
	  invariants_cache_t *cache);
} // end namespace json
} // end namespace clam
