#pragma once
/** This file can go to the Crab library **/

#include <boost/algorithm/string/classification.hpp> // boost::is_any_of
#include <boost/algorithm/string/split.hpp>
#include <boost/lexical_cast.hpp>
#include <regex>

#include <crab/types/linear_constraints.hpp>
#include <crab/types/variable.hpp>

namespace crab {

#define TRUE_PATTERN R"_(\s*true\s*)_"
#define FALSE_PATTERN R"_(\s*false\s*)_"  
#define IMM_PATTERN R"_(\s*([-+]?\d+)\s*)_"
#define KIND_PATTERN R"_(\s*(<=|<|>=|>|==|=|!=|<=_u|<_u|>=_u|>_u)\s*)_"
#define LIT_PATTERN R"_(\s*([-+]?)(\d*)\*?([\.@a-zA-Z_][\.a-zA-Z0-9_]*)\s*)_"
#define LHS_CST_PATTERN R"_(\s*(([-+]?\d*\*?[\.@a-zA-Z_][\.a-zA-Z0-9_]*)*)\s*)_"
#define CSTS_SYS_PATTERN R"_(\s*\{(.*)\}\s*)_"

/**
 *  Parse a string as a linear constraint where all variables have type @ty.
 *  @cst_text is expected to have the same format than
 *  linear_constraint::write() uses.
 **/
template <typename Number, typename VariableName, typename VariableNameFactory>
ikos::linear_constraint<Number, VariableName>
parse_linear_constraint(const std::string &cst_text, VariableNameFactory &vfac,
                        variable_type ty) {

  static_assert(std::is_same<VariableName,
                             typename VariableNameFactory::varname_t>::value,
                "variable factory and linear constraint must agree on the type "
                "VariableName");

  using linear_expression_t = ikos::linear_expression<Number, VariableName>;
  using linear_constraint_t = ikos::linear_constraint<Number, VariableName>;
  using variable_t = typename linear_constraint_t::variable_t;
  using number_t = typename linear_constraint_t::number_t;

  auto parse_number = [](const std::string &s) -> number_t {
    return number_t(s);
  };

  auto parse_var = [&ty](const std::string &s, VariableNameFactory &vfac) {
    return variable_t(vfac[s], ty);
  };

  auto parse_lhs_cst = [&parse_var, &parse_number](const std::string &s,
                                                   VariableNameFactory &vfac) {
    std::regex p(LIT_PATTERN);
    auto vars_begin = std::sregex_iterator(s.begin(), s.end(), p);
    auto vars_end = std::sregex_iterator();
    linear_expression_t e(0);
    for (std::sregex_iterator it = vars_begin; it != vars_end; ++it) {
      std::smatch match = *it;
      std::string polarity_text = match[1].str();
      std::string coefficient_text = match[2].str();
      std::string var_text = match[3].str();
      number_t coefficient =
          (coefficient_text == "" ? number_t(1)
                                  : parse_number(coefficient_text));
      if (polarity_text == "+" || polarity_text == "") {
        e = e + (coefficient * parse_var(var_text, vfac));
      } else if (polarity_text == "-") {
        e = e - (coefficient * parse_var(var_text, vfac));
      } else {
        CRAB_ERROR("parser of linear constraint cannot recognize polarity ",
                   polarity_text);
      }
    }
    return e;
  };

  auto build_cst = [](const std::string &kind,
                      const linear_expression_t &e) -> linear_constraint_t {
    if (kind == "<=") {
      return linear_constraint_t(e <= 0);
    } else if (kind == "<") {
      return linear_constraint_t(e < 0);
    } else if (kind == ">=") {
      return linear_constraint_t(e >= 0);
    } else if (kind == ">") {
      return linear_constraint_t(e > 0);
    } else if (kind == "==" || kind == "=") {
      return linear_constraint_t(e == 0);
    } else if (kind == "!=") {
      return linear_constraint_t(e != 0);
    } else if (kind == "<=_u") {
      linear_constraint_t c(e <= 0);
      c.set_unsigned();
      return c;
    } else if (kind == "<_u") {
      linear_constraint_t c(e < 0);
      c.set_unsigned();
      return c;
    } else if (kind == ">=_u") {
      linear_constraint_t c(e >= 0);
      c.set_unsigned();
      return c;
    } else if (kind == ">_u") {
      linear_constraint_t c(e > 0);
      c.set_unsigned();
      return c;
    } else {
      CRAB_ERROR("parser of linear constraint cannot recognize ", kind);
    }
  };

  std::smatch m;
  if (std::regex_match(cst_text, m, std::regex(TRUE_PATTERN))) {    
    return linear_constraint_t::get_true();
  } else if (std::regex_match(cst_text, m, std::regex(FALSE_PATTERN))) {
    return linear_constraint_t::get_false();
  } else {
    if (std::regex_match(
            cst_text, m,
            std::regex(LHS_CST_PATTERN KIND_PATTERN IMM_PATTERN))) {
      linear_expression_t e = parse_lhs_cst(m[1], vfac);
      number_t n = parse_number(m[4]);
      std::string kind_text = m[3];
      return build_cst(kind_text, e - n);
    } else {
      CRAB_ERROR("cannot parse ", cst_text, " as a linear constraint");
    }
  }
}

/**
 * Parse a string as a system of linear constraints where all variables have
 * type @ty.
 **/
template <typename Number, typename VariableName, typename VariableNameFactory>
ikos::linear_constraint_system<Number, VariableName>
parse_linear_constraint_system(const std::string &csts_text,
                               VariableNameFactory &vfac, variable_type ty) {
  static_assert(std::is_same<VariableName,
                             typename VariableNameFactory::varname_t>::value,
                "variable factory and linear constraint must agree on the type "
                "VariableName");
  std::smatch m;
  if (std::regex_match(csts_text, m, std::regex(CSTS_SYS_PATTERN))) {
    ikos::linear_constraint_system<Number, VariableName> out;
    std::string csts_text = m[1];
    if (csts_text == "") {
      return out;
    }

    std::vector<std::string> tokens;
    boost::algorithm::split(tokens, csts_text, boost::is_any_of(";"));
    for (auto const &token : tokens) {
      out += parse_linear_constraint<Number, VariableName, VariableNameFactory>(
          token, vfac, ty);
    }
    return out;
  } else {
    CRAB_ERROR("cannot parse ", csts_text,
               " as a system of linear constraints");
  }
}

} // end namespace crab
