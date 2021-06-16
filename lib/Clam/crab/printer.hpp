#include "clam/Clam.hh"
#include "crab/analysis/dataflow/assumptions.hpp"
#include "crab/checkers/checker.hpp"

#include <functional>
#include <string>
#include <vector>

/** Print Crab Analysis Results **/

namespace clam {
namespace crab_pretty_printer {

using abs_dom_map_t = typename IntraClam::abs_dom_map_t;

/** Generic class for a block annotation **/
class block_annotation {
public:
  block_annotation() = default;
  virtual ~block_annotation() = default;

  virtual std::string name() const = 0;
  // Print at the beginning of a block
  virtual void print_begin(const basic_block_label_t &bbl, crab::crab_os &o) const {}
  // Print at the end of a block
  virtual void print_end(const basic_block_label_t &bbl, crab::crab_os &o) const {}
  // Print at the beginning of a statement
  virtual void print_begin(const statement_t &s, crab::crab_os &o) const {}
  // Print at the end of a statement
  virtual void print_end(const statement_t &s, crab::crab_os &o) const {}
};

/** Invariant annotations **/
class invariant_annotation : public block_annotation {
public:
  using lookup_function = std::function<llvm::Optional<clam_abstract_domain>(
      const abs_dom_map_t, const llvm::BasicBlock &,
      const std::vector<varname_t> &)>;

private:
  const abs_dom_map_t &m_premap;
  const abs_dom_map_t &m_postmap;
  const std::vector<varname_t> &m_shadow_vars;
  lookup_function m_lookup;

public:
  invariant_annotation(const abs_dom_map_t &premap,
                       const abs_dom_map_t &postmap,
                       const std::vector<varname_t> &shadow_vars,
                       lookup_function lookup);

  void print_begin(const basic_block_label_t &bbl, crab::crab_os &o) const override;

  void print_end(const basic_block_label_t &bbl, crab::crab_os &o) const override ;

  std::string name() const override { return "INVARIANTS"; }
};

/** Unproven assumptions annotations **/
class unproven_assumption_annotation : public block_annotation {
public:
  using unproven_assumption_analysis_t = crab::analyzer::assumption_analysis<cfg_ref_t>;

private:
  cfg_ref_t m_cfg;
  unproven_assumption_analysis_t &m_analyzer;

public:
  unproven_assumption_annotation(cfg_ref_t cfg,
                                 unproven_assumption_analysis_t &analyzer);

  std::string name() const override { return "UNPROVEN ASSUMPTIONS"; }

  void print_begin(const statement_t &s, crab::crab_os &o) const override ;
};


/** Variables-of-influence of assertions **/
class voi_annotation : public block_annotation {
public:
  using voi_analysis_t = crab::analyzer::inter_assertion_crawler<cg_t>;

private:
  cfg_ref_t m_cfg;
  voi_analysis_t &m_analyzer;

public:
  voi_annotation(cfg_ref_t cfg, voi_analysis_t &analyzer);

  std::string name() const override { return "VARIABLES-OF-INFLUENCE"; }

  void print_begin(const basic_block_label_t &s, crab::crab_os &o) const override;
};
  
/** Print a block together with its annotations **/
class print_block {
  cfg_ref_t m_cfg;
  crab::crab_os &m_o;
  const typename IntraClam::checks_db_t &m_checksdb;
  const std::vector<std::unique_ptr<block_annotation>> &m_annotations;

public:
  print_block(
      cfg_ref_t cfg, crab::crab_os &o,
      const typename IntraClam::checks_db_t &checksdb,
      const std::vector<std::unique_ptr<block_annotation>> &annotations);

  void operator()(const basic_block_label_t &bbl) const;
};

void print_annotated_cfg(
    crab::crab_os &os, cfg_ref_t cfg,
    const typename IntraClam::checks_db_t &checksdb,
    const std::vector<std::unique_ptr<block_annotation>> &annotations);

} // namespace crab_pretty_printer
} // namespace clam
