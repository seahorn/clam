#pragma once

#include "clam/Clam.hh"
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace clam {
namespace json {

using abs_dom_map_t = typename IntraClam::abs_dom_map_t;
using checks_db_t = typename IntraClam::checks_db_t;

/**
 * LLM-friendly, per-assertion JSON report.
 *
 * Unlike json_report (write_json.hh), which emits aggregate check counts and
 * block-level invariants, this writer emits one record per assertion carrying
 * everything needed to reason about (and diff) individual proofs: id, source
 * location, constraint, OK/FAIL result, the invariant that holds at the
 * assertion's block, and the variables of influence.
 *
 * The schema is documented in scripts/crab_llm_json.py and MUST match the
 * output of scripts/ocrab_to_json.py so that a frozen clam (converted from
 * --ocrab) and a newer clam (--ojson-llm) can be compared interchangeably.
 *
 * The 'invariant_sliced' field (invariant projected onto the voi) is left null
 * on purpose: it is filled by the Python side (crab_llm_json.enrich) so that
 * the projection is computed in exactly one place for every producer.
 */
class json_llm_report_impl;

class json_llm_report {
  std::unique_ptr<json_llm_report_impl> m_pimpl;

public:
  // (crab basic-block name, assertion debug-info id) -> variables of influence.
  // Built by the caller from the voi analysis (empty if voi was not run).
  using voi_map_t =
      std::map<std::pair<std::string, int64_t>, std::vector<std::string>>;

  json_llm_report();
  ~json_llm_report();

  void write(/* the cfg under analysis */
             cfg_ref_t cfg,
             /* parameters used for the analysis */
             const AnalysisParams &params,
             /* invariants that hold at the entry of each basic block */
             const abs_dom_map_t &invariants,
             /* invariants that hold at the exit of each basic block */
             const abs_dom_map_t &post_invariants,
             /* database with proven/disproven assertions */
             const checks_db_t &checks,
             /* variables of influence per (block,assertion), possibly empty */
             const voi_map_t &voi);

  std::string generate();
};

} // end namespace json
} // end namespace clam
