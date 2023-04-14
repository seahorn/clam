#pragma once

#include "clam/Clam.hh"
#include <memory>

namespace clam {
namespace json {

using abs_dom_map_t = typename IntraClam::abs_dom_map_t;
using checks_db_t = typename IntraClam::checks_db_t;

class json_report_impl;

class json_report {
  std::unique_ptr<json_report_impl> m_pimpl;

public:
  json_report();

  ~json_report();

  void write(/* the cfg under analysis */
             cfg_ref_t m_cfg,
             /* parameters used for the analysis*/
             const AnalysisParams &params,
             /* invariants that hold at the beginning of each basic block*/
             const abs_dom_map_t &invariants,
             /* database with number of proven/disproven assertions */
             const checks_db_t &m_checksdb);

  std::string generate();
};

} // end namespace json
} // end namespace clam
