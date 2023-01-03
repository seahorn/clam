#include "write_json.hh"

#include <boost/json/src.hpp>
#include <clam/Support/Debug.hh>
#include <ctime>

namespace clam {
namespace json {

class json_report_impl {
  boost::json::object m_report;

  std::string now();
  void write_invariants(const abs_dom_map_t &invariants,
                        boost::json::array &invariants_text);
  void write_parameters(const AnalysisParams &params,
                        boost::json::object &params_text);
  void write_checks(const checks_db_t &db, boost::json::object &db_text);

public:
  json_report_impl() {}

  ~json_report_impl() = default;

  void write(cfg_ref_t cfg, const AnalysisParams &params,
             const abs_dom_map_t &invariants, const checks_db_t &checks);

  std::string generate() { return boost::json::serialize(m_report); }
};

std::string json_report_impl::now() {
  time_t now = time(NULL);
  struct tm t = *localtime(&now);
  crab::crab_string_os os;
  os << t.tm_mon + 1 << "-" << t.tm_mday << "-" << t.tm_year + 1900 << " "
     << t.tm_hour << ":" << t.tm_min << ":" << t.tm_sec;
  return os.str();
}

void json_report_impl::write_invariants(const abs_dom_map_t &invariants,
                                        boost::json::array &invariants_text) {
  for (auto &kv : invariants) {
    std::string block = kv.first->getName().str();
    auto csts = kv.second.to_linear_constraint_system();
    crab::crab_string_os csts_text;
    csts.write(csts_text);
    boost::json::value json_invariant = {block, csts_text.str()};
    invariants_text.push_back(std::move(json_invariant));
  }
}

void json_report_impl::write_parameters(const AnalysisParams &params,
                                        boost::json::object &params_text) {
  params_text["domain"] = params.dom.name().str();
  params_text["inter"].emplace_bool() = params.run_inter;
  params_text["inter.analyze_recursive"].emplace_bool() =
      params.analyze_recursive_functions;
  params_text["inter.max_number_of_summaries"].emplace_uint64() =
      params.max_calling_contexts;
  params_text["inter.exact_summary_reuse"].emplace_bool() =
      params.exact_summary_reuse;
  params_text["backward"].emplace_bool() = params.run_backward;
  params_text["widening_delay"].emplace_uint64() = params.widening_delay;
  params_text["narrowing_iterations"].emplace_uint64() = params.narrowing_iters;
  params_text["widening_jumpset"].emplace_uint64() = params.widening_jumpset;
}

void json_report_impl::write_checks(const checks_db_t &db,
                                    boost::json::object &db_text) {
  db_text["safe"].emplace_uint64() = db.get_total_safe();
  db_text["warning"].emplace_uint64() = db.get_total_warning();
  db_text["error"].emplace_uint64() = db.get_total_error();
}

void json_report_impl::write(cfg_ref_t cfg, const AnalysisParams &params,
                             const abs_dom_map_t &invariants,
                             const checks_db_t &checks) {

  if (!cfg.has_func_decl()) {
    CLAM_ERROR("write_to_json requires a cfg to have a name");
  }

  boost::json::array invariants_array;
  write_invariants(invariants, invariants_array);
  boost::json::object params_obj;
  write_parameters(params, params_obj);
  boost::json::object checks_obj;
  write_checks(checks, checks_obj);
  std::string function_name = cfg.get_func_decl().get_func_name();

  boost::json::object body;
  body["report_date"].emplace_string() = now();
  body["clam_version"].emplace_string() = "TODO";
  body["analysis_time"].emplace_string() = "TODO";
  body["analysis_parameters"] = params_obj;
  body["invariants"] = invariants_array;
  body["checks"] = checks_obj;

  std::string function = cfg.get_func_decl().get_func_name();
  m_report[function] = body;
}

json_report::~json_report() = default;

json_report::json_report() : m_pimpl(std::make_unique<json_report_impl>()) {}

void json_report::write(cfg_ref_t cfg, const AnalysisParams &params,
                        const abs_dom_map_t &invariants,
                        const checks_db_t &checks) {
  m_pimpl->write(cfg, params, invariants, checks);
}

std::string json_report::generate() { return m_pimpl->generate(); }

} // end namespace json
} // end namespace clam
