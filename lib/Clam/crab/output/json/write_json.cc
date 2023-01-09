#include "write_json.hh"
#include <boost/version.hpp>

#if BOOST_VERSION / 100 % 100 < 80
#include "llvm/Support/raw_ostream.h"
namespace clam {
namespace json {
class json_report_impl {
public:
  void write(cfg_ref_t cfg, const AnalysisParams &params,
             const abs_dom_map_t &invariants, const checks_db_t &checks) {}
  std::string generate() {
    llvm::errs()
      << "Warning: too old version of boost. It needs >= 1.80 for json support\n";
    return "";
  }
};  
} // end namespace json
} // end namespace clam

#else
/** Real implementation starts here **/

#include <boost/json/src.hpp>
#include <clam/Support/Debug.hh>
#include <crab/fixpoint/wto.hpp>
#include <ctime>

namespace clam {

namespace json {

class loop_info : public ikos::wto_component_visitor<cfg_ref_t> {
public:
  using basic_block_label_t = typename cfg_ref_t::basic_block_label_t;
  using basic_block_t = typename cfg_ref_t::basic_block_t;
  using wto_vertex_t = ikos::wto_vertex<cfg_ref_t>;
  using wto_cycle_t = ikos::wto_cycle<cfg_ref_t>;

private:
  std::unordered_set<std::string> &m_loops;

public:
  
  loop_info(std::unordered_set<std::string> &loops) : m_loops(loops) {}
  
  virtual void visit(wto_vertex_t &vertex) override {}
  
  virtual void visit(wto_cycle_t &cycle) override {
    m_loops.insert(cycle.head().get_name());
    for (auto &wto_component : cycle) {
      wto_component.accept(this);
    }
  }
}; 


  
class json_report_impl {
  boost::json::object m_report;

  std::string now();
  void write_invariants(cfg_ref_t cfg, const AnalysisParams &params,
			const abs_dom_map_t &invariants,
                        boost::json::array &invariants_text,
			// whether a block from invariants should be written
			std::function<bool(const std::string&)> shouldBeWritten);
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

/** Invariants can contain other invariants apart from those from cfg **/  
void json_report_impl::write_invariants(cfg_ref_t cfg,
					const AnalysisParams &params,
					const abs_dom_map_t &invariants,
                                        boost::json::array &invariants_text,
					std::function<bool(const std::string&)> shouldBeWritten) {

  auto get_ghost_variables = [&params](const lin_cst_sys_t& csts) -> std::vector<var_t> {
    if (params.keep_shadow_vars) {
      return {}; 
    } else {
      std::vector<var_t> out;
      for (auto const&cst: csts) {
	for (auto v : cst.variables()) {
	  boost::optional<const llvm::Value*> llvm_v = v.name().get();
	  if (llvm_v == boost::none) {
	    out.push_back(v);
	  }
	}
      }
      return out;
    }
  };


    
  for (basic_block_label_t l :llvm::make_range(cfg.label_begin(), cfg.label_end())) {
    if (const llvm::BasicBlock* BB = l.get_basic_block()) {
      auto invIt = invariants.find(BB);
      if (invIt != invariants.end()) {
	std::string block = BB->getName().str();
	if (shouldBeWritten(block)) {
	  // Filtering out ghost variables is really expensive because it's done semantically:
	  // 1. Copy abstract value
	  // 2. Convert to linear constraints system so that we can get all variables
	  // 3. Forget abstract operation
	  // 4. Convert again to linear constraints system.
	  clam_abstract_domain absVal = clam_abstract_domain(invIt->second);
	  auto csts = absVal.to_linear_constraint_system();
	  std::vector<var_t> ghosts = get_ghost_variables(csts);
	  absVal.forget(ghosts);
	  auto filtered_csts = absVal.to_linear_constraint_system();
	  crab::crab_string_os csts_text;
	  filtered_csts.write(csts_text);
	  boost::json::value json_invariant = {block, csts_text.str()};
	  invariants_text.push_back(std::move(json_invariant));
	}
      }
    }
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
  params_text["print_ghost_variables"].emplace_bool() = params.keep_shadow_vars;
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
  switch(params.print_invars) {
  case InvariantPrinterOptions::BLOCKS:
    write_invariants(cfg, params, invariants, invariants_array,
		     [](const std::string&) { return true;});
    break;
  case InvariantPrinterOptions::LOOPS: {
    /* Infer loops from cfg */
    std::unordered_set<std::string> loops; 
    loop_info li(loops);
    ikos::wto<cfg_ref_t> wto_cfg(cfg, cfg.entry());
    wto_cfg.accept(&li);
    write_invariants(cfg, params, invariants, invariants_array,
		     [loops](const std::string&b) { return loops.count(b);});
    break;
  }
  default: /* do nothing */;;
  }
    
  boost::json::object params_obj;
  write_parameters(params, params_obj);
  boost::json::object checks_obj;
  write_checks(checks, checks_obj);
  std::string function_name = cfg.get_func_decl().get_func_name();

  boost::json::object body;
  body["report_date"].emplace_string() = now();
  body["clam_version"].emplace_string() = "TBD";
  body["analysis_time"].emplace_string() = "TBD";
  body["analysis_parameters"] = params_obj;
  body["invariants"] = invariants_array;
  body["checks"] = checks_obj;

  std::string function = cfg.get_func_decl().get_func_name();
  m_report[function] = body;
}
} // end namespace json
} // end namespace clam
#endif

namespace clam {
namespace json {  
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
