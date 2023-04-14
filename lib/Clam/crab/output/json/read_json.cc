#include <clam/crab/read_json.hh>
#include "crab/support/debug.hpp"

#if BOOST_VERSION / 100 % 100 < 80
#include "llvm/Support/raw_ostream.h"
namespace clam {
namespace json {
std::unique_ptr<global_analysis_results>
read_json(std::string json_filename, clam::json::parse_options &clam_opts,
	  invariants_cache_t* cache) {
  llvm::errs()
    << "Warning: too old version of boost. It needs >= 1.80 for json support\n";
  return nullptr;
}
} // end namespace json
} // end namespace clam
#else
/** Real implementation starts here **/
#include <boost/json/parse.hpp>
#include <clam/Support/Debug.hh>
#include <fstream>
#include <string>

#include "linear_constraints_parser_impl.hpp"
namespace crab {
template clam::json::linear_constraint_t
parse_linear_constraint<clam::json::number_t, clam::json::varname_t,
                        clam::json::variable_factory_t>(
    const std::string &, clam::json::variable_factory_t &, crab::variable_type);

template clam::json::linear_constraint_system_t
parse_linear_constraint_system<clam::json::number_t, clam::json::varname_t,
                               clam::json::variable_factory_t>(
    const std::string &, clam::json::variable_factory_t &, crab::variable_type);
} // end namespace crab

namespace clam {
namespace json {

static clam::json::linear_constraint_system_t
parse_linear_constraint_system(const std::string &csts_text,
                               parse_options &opts) {
  return crab::parse_linear_constraint_system<number_t, varname_t,
                                              variable_factory_t>(
      csts_text, opts.m_vfac, opts.m_var_type);
}

static const boost::json::object &read_as_object(const boost::json::value &v) {
  if (v.is_object()) {
    return v.get_object();
  }
  CLAM_ERROR("cannot read json value as an object");
}

static const boost::json::array &read_as_array(const boost::json::value &v) {
  if (v.is_array()) {
    return v.get_array();
  }
  CLAM_ERROR("cannot read json value as an array");
}

static uint64_t read_as_uint64(const boost::json::value &v) {
  if (v.is_uint64()) {
    return v.get_uint64();
  } else if (v.is_int64()) {
    int64_t x = v.get_int64();
    if (x >= 0) {
      return x;
    }
  }
  CLAM_ERROR("cannot read json value as an uint64_t");
}

static std::string read_as_string(const boost::json::value &v) {
  if (v.is_string()) {
    boost::json::string str = v.get_string();
    std::string out(str.begin(), str.end());
    return out;
  }
  CLAM_ERROR("cannot read json value as an std::string");
}

static std::shared_ptr<linear_constraint_system_t>
lookup(invariants_cache_t *cache, const std::string &invariant_text) {
  if (cache == nullptr) {
    return nullptr;
  } 
  auto it = cache->find(invariant_text);
  if (it != cache->end()) {
    return it->second;
  } else {
    return nullptr;
  }
}
  
std::unique_ptr<global_analysis_results>
read_json(std::string filename, clam::json::parse_options &clam_opts,
	  invariants_cache_t* cache) {

  boost::json::parse_options opts; // all extensions default to off
  opts.allow_comments =
      true; // permit C and C++ style comments to appear in whitespace
  opts.allow_trailing_commas = true; // allow an additional trailing comma in
                                     // object and array element lists
  opts.allow_invalid_utf8 = true; // skip utf-8 validation of keys and strings

  std::ifstream ifs(filename);
  if (!ifs.is_open()) {
    CLAM_ERROR("Cannot open file " << filename);
  }
  
  std::string content((std::istreambuf_iterator<char>(ifs)),
                      (std::istreambuf_iterator<char>()));

  std::unique_ptr<global_analysis_results> out =
      std::make_unique<global_analysis_results>();

  CRAB_LOG("clam-diff", llvm::errs() << "Reading " << filename << "\n";);
  boost::json::value report = parse(content, boost::json::storage_ptr(), opts);
  auto &report_map = read_as_object(report);
  for (auto &kv : report_map) {
    std::string function_name = kv.key();
    const boost::json::value &body = kv.value();
    auto &body_obj = read_as_object(body);
    std::unique_ptr<analysis_results> results =
        std::make_unique<analysis_results>();
    results->function_name = function_name;
    results->date = read_as_string(body_obj.at("report_date"));
    results->clam_version = read_as_string(body_obj.at("clam_version"));
    results->analysis_time = read_as_string(body_obj.at("analysis_time"));
    // Parse abstract domain
    auto &params_obj = read_as_object(body_obj.at("analysis_parameters"));
    results->domain = read_as_string(params_obj.at("domain"));
    // Parse invariants
    auto &invariants_array = read_as_array(body_obj.at("invariants"));
    CRAB_LOG("clam-diff", llvm::errs() << "Found " << invariants_array.size() << " invariants \n";);    
    for (unsigned i = 0, size = invariants_array.size(); i < size; ++i) {
      auto &invariant_tuple = read_as_array(invariants_array[i]);
      std::string block_name = read_as_string(invariant_tuple[0]);
      CRAB_LOG("clam-diff", llvm::errs() << "\t" << block_name;);          
      std::string invariant_text = read_as_string(invariant_tuple[1]);
      if (clam_opts.m_semantic_diff) {
	std::shared_ptr<linear_constraint_system_t> csts = lookup(cache, invariant_text);
	if (csts == nullptr) {
	  csts = std::make_shared<linear_constraint_system_t>(
		     parse_linear_constraint_system(invariant_text, clam_opts));
	  results->invariants[block_name] = csts;
	  if (cache != nullptr) {
	    cache->insert({invariant_text, csts});
	  }
	} else {
	  results->invariants[block_name] = csts;
	  CRAB_LOG("clam-diff", llvm::errs() << " [CACHED]";);
	}
      } else {
	results->str_invariants[block_name] = invariant_text;
      }
      CRAB_LOG("clam-diff", llvm::errs() << "\n";);
    }   
    // Parse checks
    auto &checks_obj = read_as_object(body_obj.at("checks"));
    results->safe_checks = read_as_uint64(checks_obj.at("safe"));
    results->warning_checks = read_as_uint64(checks_obj.at("warning"));
    results->error_checks = read_as_uint64(checks_obj.at("error"));

    out->m_results[function_name] = std::move(results);
  }
  CRAB_LOG("clam-diff", llvm::errs() << "Finished reading " << filename << "\n";);        
  return out;
}

} // end namespace json
} // end namespace clam
#endif 
