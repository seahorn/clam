/**
 * clam-diff: compare the JSON outputs of two analyses.
 **/

#include <llvm/Support/CommandLine.h>

#include <clam/CrabDomain.hh>
#include <clam/CrabDomainParser.hh>
#include <clam/Support/Debug.hh>
#include <clam/crab/read_json.hh>

#include <crab/support/os.hpp>
#include <crab/support/debug.hpp>

/* abstract domains for semantic diff*/
#include <crab/config.h>
#include <crab/domains/apron_domains.hpp>
#include <crab/domains/elina_domains.hpp>
#include <crab/domains/intervals.hpp>
#include <crab/domains/split_dbm.hpp>
#include <crab/domains/split_oct.hpp>

llvm::cl::OptionCategory ClamDiffOptCat("clam-diff options");

// Abstract domain used for semantic diff if the two JSON outputs were
// generated with different domains.
static clam::CrabDomain::Type ClamDomain;
llvm::cl::opt<clam::CrabDomain::Type, true, clam::CrabDomainParser> YClamDomain(
    "dom",
    llvm::cl::desc("Crab numerical abstract domain used to compare invariants (if semdiff=true)"),
    llvm::cl::values(
        clEnumValN(clam::CrabDomain::INTERVALS, "int",
                   "Classical interval domain"),
        clEnumValN(clam::CrabDomain::ZONES_SPLIT_DBM, "zones",
                   "Zones domain with DBMs in Split Normal Form (default)"),
        clEnumValN(clam::CrabDomain::OCT_SPLIT_DBM, "soct",
                   "Octagons domain with DBMs in Split Normal Form"),
        clEnumValN(clam::CrabDomain::OCT, "oct",
                   "Octagons domain from Apron or Elina"),
        clEnumValN(clam::CrabDomain::PK, "pk",
                   "Polyhedra domain from Apron or Elina"),
        clEnumValN(clam::CrabDomain::PK_PPLITE, "pk-pplite",
                   "Polyhedra domain from PPLite")),
    llvm::cl::location(ClamDomain),
    llvm::cl::init(clam::CrabDomain::ZONES_SPLIT_DBM),
    llvm::cl::cat(ClamDiffOptCat));

static llvm::cl::opt<std::string> Input1(llvm::cl::Positional,
                                      llvm::cl::desc("<first JSON file>"),
                                      llvm::cl::Required,
                                      llvm::cl::value_desc("filename"),
				      llvm::cl::cat(ClamDiffOptCat));

static llvm::cl::opt<std::string> Input2(llvm::cl::Positional,
                                      llvm::cl::desc("<second JSON file>"),
                                      llvm::cl::Required,
                                      llvm::cl::value_desc("filename"),
				      llvm::cl::cat(ClamDiffOptCat));

static llvm::cl::opt<bool> SemanticDiff(
    "semdiff",
    llvm::cl::desc("Enable semantic diff (true by default)"),
    llvm::cl::init(true),
    llvm::cl::cat(ClamDiffOptCat));

static llvm::cl::opt<unsigned> Verbosity(
    "verbose",
    llvm::cl::desc("Verbosity level"),
    llvm::cl::init(0),
    llvm::cl::cat(ClamDiffOptCat));

struct LogOpt {
  void operator=(const std::string &tag) const {
     crab::CrabEnableLog(tag);
  } 
};

LogOpt Yloc;
static llvm::cl::opt<LogOpt, true, llvm::cl::parser<std::string>> 
LogClOption("log",
             llvm::cl::desc("Enable specified log level"),
             llvm::cl::location(Yloc),
             llvm::cl::value_desc("string"),
             llvm::cl::ValueRequired, llvm::cl::ZeroOrMore,
	     llvm::cl::cat(ClamDiffOptCat));


namespace impl_details {
using interval_domain_t =
    ikos::interval_domain<clam::json::number_t, clam::json::varname_t>;
using zones_domain_t = crab::domains::split_dbm_domain<clam::json::number_t,
                                                       clam::json::varname_t>;
using split_oct_domain_t =
    crab::domains::split_oct_domain<clam::json::number_t,
                                    clam::json::varname_t>;
#ifdef HAVE_APRON
using oct_domain_t =
    crab::domains::apron_domain<clam::json::number_t, clam::json::varname_t,
                                crab::domains::apron_domain_id_t::APRON_OCT>;
using pk_domain_t =
    crab::domains::apron_domain<clam::json::number_t, clam::json::varname_t,
                                crab::domains::apron_domain_id_t::APRON_PK>;
#else
using oct_domain_t =
    crab::domains::elina_domain<clam::json::number_t, clam::json::varname_t,
                                crab::domains::elina_domain_id_t::ELINA_OCT>;
using pk_domain_t =
    crab::domains::elina_domain<clam::json::number_t, clam::json::varname_t,
                                crab::domains::elina_domain_id_t::ELINA_PK>;
#endif
using pk_pplite_domain_t = crab::domains::apron_domain<
    clam::json::number_t, clam::json::varname_t,
    crab::domains::apron_domain_id_t::APRON_PPLITE_POLY>;

using abstract_domain_t = crab::domains::abstract_domain_ref<
    crab::variable<clam::json::number_t, clam::json::varname_t>>;

/**
 * We try to use the same domain used when then invariants were generated.
 *  If we cannot then, we use whatever uses chooses.
 **/
static clam::CrabDomain::Type select_domain(const std::string &dom1,
                                            const std::string &dom2) {

  if (dom1 != dom2) {
    return ClamDomain;
  } else {
    if (dom1 == "int") {
      return clam::CrabDomain::INTERVALS;
    } else if (dom1 == "zones") {
      return clam::CrabDomain::ZONES_SPLIT_DBM;
    } else if (dom1 == "soct") {
      return clam::CrabDomain::OCT_SPLIT_DBM;
    } else if (dom1 == "oct") {
      return clam::CrabDomain::OCT;
    } else if (dom1 == "pk") {
      return clam::CrabDomain::PK;
    } else if (dom1 == "pk-pplite") {
      return clam::CrabDomain::PK_PPLITE;
    } else {
      return ClamDomain;
    }
  }
}

/**
 * Convert a system of linear constraints into an abstract value so that we can
 *compare semantically
 **/
static abstract_domain_t
convert(const clam::json::linear_constraint_system_t &csts,
        clam::CrabDomain::Type dom) {
  switch (dom) {
  case clam::CrabDomain::INTERVALS: {
    interval_domain_t abs_val;
    abs_val += csts;
    return abstract_domain_t(abs_val);
  }
  case clam::CrabDomain::ZONES_SPLIT_DBM: {
    zones_domain_t abs_val;
    abs_val += csts;
    return abstract_domain_t(abs_val);
  }
  case clam::CrabDomain::OCT_SPLIT_DBM: {
    split_oct_domain_t abs_val;
    abs_val += csts;
    return abstract_domain_t(abs_val);
  }
  case clam::CrabDomain::OCT: {
    oct_domain_t abs_val;
    abs_val += csts;
    return abstract_domain_t(abs_val);
  }
  case clam::CrabDomain::PK: {
    pk_domain_t abs_val;
    abs_val += csts;
    return abstract_domain_t(abs_val);
  }
  case clam::CrabDomain::PK_PPLITE: {
    pk_pplite_domain_t abs_val;
    abs_val += csts;
    return abstract_domain_t(abs_val);
  }
  default:
    CLAM_ERROR("clam-diff found unexpected domain " << dom.name());
  }
}

using invariant_map = typename clam::json::analysis_results::invariant_map;
using str_invariant_map = typename clam::json::analysis_results::str_invariant_map;  

struct counters {
  uint64_t equals; 
  uint64_t regressions;
  uint64_t map1_more_precise;
  uint64_t map2_more_precise;
  uint64_t incomparable;
  counters(): equals(0), regressions(0), map1_more_precise(0), map2_more_precise(0), incomparable(0) {}

  void write(crab::crab_string_os &o) {
    o << "Number of equals                     : " << equals << "\n";    
    o << "Number of regressions                : " << regressions << "\n";
    o << "Number of 1 being more precise than 2: " << map1_more_precise << "\n";
    o << "Number of 1 being less precise than 2: " << map2_more_precise << "\n";
    o << "Number of incomparable               : " << incomparable << "\n";
  }
};
  
/** Syntactic diff **/
static void diff_invariants(const std::string &function,
			    const str_invariant_map &map1,
			    const str_invariant_map &map2,
			    crab::crab_string_os &diff,
			    counters &c) {
  if (map1.size() != map2.size()) {
    CLAM_ERROR("clam-diff cannot compare invariant maps of different sizes");
  }
  for (auto &kv : map1) {
    auto const &block = kv.first;
    auto const &inv1 = kv.second;
    auto it = map2.find(block);
    if (it == map2.end()) {
      CLAM_ERROR("clam-diff cannot find block in one of the invariant maps");
    }
    auto const &inv2 = it->second;
    if (inv1 != inv2) {
      diff << "\tRegression found for block " << block << " at function "
           << function << ":\n";
      diff << "\tInvariant #1: " << inv1 << "\n";
      diff << "\tInvariant #2: " << inv2 << "\n";
      c.regressions++;
    } else {
      c.equals++;
    }
  }
}

  
/** Semantic diff using inclusion abstract operation from @domain **/
static void semdiff_invariants(const std::string &function,
			       const invariant_map &map1,
			       const invariant_map &map2,
			       clam::CrabDomain::Type domain,
			       crab::crab_string_os &diff,
			       counters &c) {

  if (map1.size() != map2.size()) {
    CLAM_ERROR("clam-diff cannot compare invariant maps of different sizes");
  }
  for (auto &kv : map1) {
    auto const &block = kv.first;
    std::shared_ptr<clam::json::linear_constraint_system_t> csts1 = kv.second;
    auto it = map2.find(block);
    if (it == map2.end()) {
      CLAM_ERROR("clam-diff cannot find block in one of the invariant maps");
    }
    std::shared_ptr<clam::json::linear_constraint_system_t> csts2 = it->second;

    if (csts1 == csts2) {
      c.equals++;
      continue; // no regression
    }

    abstract_domain_t v1 = convert(*csts1, domain);
    abstract_domain_t v2 = convert(*csts2, domain);
    
    if (v1 <= v2) {
      if (v2 <= v1) {
	c.equals++;
	continue; // no regression
      } else {
	// v1 is more precise
	c.map1_more_precise++;
      }
    } else {
      if (v2 <= v1) {
	// v2 is more precise
	c.map2_more_precise++;
      } else {
	// incomparable
	c.incomparable++;	
      }
    }
	     
    if (Verbosity>0) {
      diff << "\tRegression found for block " << block << " at function "
	   << function << ":\n";
      diff << "\tInvariant #1: " << v1 << "\n";
      diff << "\tInvariant #2: " << v2 << "\n";
    }
    c.regressions++;
  }
}
} // end namespace impl_details

static void print_diff(const clam::json::global_analysis_results &gres1,
                       const clam::json::global_analysis_results &gres2) {

  crab::crab_string_os diff;
  
  diff << "### clam-diff comparing checks ... ###\n";
  uint64_t num_checks = 0;
  for (auto &kv : gres1.m_results) {

    const std::string &function = kv.first;
    const clam::json::analysis_results &res1 = *(kv.second);
    if (!gres2.contains(function)) {
      CLAM_ERROR("clam-diff cannot compare the two files because they have "
                 "different functions");
    }
    const clam::json::analysis_results &res2 = gres2.get(function);

    num_checks += res1.safe_checks;
    num_checks += res1.warning_checks;
    num_checks += res1.error_checks;        
    
    if (res1.safe_checks == res2.safe_checks &&
        res1.warning_checks == res2.warning_checks &&
        res1.error_checks == res2.error_checks) {
    } else {
      diff << "FILE | SAFE CHECKS | WARNING CHECKS | ERROR CHECKS\n";
      diff << Input1 << " | " << res1.safe_checks << " | " << res1.warning_checks
           << " | " << res1.error_checks << "\n";
      diff << Input2 << " | " << res2.safe_checks << " | " << res2.warning_checks
           << " | " << res2.error_checks << "\n";
    }
  }
  if (num_checks == 0) {
    diff <<"No checks found. Enable option --crab-check=assert\n";
  } else {
    diff <<"No regressions found\n";
  }

  diff << "\n### clam-diff comparing invariants ";    
  if (SemanticDiff) {
    diff << "using semantic diff";
  } else {
    diff << "using syntactic diff";
  }
  diff << " ... ###\n";
  
  impl_details::counters c;
  for (auto &kv : gres1.m_results) {
    const std::string &function = kv.first;
    const clam::json::analysis_results &res1 = *(kv.second);
    if (!gres2.contains(function)) {
      CLAM_ERROR("clam-diff cannot compare the two files because they have "
		 "different functions");
    }
    const clam::json::analysis_results &res2 = gres2.get(function);
    if (SemanticDiff) {
      clam::CrabDomain::Type dom =
	impl_details::select_domain(res1.domain, res2.domain);
      impl_details::semdiff_invariants(function, res1.invariants,
				       res2.invariants, dom, diff, c);
    } else {
      impl_details::diff_invariants(function, res1.str_invariants,
				    res2.str_invariants, diff, c);
    } 
  }
  if (c.regressions == 0) {
    diff << "No regressions found\n";
  } else {
    c.write(diff);
  }
  llvm::errs() << diff.str();
}

int main(int argc, char *argv[]) {
  llvm::llvm_shutdown_obj shutdown; // calls llvm_shutdown() on exit
  llvm::cl::HideUnrelatedOptions(ClamDiffOptCat);  
  llvm::cl::ParseCommandLineOptions(
      argc, argv, "Compare the JSON output of two Clam analyses\n" 
      "Generate a JSON file to compare checks:\n"
      "\tclam.py FILE --crab-check=assert -ojson=FILE.json\n"
      "Generate a JSON file to compare invariants:\n"
      "\tclam.py FILE --crab-print-invariants=true --crab-print-invariants-kind=blocks|loops --crab-print-ghost-variables=[true|false] -ojson=FILE.json\n"
      "\tThe option value \"blocks\" won't scale with large programs due to the large number of blocks.\n"
      "If option --crab-inter is enabled then one single JSON files is generated. Otherwise, one JSON file per function.\n");
  clam::json::parse_options clam_opts(SemanticDiff);

  clam::json::invariants_cache_t cache;
  std::unique_ptr<clam::json::global_analysis_results> res1 =
    clam::json::read_json(Input1, clam_opts, &cache);
  if (!res1) {
    return 1;
  }
  std::unique_ptr<clam::json::global_analysis_results> res2 =
    clam::json::read_json(Input2, clam_opts, &cache);
  if (!res2) {
    return 1;
  }
  print_diff(*res1, *res2);
  return 0;
}
