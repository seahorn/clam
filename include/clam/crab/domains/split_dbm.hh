#pragma once

#include <clam/crab/crab_defs.hh>
#include <crab/config.h>
#include <crab/domains/split_dbm.hpp>

namespace clam {
using namespace crab::domains;
/// To choose DBM parameters
struct BigNumDBMParams {
  /* This version uses unlimited integers so no overflow */
  enum { chrome_dijkstra = 1 };
  enum { widen_restabilize = 1 };
  enum { special_assign = 1 };
  enum { close_bounds_inline = 0 };
  using Wt = ikos::z_number;
  using graph_t = crab::SparseWtGraph<Wt>;
};
struct SafeFastDBMParams {
  /* This version checks for overflow and raise error if detected*/
  enum { chrome_dijkstra = 1 };
  enum { widen_restabilize = 1 };
  enum { special_assign = 1 };
  enum { close_bounds_inline = 0 };
  using Wt = crab::safe_i64;
  using graph_t = crab::AdaptGraph<Wt>;
};
struct FastDBMParams {
  /* This version does not check for overflow */
  enum { chrome_dijkstra = 1 };
  enum { widen_restabilize = 1 };
  enum { special_assign = 1 };
  enum { close_bounds_inline = 0 };
  using Wt = int64_t;
  using graph_t = crab::AdaptGraph<Wt>;
};

#ifdef USE_DBM_BIGNUM
using BASE(split_dbm_domain_t) =
    split_dbm_domain<number_t, dom_varname_t, BigNumDBMParams>;
#else
#ifdef USE_DBM_SAFEINT
using BASE(split_dbm_domain_t) =
    split_dbm_domain<number_t, dom_varname_t, SafeFastDBMParams>;
#else
using BASE(split_dbm_domain_t) =
    split_dbm_domain<number_t, dom_varname_t, FastDBMParams>;
#endif
#endif
using split_dbm_domain_t =
    RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(split_dbm_domain_t))));
} // end namespace clam
