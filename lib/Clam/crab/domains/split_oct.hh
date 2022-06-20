#pragma once

#include <crab/domains/split_oct.hpp>
#include "crab_defs.hh"

namespace clam {
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
using DBMParams = BigNumDBMParams;
#else
#ifdef USE_DBM_SAFEINT
using DBMParams = SafeFastDBMParams;
#else
using DBMParams = FastDBMParams;
#endif
#endif

using BASE(split_oct_domain_t) =
  crab::domains::split_oct_domain<number_t, region_subdom_varname_t, DBMParams>;
using split_oct_domain_t =
  RGN_FUN(ARRAY_FUN(BOOL_NUM(BASE(split_oct_domain_t))));
} // end namespace clam
