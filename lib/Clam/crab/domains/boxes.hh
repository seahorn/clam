#pragma once

#include <crab/domains/array_smashing.hpp>
#include <crab/domains/boxes.hpp>
#include "crab_defs.hh"

namespace clam {
using BASE(boxes_domain_t) = crab::domains::boxes_domain<number_t, varname_t>;
// Boxes can reason natively about booleans so that's why we don't
// combine it with a boolean domain.
//
// TODO: Boxes does not implement rename operation so we cannot wrap
// it in ARRAY_FUN.

using boxes_domain_t = RGN_FUN(array_smashing<BASE(boxes_domain_t)>);
} // end namespace clam
