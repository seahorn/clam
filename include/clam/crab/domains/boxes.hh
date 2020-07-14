#pragma once

#include <clam/crab/crab_defs.hh>

#include <crab/config.h>
#include <crab/domains/array_smashing.hpp>
#include <crab/domains/boxes.hpp>

namespace clam {
using namespace crab::domains;
using BASE(boxes_domain_t) = boxes_domain<number_t, varname_t>;
// Boxes can reason natively about booleans so that's why we don't
// combine it with a boolean domain.
// 
// TODO: Boxes does not implement rename operation so we cannot wrap
// it in REF_FUN or ARRAY_FUN.
using boxes_domain_t = array_smashing<BASE(boxes_domain_t)>;
} // end namespace clam
