#pragma once

/**
 * All header files from the clam abstract domains.
 *
 * No instantiation happens here. The instantiations of the abstract
 * domains occur in the lib/Clam/domains directory.
 *
 * All clam domains consist of a fixed combination of functor and
 * product domains (defined in crab_defs.hh) where the only tunable
 * parameter is the base numerical domain. The names of the domains
 * listed below reflect the name of the base numerical domain:
 *
 * - boxes_domain_t
 * - dis_interval_domain_t
 * - interval_domain_t
 * - num_domain_t
 * - oct_domain_t
 * - pk_domain_t
 * - ric_domain_t
 * - split_dbm_domain_t
 * - term_int_domain_t
 * - term_dis_int_domain_t
 * - wrapped_interval_domain_t
 **/

#include <clam/crab/domains/boxes.hh>
#include <clam/crab/domains/dis_intervals.hh>
#include <clam/crab/domains/intervals.hh>
#include <clam/crab/domains/oct.hh>
#include <clam/crab/domains/pk.hh>
#include <clam/crab/domains/ric.hh>
#include <clam/crab/domains/split_dbm.hh>
#include <clam/crab/domains/terms_dis_intervals.hh>
#include <clam/crab/domains/terms_intervals.hh>
#include <clam/crab/domains/terms_zones.hh>
#include <clam/crab/domains/wrapped_intervals.hh>
