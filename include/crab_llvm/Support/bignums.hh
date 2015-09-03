#ifndef __BIGNUMS_HH_
#define __BIGNUMS_HH_

/// Extra support for bignums
#include <crab/common/bignums.hpp>

namespace crab_llvm
{
  using namespace ikos;

  inline std::string toStr (z_number n)
  {
    std::ostringstream s;
    s << n;
    return s.str ();
  }

}

#endif
