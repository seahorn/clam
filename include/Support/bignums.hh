#ifndef __BIGNUMS_HH_
#define __BIGNUMS_HH_

/// Extra support for bignums
#include <ikos_domains/bignums.hpp>

namespace llvm_ikos
{
  using namespace ikos;

  std::string toStr (z_number n)
  {
    std::ostringstream s;
    s << n;
    return s.str ();
  }

}

#endif
