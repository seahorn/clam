// RUN: %clam -O0 --crab-dom=tvpi-dbm --crab-dom-params="fixed_tvpi.coefficients=2,3" --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

// Each iteration adds a non-deterministic 2 or 3 to x, so x is bounded by
// 2N and 3N: both bounds need the coefficients 2 and 3 in the template.

#include "clam/clam.h"

int main() {
  int i, x;
  int N = nd_int();
  __CRAB_assume(N > 0);
  i = 0;
  x = 0;
  while (i < N) {
    i++;
    if (nd_int()) {
      x = x + 2;
    } else {
      x = x + 3;
    }
  }

  __CRAB_assert(x >= 2*N);
  __CRAB_assert(x <= 3*N);

  return x;
}
