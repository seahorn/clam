// RUN: %clam -O0 --crab-dom=tvpi-dbm --crab-dom-params="fixed_tvpi.coefficients=2" --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

// x grows twice as fast as y, so the loop invariant x=2y has a non-unit
// coefficient. Zones cannot express it (--crab-dom=zones reports a warning
// here); tDBM can, once 2 is in the coefficient template.

#include "clam/clam.h"

int main() {
  int x, y, i;
  x = 0;
  y = 0;
  for (i = 0; i < 10; i++) {
    x += 2;
    y += 1;
  }

  __CRAB_assert(x - 2*y == 0);

  return x + y;
}
