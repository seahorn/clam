// RUN: %clam -O0 --inline --lower-unsigned-icmp --crab-dom=int --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck -l debug %s
// RUN: %clam -O0 --inline --crab-lower-unsigned-icmp --crab-dom=int --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck -l debug %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$
#include "clam/clam.h"

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}

/** 
   The fact x is unsigned makes this program hard to verify because it
   requires non-trivial boolean reasoning due to the lowering of
   unsigned < operator and >= to signed operators.
**/

int main(void) {
  unsigned int x = 0;
  while (x < 0x0fffffff) {
    x++;
  }
  __VERIFIER_assert(x > 0x0fffffff);
}
