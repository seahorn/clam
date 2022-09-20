// RUN: %clam -O0 --crab-inter --crab-track=mem --crab-dom=zones --crab-check=assert --crab-sanity-checks --crab-dom-params="region.is_dereferenceable=true" --crab-widening-delay=2  "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
// XFAIL: *


// Note that we need to delay widening two iterations to prove the
// assertion.

// After Crab commit cb2d8c57: we cannot prove the assertion because
// the transfer function for p != q is not precise enough.


#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"

int main() {
  int sz = nd_int();
  __CRAB_assume(sz > 0);
  __CRAB_assume(sz < 10);
  uint8_t *start = malloc(sizeof(uint8_t) * sz);
  uint8_t *end = start + sz;
  uint8_t *i = start;
  while (i != end) {
    __CRAB_assert(sea_is_dereferenceable(i, 1));
    i++;
  }
  
  return 0;
}
