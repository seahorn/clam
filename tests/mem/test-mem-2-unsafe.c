#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0  --crab-inter --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

int* foo(int *p) {
  if (nd_int()) {
    *p = 5;
  } else {
    *p = 10;
  }
  return p;
}

int main() {
  int *p = (int*) malloc(sizeof(int));
  int *q = foo(p);
  __CRAB_assert(*q >= 6);
  return 0;
}
