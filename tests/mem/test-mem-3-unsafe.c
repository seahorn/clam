#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0  --crab-inter --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

int* foo() {
  int *p = (int*) malloc(sizeof(int));
  if (nd_int()) {
    *p = 4;
  } else {
    *p = 10;
  }
  return p;
}

int main() {
  int *q = foo();
  __CRAB_assert(*q >= 5);
  return 0;
}
