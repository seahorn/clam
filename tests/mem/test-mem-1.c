#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0  --crab-inter --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

int main() {
  volatile int *a = (int*) malloc(sizeof(int));
  volatile int *p,*q;

  if (nd_int()) {
    p = a;
  } else {
    q = a;
  }

  if (nd_int()) {
    *p = 5;
  } else {
    *q = 10;
  }

  __CRAB_assert(*a >= 5);
  __CRAB_assert(*a <= 10);  
  return 0;
}
