// RUN: %clam -O0  --crab-inter --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

#include "clam/clam.h"

void f(int* p, int* q) {
  *p = 1;
  *q = 2;
}

int main() {
  volatile int x;
  volatile int y;
  f(&x, &y);

  __CRAB_assert(x == 1);
  __CRAB_assert(y == 2);
  return 0;
}
