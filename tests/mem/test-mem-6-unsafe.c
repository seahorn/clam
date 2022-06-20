// RUN: %clam -O0  --crab-inter --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

#include "clam/clam.h"

// The first store in f is a strong update and the second is a weak
// update.

// if p and q alias then *p=2 and *q=2
// else *p=1 and *q=2
void f(int* p, int* q) {
  if (nd_int()) {
    p = q;
  }    
  *p = 1;
  *q = 2;
}

int main() {
  volatile int x;
  volatile int y;
  f(&x, &y);

  __CRAB_assert(x >= 1);
  __CRAB_assert(x <= 2);
  __CRAB_assert(y == 2);
  return 0;
}
