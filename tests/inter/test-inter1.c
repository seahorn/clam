// RUN: %clam -O0 --crab-inter --crab-inter-recursive-functions --crab-dom=zones --crab-track=mem --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$

#include "clam/clam.h"

// Example of simple recursive function

int foo(int x) {
  if (x > 0) {
    return 1 + foo(x-1);
  } else {
    return x;
  }
}

int main () {

  int x1 = nd_int();
  int y1 = foo(x1);
  __CRAB_assert(y1 == x1);

  int x2 = 1000;
  int y2 = foo(x2);
  __CRAB_assert(y2 == 1000);
  
  return 0;
}
