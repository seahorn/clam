// RUN: %clam -O0 --crab-inter --crab-inter-recursive-functions --crab-dom=zones --crab-track=mem --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// XFAIL: *

#include "clam/clam.h"

// Example of simple recursive function
int foo(int i, int n) {
  if (i < n) {
    return 1 + foo(i+1,n);
  } 
  return 0;
}

int main () {

  int n1 = nd_int();
  __CRAB_assume(n1 > 0);
  int y1 = foo(0, n1);
  __CRAB_assert(y1 <= n1);
  __CRAB_assert(y1 >= n1);
  return 0;
}
