// RUN: %clam -O0 --crab-inter --crab-inter-recursive-functions --crab-dom=int --crab-track=mem --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// XFAIL: *

#include "clam/clam.h"

// We need to statically or dynamically peel the recursive function
// one iteration to keep the invariant that forall i :: a[i] == 5.

/* Initialize an array */
void rec_init(int* a, int n) {
  if (n == 0) {
    a[0] = 5;
    return;
  }
  else {
    __CRAB_assert(n > 0);
    a[n] = 5;
    rec_init(a, n-1);
  }
}

int main () {
  int a[10];
  rec_init(&a[0], 9);

  int i = nd_int();
  __CRAB_assume(i >=0);
  __CRAB_assume(i < 10);
  int res = a[i-1];
  __CRAB_assert(res == 5);
  return res;
}
