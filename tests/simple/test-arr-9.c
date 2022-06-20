// RUN: %clam -O0 --lower-unsigned-icmp --crab-inter --crab-dom=zones --crab-track=sing-mem --crab-heap-analysis=ci-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --lower-unsigned-icmp --crab-inter --crab-dom=zones --crab-track=sing-mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-inter --crab-dom=zones --crab-track=sing-mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks
// CHECK: ^1  Number of total warning checks$
#include "clam/clam.h"

/** 
    Current array domain cannot handle this case but we at least check
    that all sanity checks pass.
**/


int* foo(int sz, int val) {
  int* res = (int*) malloc (sz* sizeof(int));
  int i;
  for (i=0;i<sz;i++) {
    res[i] = val;
  }
  return res;
}

#define N 10

int main ()
{
  int *a = foo(N, 5);
  int b[N];
  int i;
  for (i=0;i<N;i++) {
    b[i] = a[i];
  }

  int j = nd_int();
  __VERIFIER_assume(j >= 0 && j < N);
  int x = b[j];

  __CRAB_assert(x == 5);
  
  /* if (x != 5) { */
  /*   __VERIFIER_error (); */
  /* } */
  
  return 42;
  /* if (x == 0) */
  /*   return 42; */
  /* else */
  /*   return 0; */
}
