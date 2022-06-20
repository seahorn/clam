// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=mem --llvm-peel-loops=1 --crab-heap-analysis=ci-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=mem --llvm-peel-loops=1 --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s

// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

#include "clam/clam.h"

/*
  With option crab-track=sing-mem we cannot prove this program.  The
  reason is that this option sing-mem uses array-adaptive while mem
  uses the region domain. They both smash arrays sooner or later but
  that makes a difference. Nevertheless, we need loop peeling=1

  See tests/simple/test-arr-1-local.c  for more details.
 */

int main () {
  // local array
  int a[10];
  int i;
  for (i=0;i<10;i++) {
    if (nd_int())
      a[i] =0;
    else 
      a[i] =5;
  }

  int res = a[i-1];
  __CRAB_assert(res >= 0 && res <= 5);
  return res;
}
