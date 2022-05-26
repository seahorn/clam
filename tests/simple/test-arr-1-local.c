// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=sing-mem --crab-heap-analysis=ci-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=sing-mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s

// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
// XFAIL: *

#include "clam/clam.h"

/*
  Even with loop peeling the array_adaptive domain (used by option
  sing-mem) cannot be precise here. Let's assume that we have peeled
  one iteration the loop. Before we start the loop we have 0<= a[0..3]
  <= 5. After one loop iteration, we have 0<= a[0..3] <= 5 and 0<=
  a[4..7] <= 5. After we join, we have 0<=a[0..3]<= 5. In the second
  iteration 1<=i<=2 so we smash the array because the array index i is
  not a constant. The problem arises now. The information we have
  about the array is only 0<=a[0..3]<= 5. However, the domain
  remembers that the cell a[4..7] was created. Since there is no
  information about it, after we smash we lose everything.
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
