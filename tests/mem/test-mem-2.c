#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0  --crab-inter --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

int* foo(int *p) {
  if (nd_int()) {
    *p = 5;
  } else {
    *p = 10;
  }
  return p;
}

/* The malloc is converted to an alloca the LLVM frontend. 
 * 
 * With -crab-track=sing-mem, Clam infers that *q is between 0 and
 * 10. The lower bound is not 5 because Clam assumes that all allocas
 * are initialized to 0. Then, the first store in foo is already a
 * weak update.
 *
 * With --crab-track=mem Clam can infer that *q is between 5 and 10.
*/
int main() {
  int *p = (int*) malloc(sizeof(int));
  int *q = foo(p);
  __CRAB_assert(*q >= 5);
  return 0;
}
