#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0  --crab-inter --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

/*
 *  The malloc cannot be converted to an alloca by the frontend.
 *
 *  With --crab-track=sing-mem, Clam ignores dynamically allocated memory.
 *
 *  With --crab-track=mem, Clam can prove the property.
 * 
 */
int* foo() {
  int *p = (int*) malloc(sizeof(int));
  if (nd_int()) {
    *p = 5;
  } else {
    *p = 10;
  }
  return p;
}

int main() {
  int *q = foo();
  __CRAB_assert(*q >= 5);
  return 0;
}
