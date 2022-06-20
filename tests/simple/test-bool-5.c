// RUN: %clam --crab-dom=zones --crab-track=mem --inline --crab-heap-analysis=cs-sea-dsa --lower-unsigned-icmp --crab-lower-with-overflow-intrinsics=true --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"

size_t size_t_nd(void) {
    int res = nd_int();
    __CRAB_assume(res >= 0);
    return res;
}

int main() {

  bool a = nd_bool();
  bool b = nd_bool();
  bool c = nd_bool();

  __CRAB_assume(a && b); // translate into single assume

  bool d = a && c; // should not translate

  if (d) {
    __CRAB_assert(b);
  } else {
    __CRAB_assume(d);
  }

  __CRAB_assert(d);

  return 0;
} 
