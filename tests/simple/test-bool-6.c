// RUN: %clam --crab-dom=zones --crab-track=mem --inline --crab-heap-analysis=cs-sea-dsa --crab-lower-unsigned-icmp=true --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"

int main() {

  bool b;
  int64_t x = nd_int64_t();
  int64_t y = nd_int64_t();  

  __CRAB_assume(x <= y);
  __CRAB_assume(y >= 0);
  if (x >= 0) {  
    if (y >= 0) {
      b = (x <= y);
    } else {
      b = true;
    }
  } else {
    if (y >= 0) {
      b = false;
    } else {
      b = (x <= y);
    }
  }

  // This assertion is not provable e.g., x=-1 and y=1
  __CRAB_assert(b);
  
  return 0;
} 
