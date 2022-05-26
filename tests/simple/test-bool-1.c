// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-check=assert "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-dom=int --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"

int main(void) {
  uint8_t x = nd_uint8_t();
  __CRAB_assume(x > 5);
  uint8_t mmax = 10;
  __CRAB_assume(x < mmax);

  
  __CRAB_assert(x >= 3); 
  __CRAB_assert(x <= 9); 
  return 0;
}
