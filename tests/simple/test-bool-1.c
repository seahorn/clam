// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-check=assert "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-dom=int --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

extern uint8_t uint8_t_nd(void);
extern void __CRAB_assert(int);
extern void __CRAB_assume(int);
int main(void) {
  uint8_t x = uint8_t_nd();
  __CRAB_assume(x > 5);
  uint8_t mmax = 10;
  __CRAB_assume(x < mmax);

  
  __CRAB_assert(x >= 3); // EXPECTED OK
  __CRAB_assert(x <= 8); // EXPECTED OK BUT CANNOT BE PROVEN
  return 0;
}
