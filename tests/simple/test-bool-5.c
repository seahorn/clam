// RUN: %clam --crab-dom=zones --crab-track=mem --inline --crab-heap-analysis=cs-sea-dsa --lower-unsigned-icmp --crab-lower-with-overflow-intrinsics=true --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

extern uint8_t uint8_t_nd(void);
extern bool bool_nd(void);
extern int int_nd(void);
extern void __CRAB_assert(int);
extern void __CRAB_assume(int);

extern void no_op(size_t, size_t);

size_t size_t_nd(void) {
    int res = int_nd();
    __CRAB_assume(res >= 0);
    return res;
}

int main() {
  
  bool a = bool_nd();
  bool b = bool_nd();
  bool c = bool_nd();

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