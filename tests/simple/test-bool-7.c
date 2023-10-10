// RUN: %clam --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

#include "clam/clam.h"

extern int __VERIFIER_nondet_int();

int main() {
  int S = __VERIFIER_nondet_int();
  __CRAB_assume(S != 0 && S <= 1);
  __CRAB_assume(S > 1);
  __CRAB_assert(0);
  __CRAB_assume(S < 2);
  return 0;
}
