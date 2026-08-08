// RUN: %clam -O0 --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

// Crab must relate x and y across the join of a lowered switch.
//
// The case bodies are offsets of a nondeterministic k rather than constants:
// with constants the preprocessor discharges the assertion itself (LLVM 18
// folds it to __CRAB_assert(1)), leaving crab with nothing to check and this
// test asserting nothing. Keeping the values symbolic puts the assertion back
// in front of crab, which is what this test is about; the switch still reaches
// CrabIR fully lowered.
int main () {
  int k = nd_int();
  int x = k+7;
  int y = k+8;
  int v = nd_int();
  switch (v) {
  case 1:
    x = k;
    y = k+1;
    break;
  case 2:
    x = k+1;
    y = k+2;
    break;
  case 3:
    x = k+2;
    y = k+3;
    break;
  default:
    x = k+5;
    y = k+6;
  }

  __CRAB_assert(y >= x+1);

  return 0;
}
