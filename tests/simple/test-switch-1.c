// RUN: %clam -O0 --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

int main () {
  int x = 7;
  int y = 8;
  int v = nd_int();
  switch (v) {
  case 1:
    x = 0;
    y = 1;
    break;
  case 2:
    x = 1;
    y = 2;
    break;
  case 3:
    x = 2;
    y = 3;
    break;
  default:
    x = 5;
    y = 6;
  }

  __CRAB_assert(x >= 0 && x <= 7); // proved by preprocessor
  __CRAB_assert(y >= 1 && y <= 8); // proved by preprocessor
  __CRAB_assert(y >= x+1);
  
  return 0;
}
