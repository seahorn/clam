// RUN: %clam -O0 --crab-dom=zones --crab-inter --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-dom=soct --crab-inter --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$
#include "clam/clam.h"

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
      ERROR: __VERIFIER_error();
  }
  return;
}
int main() {
    int i,j;
    i = 1;
    j = 10;
    while (j >= i) {
        i = i + 2;
        j = -1 + j;
    }
    __VERIFIER_assert(j == 6);
    return 0;
}
