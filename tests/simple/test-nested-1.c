// RUN: %clam -O0 --crab-dom=int --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck -l debug %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

// simple program with nested loops from Scozzari SAS'13
int main(void) {
  int i,j;
  
  i=0;
  while (i < 10) {
    j=0;
    while (j < 10) {
      j++;
    }
    i++;
  }
  __CRAB_assert(i == 10);
  return 0;
}

