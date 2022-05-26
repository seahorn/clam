// RUN: %clam -O0 --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck -l debug %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

// program hybrid from Scozzari SAS'13
int main(void) {
  int i,j;
  
  i=0;
  while (1) {
    i++;
    j=0;
    while (j < 10) {
      __CRAB_assert(i >= 0);
      __CRAB_assert(i <= 10);            
      j++;
    }
    if (i>9) i=0;
  }
  
  return 0;
}

