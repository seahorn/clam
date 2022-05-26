// RUN: %clam -O0 --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck -l debug %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

// program hh from Scozzari SAS'13
int main(void) {
  int i,j;
  
  i=0;
  while (i<4) {
    j=0;
    while (j < 4) {
      __CRAB_assert(i <= j+3);
      i++;
      j++;
    }
    i = i-j+1;
  }
  
  return 0;
}

