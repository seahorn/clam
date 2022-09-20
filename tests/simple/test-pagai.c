// RUN: %clam -O0 --crab-dom=boxes --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck -l debug %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
// XFAIL: *
#include "clam/clam.h"

int main () {
  int x =  0 ;
  int t =  0 ;
  int phase = 0 ;
  while ( t < 100) {
    if (phase == 0)
      x = x +2;
    if (phase == 1)
      x = x - 1;
    
    phase = 1 - phase ;
    t++;
  }
  __CRAB_assert ( x <= 100);
  return 0;
}
