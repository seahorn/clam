// RUN: %clam -O0 --crab-dom=w-int --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

int main() {
  char x,y;
  
  y=-10;
  if(nd_int()) x=0;
  else  x=100;
  while (x >= y){   
    x = x-y;        
  }                 
  __CRAB_assert(x >= -128 && x <= -119); 
  return 0;
}
