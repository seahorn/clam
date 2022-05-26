// RUN: %clam -m32 --crab-inter --crab-track=sing-mem --crab-dom=int --crab-check=assert --crab-sanity-checks --crab-heap-analysis=cs-sea-dsa-types "%s" 2>&1 | OutputCheck %s
// CHECK: ^4  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

int a[10];

void check(int* s, int n) {
  __CRAB_assert(s[n] >= 0);
  __CRAB_assert(s[n] <= 1);
}

int main(){

  if (nd_int()) 
    a[0] = 1;
  if (nd_int())
    a[1] = 1;
  if (nd_int())  
    a[2] = 1;
  if (nd_int())  
    a[3] = 1;
  if (nd_int())  
    a[4] = 1;
  if (nd_int())
    a[5] = 1;
  if (nd_int())  
    a[6] = 1;
  if (nd_int())  
    a[7] = 1;
  if (nd_int())
    a[8] = 1;
  if (nd_int())  
    a[9] = 1;  

  check(&a[0], 4);
  check(&a[0], 6);  
  
  return 0;
}
