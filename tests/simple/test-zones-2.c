// RUN: %clam -O0 --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

int main (){

  int x,y,i;
  x=0;
  y=0;
  for (i=0;i< 10;i++) {
    x++;
    y++;
  }

  __CRAB_assert(x==y);
  return x+y;
}
