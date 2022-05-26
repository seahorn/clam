// RUN: %clam -O0 --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

#define assert(X) if(!(X)){__VERIFIER_error();}

int main(){
  int x = 0;
  int y = 1;
  
  while (nd_int()) {
    int nx = x;
    int ny = y;
    if (nd_int()) {
      if (nx < 32) {
	nx += 2;
	ny += 2;
      } else {
	nx++;
	break;
      }
    }
    x = nx;
    y = ny;
  }
  assert(x <= y);
  return 0;
}
