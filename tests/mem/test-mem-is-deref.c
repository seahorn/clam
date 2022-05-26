// RUN: %clam -O0 --crab-track=mem --crab-only-cfg --crab-print-cfg=true "%s" 2>&1 | OutputCheck %s
// CHECK: crab_intrinsic
// CHECK: crab_intrinsic

#include <stdbool.h>
#include <stdint.h>
#include "clam/clam.h"

// Check that sea_is_dereferenceable is translated propertly as a Crab
// intrinsics.

extern int __CRAB_intrinsic_foo(int, int);
extern bool __CRAB_intrinsic_is_dereferenceable(const void *ptr, intptr_t offset);

int main (){

  int x,y, i;
  x=0;
  y=0;
  for (i=0;i< 10;i++) {
    x++;
    y++;
    __CRAB_assert(sea_is_dereferenceable(&x, sizeof(int)));
    __CRAB_assert(__CRAB_intrinsic_is_dereferenceable(&y, sizeof(int)));    
  }

  __CRAB_assert(x>=y);
  __CRAB_assert(y>=x);
  return x+y;
}

