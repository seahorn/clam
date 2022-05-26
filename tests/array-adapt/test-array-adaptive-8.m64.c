// RUN: %clam -m64 --crab-inter --crab-track=sing-mem --crab-dom=int --crab-check=assert --crab-sanity-checks  --lower-unsigned-icmp --crab-widening-jump-set=20  --llvm-pp-loops "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
// XFAIL: *

// With a relational domain we should be able to prove the property.
// The problem is that after some LLVM optimizations we lose precision
// after widening the variable "i" in the for loop in main.

#include "clam/clam.h"

extern char* nd_string(void);

typedef struct {
  char *name;
  char id;
} S1;


void foo(S1 *devices, int len) {
  int i = nd_int();
  __CRAB_assume(i >= 0);
  __CRAB_assume(i < len);
  devices[i].id = 0; 
  devices[i].name = nd_string();
}

S1 devices[4];
extern void avoid_opt(S1 *);

int main(){
  avoid_opt(&devices[0]);

  for (unsigned i=0; i<4; ++i) {
    devices[i].id = i;
    devices[i].name = nd_string();
  }

  foo(&devices[0], 4);
  
   
  int x = nd_int();
  __CRAB_assume(x >= 0);
  __CRAB_assume(x < 4);
  __CRAB_assert(devices[x].id >= 0);
  __CRAB_assert(devices[x].id <= 4);
  
  return 0;
}
