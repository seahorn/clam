// RUN: %clam -m32 --crab-inter --crab-track=sing-mem  --crab-dom=int --crab-check=assert --crab-sanity-checks  --lower-unsigned-icmp "%s" --crab-widening-jump-set=20  --llvm-pp-loops 2>&1 | OutputCheck %s
// RUN: %clam -m64 --crab-inter --crab-track=sing-mem  --crab-dom=int --crab-check=assert --crab-sanity-checks  --lower-unsigned-icmp "%s" --crab-widening-jump-set=20  --llvm-pp-loops 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$
 
//#include <stdio.h>
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
  devices[i].id = 8; // unsafe
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
