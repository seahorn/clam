// RUN: %clam -m32 --crab-inter --crab-track=sing-mem  --crab-dom=int --crab-check=assert --crab-sanity-checks  --lower-unsigned-icmp "%s" 2>&1 | OutputCheck %s
// RUN: %clam -m64 --crab-inter --crab-track=sing-mem  --crab-dom=int --crab-check=assert --crab-sanity-checks  --lower-unsigned-icmp "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
 
#include <stdio.h>
#include "clam/clam.h"

typedef struct {
  char *name;
  char id;
} S1;


S1 devices[4];
extern void avoid_opt(S1 *);

int main(){
  avoid_opt(&devices[0]);
  
  for (unsigned i=0; i<4; ++i) {
    printf("id=%d\n", devices[i].id);
  }
  
  int x = nd_int();
  __CRAB_assume(x >= 0);
  __CRAB_assume(x < 4);
  __CRAB_assert(devices[x].id >= 0);
  __CRAB_assert(devices[x].id <= 4);
  
  return 0;
}
