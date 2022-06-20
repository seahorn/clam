// RUN: %clam -m32 --crab-inter --crab-track=sing-mem --crab-dom=int --crab-check=assert --crab-sanity-checks --crab-heap-analysis=cs-sea-dsa-types "%s" 2>&1 | OutputCheck %s
// CHECK: ^4  Number of total safe checks$
// CHECK: ^4  Number of total warning checks$
#include "clam/clam.h"

typedef struct {
  int x;
  int y;
  int z[10];
} S1;

#define N 20
S1 a[N];

void check(S1* s1, int flag) {
  if (flag > 0) {
    __CRAB_assert(s1[4].z[4] >= 0);
    __CRAB_assert(s1[4].z[4] <= 1);
    __CRAB_assert(s1[5].x >= 0);
    __CRAB_assert(s1[5].x <= 1);
    
  } else {
    __CRAB_assert(s1[8].z[7] >= 0);
    __CRAB_assert(s1[8].z[7] <= 1);
    __CRAB_assert(s1[6].x >= 0);
    __CRAB_assert(s1[6].x <= 1);
    
  }
}

int main(){
  int i,j;
  for(i=0;i<N;++i) {
    for(j=0;j<10;++j) {
      if (nd_int()) {
	a[i].z[j] = 1;
      }
      if (nd_int()) {
	a[i].x = 1;
      }
      if (nd_int()) {
	a[i].z[j] = 3;
      }      
    }
  }
  
  check(&a[0], nd_int());
  
  return 0;
}
