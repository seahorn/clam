// RUN: %clam -m32 --crab-inter --crab-track=sing-mem --crab-dom=int --crab-check=assert --crab-sanity-checks --crab-heap-analysis=cs-sea-dsa-types "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^8  Number of total warning checks$
#include "clam/clam.h"

#define N 20

typedef struct {
  int x;
  char y; // <-- make everything collapses
  int z[10];
} S1;

S1 a[N];

void foo(S1* s1, int flag) {
  if (flag > 0) {
    s1[4].z[4] = 2;
    s1[5].x = 2;
  } else {
    s1[8].z[7] = 2;
    s1[6].x  = 2;
  }
}

void check(S1* s1, int flag) {
  if (flag > 0) {
    __CRAB_assert(s1[4].z[4] >= 0);
    __CRAB_assert(s1[4].z[4] <= 2);
    __CRAB_assert(s1[5].x >= 0);
    __CRAB_assert(s1[5].x <= 2);
    
  } else {
    __CRAB_assert(s1[8].z[7] >= 0);
    __CRAB_assert(s1[8].z[7] <= 2);
    __CRAB_assert(s1[6].x >= 0);
    __CRAB_assert(s1[6].x <= 2);
    
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
    }
  }
  foo(&a[0], nd_int());  
  check(&a[0], nd_int());
  
  return 0;
}
