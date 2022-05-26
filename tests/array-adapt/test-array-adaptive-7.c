// RUN: %clam -O0 --crab-inter --crab-track=sing-mem --crab-dom=int --crab-check=assert --crab-sanity-checks  --lower-unsigned-icmp "%s" 2>&1 | OutputCheck %s
// CHECK: ^5  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

#include <stdint.h>
#include "clam/clam.h"

int x;
int a[10];

int32_t compute1(int32_t *t) {
  int64_t res = t[0] + t[2]; // exact res = 4 but we get res =[2,40]
  int idx = nd_int();
  __CRAB_assume(idx >= 0);
  __CRAB_assume(idx <= 3);
  // A symbolic index makes the pointer analysis to classify t as a
  // "sequence" node and therefore the translation of GEP is
  // imprecise.
  t[idx] = 30;
  return res;
}

int32_t compute2(const uint32_t t[6][2]) {
  // res = 30
  return t[0][1] + t[2][1];
}

int main () {

  int32_t table1[4];
  table1[0] = 1;
  table1[1] = 20;
  table1[2] = 3;

  uint32_t table2[3][2];
  table2[0][1] = 10;
  table2[2][1] = 20;
  
  int i;
  for (i=0;i<10;i++) {
    a[i] = (nd_int() ? 3: 5);
  }  
  
  int y = nd_int();
  __CRAB_assume(y >= 0);
  __CRAB_assume(y < 10);
  int res = a[y];    
  __CRAB_assert(res >= 0); // ok
  __CRAB_assert(res <= 5); // ok

  int z = compute1(table1);
  __CRAB_assert(z <= 40);  // ok

  z = table1[1];
  __CRAB_assert(z <= 30);  // ok
  

  int w = compute2(table2);
  __CRAB_assert(w == 30);  // ok

  
  return res+z+w;
}
