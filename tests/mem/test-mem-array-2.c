// RUN: %clam -O0 --crab-inter --crab-track=mem --crab-dom=int --crab-check=assert --crab-sanity-checks --lower-unsigned-icmp "%s" 2>&1 | OutputCheck %s
// CHECK: ^4  Number of total safe checks$
// CHECK: ^2  Number of total warning checks$

#include <stdint.h>
#include "clam/clam.h"

int32_t table1[16] = {
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
};

uint32_t table2[6][2] = {
  { 1, 10}, {2, 20}, {3, 30}, {4, 40}, {5, 50}, {6, 60} 
};

int x;
int a[10];

int32_t compute1(int32_t *t) {
  int i1 = nd_int();
  __CRAB_assume(i1 >= 0 && i1 < 16);
  __CRAB_assert(t[i1] >= 0);
  // The pointer analysis cannot tell that the accesses are wrt to the
  // base address of the memory region because the memory region is
  // marked as "sequence" by sea-dsa. Thus, the indexes are not
  // translated precisely to Crab.
  
  // The return value is [4,64]
  return t[2] + t[4] +  t[6] + t[15];
}

int32_t compute2(uint32_t t[6][2]) {
  int i2 = nd_int();
  __CRAB_assume(i2 >= 0 && i2 < 6);
  __CRAB_assert(t[i2][1] <= 100);
  // Same than above.
  
  // The return value is [30, 180]
  return t[0][1] + t[1][1] + t[5][1];
}

int main () {
  int i;
  for (i=0;i<10;i++) {
    a[i] = (nd_int() ? 0: 5);
  }  
  
  int y = nd_int();
  __CRAB_assume(y >= 0);
  __CRAB_assume(y < 10);
  int res = a[y];
  __CRAB_assert(res >= 0);
  __CRAB_assert(res <= 5);

  int z = compute1(table1);
  __CRAB_assert(z  == 31); // cannot be proven

  int w = compute2(table2);
  __CRAB_assert(w == 90);   // cannot be proven
  return res+z+w;
}
