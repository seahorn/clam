// RUN: %clam -O0 --crab-inter --crab-track=mem --crab-dom=int  --crab-check=assert --crab-sanity-checks --lower-unsigned-icmp "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^3  Number of total warning checks$
  
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
  int y;
  switch (nd_int()) {
  case 1:
    y = 2;
    break;
  case 2:
    y = 4;
    break;
  case 3:
    y = 6;
    break;
  default:
    y = 15;
    break;
  }
  t[y] = 100;
  return t[2] + t[4] +  t[6] + t[15];
}

int32_t compute2(const uint32_t t[6][2]) {
  int i2 = nd_int();
  return t[0][1] + t[1][1] + t[5][1];
}

int main () {
  int i;
  for (i=0;i<10;i++) {
    a[i] = (nd_int() ? 3: 5);
  }  
  
  int y = nd_int();
  __CRAB_assume(y >= 0);
  __CRAB_assume(y < 10);
  a[y] = nd_int();   // (1)
  
  int res = a[y];    
  __CRAB_assert(res >= 0); // it's not true because (1)
  __CRAB_assert(res <= 5);

  table1[15] = nd_int();  // (2)
  int z = compute1(table1);
  __CRAB_assert(z  <= 400); // it's not true because of (2)

  int w = compute2(table2);
  __CRAB_assert(w == 90); 

  
  return res+z+w;
}
