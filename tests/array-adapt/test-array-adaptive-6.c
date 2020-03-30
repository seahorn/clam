// RUN: %clam -O0 --crab-inter --crab-track=arr --crab-disable-array-smashing --crab-dom=int --crab-check=assert --crab-sanity-checks  --lower-unsigned-icmp "%s" 2>&1 | OutputCheck %s
// CHECK: ^4  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

#include <stdint.h>

extern int nd(void);
extern void __CRAB_assert(int);
extern void __CRAB_assume(int);

int x;
int a[10];

int32_t compute1(int32_t *t) {
  int idx = nd();
  __CRAB_assume(idx >= 0);
  __CRAB_assume(idx <= 3);
  // res = 6 but if idx == 3 then there is undefined behavior. 
  return t[idx];
}

int32_t compute2(const uint32_t t[6][2]) {
  // res = 30
  return t[0][1] + t[2][1];
}

int main () {

  int32_t table1[4];
  table1[0] = 1;
  table1[1] = 2;
  table1[2] = 3;

  uint32_t table2[3][2];
  table2[0][1] = 10;
  table2[2][1] = 20;
  
  int i;
  for (i=0;i<10;i++) {
    a[i] = (nd() ? 3: 5);
  }  
  
  int y = nd();
  __CRAB_assume(y >= 0);
  __CRAB_assume(y < 10);
  int res = a[y];    
  __CRAB_assert(res >= 0); 
  __CRAB_assert(res <= 5);

  //table1[3] = 5;
  int z = compute1(table1);
  __CRAB_assert(z  <= 3); 

  int w = compute2(table2);
  __CRAB_assert(w == 30); 

  
  return res+z+w;
}
