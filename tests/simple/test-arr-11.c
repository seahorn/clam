// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=sing-mem --crab-heap-analysis=ci-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=sing-mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-dom=int --crab-track=sing-mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

typedef struct {
  int x;
  int y;  
} gstate;

gstate g;
int a[10];
const char* str = "hello world";

int main ()
{
  g.x = 5;
  g.y = 3;
  int i;
  int *p =&g.x;
  *p= *p + 1;
  for (i=0;i<10;i++)
  {
    if (nd_int())
      a[i] =g.y;
    else 
      a[i] =g.x;
  }
  g.y++;
  int res = a[i-1];
  __CRAB_assert(res >= 0 && res <= 6);
  return res + str[0];
}
