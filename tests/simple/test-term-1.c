// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=term-dis-int --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-dom=term-dis-int --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

int main()
{
  int u,y,z,x;

  if (nd_int ())
    u = 0;
  else 
    u = 10;

  if (nd_int ())
    y = 0;
  else 
    y = 10;

  if (nd_int ())
    z = 0;
  else 
    z = 10;

  if (nd_int ())
    x = u+y;
  else 
    x = u+z;

  if (x < 3)
    u = u + 3;
  else
    u = 3;

  //__CRAB_assert(u >= 3 && u <= 13);
  
  __CRAB_assert(u >= 3 && u <= 5);
  if (nd_int ())
    return 42;
  else 
    return u;
}
