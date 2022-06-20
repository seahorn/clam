// RUN: %clam -O0 --devirt-functions=types --crab-inter --crab-check=assert --crab-sanity-checks --lower-unsigned-icmp "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

int a (void);
int b (void);

int main(int argc, char** argv) {
  int (*p) (void);
  if (argc == 1)
      p = a;
  else
      p = b;
  int x = p();
  __CRAB_assert(x>=5 && x <=10);
  return 0;
}

int a() {return 10;}
int b() {return 5;}
