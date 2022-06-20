// RUN: %clam -O0 --devirt-functions=sea-dsa --crab-inter --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

#include "clam/clam.h"

int a (void);
int b (void);
int c (void);
int d (void);

int main(int argc, char** argv) {
  int (*p) (void);
  int (*q) (void);  
  
  if (argc == 1) {
      p = a;
      q = c;
  } else {
      p = b;
      q = d;
  }

  int x = p();
  int y = q();

  __CRAB_assert(x>= 5);
  __CRAB_assert(y>= 15);    
  return 0;
}

int a() {return 10;}
int b() {return 5;}
int c() {return 15;}
int d() {return 20;}
