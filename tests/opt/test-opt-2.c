#include <stdio.h>
#include "clam/clam.h"

int x = 0;

void incr_x() {
  x++;
}

void decr_x() {
  x--;
}

int bar(int y) {
  printf("fool llvm\n");
  return y;
}

void foo() {
  printf("this function should not be called\n");
}

int a[10];

int main(int argc, char**argv) {
  incr_x();
  decr_x();

  int i;
  int n = nd_int();
  __CRAB_assume(n == 10);
  
  for (i=0;i<n;i++) {
    a[i] = x;
  }
  int j = nd_int();
  __CRAB_assume(j >= 0);
  __CRAB_assume(j < 10);
  int k = bar(a[j]);
  if (k != 0) {
    foo();
  }
  return 0;
}
