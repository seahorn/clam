#include <stdio.h>

extern void __CRAB_assume(int);
extern int int_nd(void);

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
  int n = int_nd();
  __CRAB_assume(n == 10);
  
  for (i=0;i<n;i++) {
    a[i] = x;
  }
  int j = int_nd();
  __CRAB_assume(j >= 0);
  __CRAB_assume(j < 10);
  int k = bar(a[j]);
  if (k != 0) {
    foo();
  }
  return 0;
}
