#include <stdio.h>

// RUN: %clam  -O0 --crab-inter --crab-inter-recursive-functions --crab-dom=int --crab-track=mem --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
  
extern void __CRAB_assert(int);
extern void __CRAB_assume(int);
extern int int_nd(void);

extern int nd();

void fun1(int a) {
  printf("Value of a is %d\n", a);
}
void fun2(int a) {
  printf("Value of a is %d\n", a+1);
}
void fun3(int a) {
  printf("Value of a is %d\n", a+2);
}

void (*fun_ptr)(int) = fun3;

int main() {

   int x = nd();
   __CRAB_assume (x >= 0);
   if (x > 0)
     fun_ptr = fun1;
   else
     fun_ptr = fun2;

   if (x < 0) fun_ptr = fun3;
   
   __CRAB_assert(fun_ptr != fun3);
   
   (*fun_ptr)(x);
   return 0;
}
