#include <stdio.h>
#include "clam/clam.h"

void fun1(int a) {
  printf("Value of a is %d\n", a);
}
void fun2(int a) {
  printf("Value of a is %d\n", a+1);
}
void fun3(int a) {
  printf("You shoud not see this.\n");
}

void (*fun_ptr)(int) = fun3;

int main() {

   int x = nd_int();
   __CRAB_assume (x >= 0);
   if (x > 0)
     fun_ptr = fun1;
   else
     fun_ptr = fun2;

   if (x < 0) fun_ptr = fun3;
   
   (*fun_ptr)(x);
   return 0;
}
