// RUN: %clam -O0 --crab-track=mem --crab-inter --crab-print-invariants=false --crab-disable-warnings  "%s" 2>&1 | OutputCheck %s
// XFAIL: *
#include "clam/clam.h"


int* mk_int_ptr_and_init(int val) {
  int* res= (int*)malloc(sizeof(int));
  *res = val;
  return res;
}

int main (){

  int x,y,z, i;
  x=0;
  y=0;
  int* w = mk_int_ptr_and_init(0);
  for (i=0;i< 10;i++) {
    x++;
    y++;
    *w = *w+1;
  }
  CRAB_PRINT_INVARIANTS(x);
  /* it should print something like 
     crab_intrinsic(print_invariants,.01:int32)  // loc(file=none line=-1 col=-1) id=1
          ({}, {.01 -> [10, 10]})
  */
  
  CRAB_PRINT_INVARIANTS(y,*w);

  /* it should print something like 
     crab_intrinsic(print_invariants,.02:int32,_call3:int32) // loc(file=none line=-1 col=-1) id=2 
   	  ({}, {.02 -> [10, 10], _call3 -> [10, 10]})
  */
    
  return 0;
}


