// RUN: %clam -O0 --lower-unsigned-icmp --crab-dom=int --crab-widening-delay=3 --crab-check=assert --inline --crab-sanity-checks --crab-track=sing-mem --crab-singleton-aliases "%s"  2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-dom=int --crab-widening-delay=3 --crab-check=assert --inline --crab-sanity-checks --crab-track=sing-mem --crab-singleton-aliases "%s"  2>&1 | OutputCheck %s
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
#include "clam/clam.h"

int e=0;
int s=2;  

// we should get e=[0,2] and s=[2,5] without applying widening 

void foo() {

  while (nd_int()) {
    if (s == 2){
      if (e ==0) e=1;
      s = 3;
    }
    else if (s == 3){
      if (e ==1) e=2;
      s=4;
    }
    else if (s == 4){
      if (e == 3) {
         __VERIFIER_error();
      }
      s=5;
    }
  }

  __CRAB_assert (e >= 0 && e <=2);
  __CRAB_assert (s >= 2 && s <=5); 
}


int main(){
  foo();
  return 42;
}
