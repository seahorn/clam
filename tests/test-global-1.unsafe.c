// RUN: %crabllvm -O0 --crab-dom=int --crab-check=assert "%s" 2>&1 | OutputCheck %s 
// CHECK: ^1  Number of total warning checks$

extern int __VERIFIER_NONDET();
extern void __VERIFIER_error() __attribute__((noreturn));
extern void __CRAB_assert(int);

int e=0;
int s=2;  

// we should get e=[0,2] and s=[2,5] with thresholds or without applying widening
// we should get e=[0,+oo] and s=[2,5] otherwise

int main () {

  while (__VERIFIER_NONDET()) {
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
  __CRAB_assert (e >= 0 && e <=3);
  __CRAB_assert (s >= 2 && s <=5);
  
  return 42;
}
