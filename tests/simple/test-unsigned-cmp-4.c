// RUN: clang -O1 -c -emit-llvm %s -o %s.bc
// RUN: %clam --no-preprocess --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-lower-unsigned-icmp=true --crab-check=assert %s.bc 2>&1 | OutputCheck %s
// RUN: %clam --no-preprocess --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --lower-unsigned-icmp --crab-check=assert %s.bc 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$

extern void __VERIFIER_assume(int cond);
extern int __VERIFIER_nondet_int(void);
extern void __CRAB_assert(int cond);

int main(void) {

  int x = __VERIFIER_nondet_int();
  int y = __VERIFIER_nondet_int();  
  __VERIFIER_assume(x < 8561 && x> -2000);
  __VERIFIER_assume(x == 6000);  
  __CRAB_assert(0); // EXPECTED FALSE
  return 0;
}
