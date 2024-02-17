// RUN: %clam --crab-dom=int --crab-lower-unsigned-icmp --crab-check=assert %s 2>&1 | OutputCheck %s
// RUN: %clam --crab-dom=int --lower-unsigned-icmp --crab-check=assert %s 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^2  Number of total warning checks$

extern int __VERIFIER_nondet_int();
extern void __CRAB_assert(int);
int main() {
  int S = __VERIFIER_nondet_int();
  __CRAB_assert(S != 2U && S != 1U);
  __CRAB_assert(S > 2); 
  return 0;
}
