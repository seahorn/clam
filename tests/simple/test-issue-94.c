// RUN: %clam --crab-dom=zones --crab-check=assert "%s" 2>&1 | OutputCheck -l debug %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$

extern void __VERIFIER_assume(int);
extern void __CRAB_assert(int);
extern int __VERIFIER_nondet_int(void);

int main() {
  const int MAX = __VERIFIER_nondet_int();
  __VERIFIER_assume(MAX <= 1);
  for (int i = MAX - 1; i >= 0; i--) {
  }
  __CRAB_assert(MAX <= 0);
}
