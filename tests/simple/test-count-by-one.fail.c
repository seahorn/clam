// RUN: %clam -O0 --crab-dom=zones --crab-check=assert --inline --crab-widening-jump-set=20 --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-dom=oct --crab-check=assert --inline --crab-widening-jump-set=20 --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-dom=pk --crab-check=assert --inline --crab-widening-jump-set=20 --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
// XFAIL: *


/* It requires widening with thresholds */

extern void __VERIFIER_error(void);
extern void __VERIFIER_assume(int);
void __VERIFIER_assert(int cond) {
  if (!(cond)) {
  ERROR: __VERIFIER_error();
  }
  return;
}
int __VERIFIER_nondet_int();
int main() {
    int i;
    for (i = 0; i != 1000000; i++) {
 __VERIFIER_assert(i <= 1000000);
    }
    return 0;
}
