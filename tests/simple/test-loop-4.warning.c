// RUN: %clam -O0 --crab-dom=zones --crab-inter --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-dom=oct --crab-inter --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$

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
    int i,j;
    i = 1;
    j = 10;
    while (j >= i) {
        i = i + 2;
        j = -1 + j;
    }
    __VERIFIER_assert(j == 6);
    return 0;
}
