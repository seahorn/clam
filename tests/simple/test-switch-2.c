// RUN: %crabllvm -O0 --disable-lower-switch --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$

// We disable the LLVM pass lower-switch to test how a switch
// instruction is translated to Crab.

extern int nd(void);
extern void __CRAB_assert(int);

int main () {
  int x = 7;
  int y = 8;
  int v = nd();
  switch (v) {
  case 1:
    x = 0;
    y = 1;
    break;
  case 2:
    x = 1;
    y = 2;
    break;
  case 3:
    x = 2;
    y = 3;
    break;
  default:
    x = 5;
    y = 6;
  }

  __CRAB_assert(x >= 0 && x <= 7); // proved by preprocessor
  __CRAB_assert(y >= 1 && y <= 8); // proved by preprocessor
  __CRAB_assert(y >= x+1);
  
  return 0;
}
