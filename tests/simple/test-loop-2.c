// RUN: %crabllvm -O0 --crab-dom=zones --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

extern int nd(void);
extern void __VERIFIER_error(void) __attribute__((noreturn));
#define assert(X) if(!(X)){__VERIFIER_error();}
extern void __VERIFIER_assume (int v);
#define assume(X) __VERIFIER_assume(X)


int main(){
  int x = 0;
  int y = 1;
  
  while (nd()) {
    int nx = x;
    int ny = y;
    if (nd()) {
      if (nx < 32) {
	nx += 2;
	ny += 2;
      } else {
	nx++;
	break;
      }
    }
    x = nx;
    y = ny;
  }
  assert(x <= y);
  return 0;
}
