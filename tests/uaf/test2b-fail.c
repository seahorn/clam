// RUN: %clam -O0 --crab-inter --crab-dom=int --crab-track=mem --promote-malloc=false --crab-check=uaf-legacy --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-inter --crab-dom=int --crab-track=mem --promote-malloc=false --crab-check=uaf --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total warning checks$

#include <stdlib.h>
#include "clam/clam.h"

struct s {
  int f1;
  int f2;
  int f3;
};


void foo(void *p, int x) {
  if (x > 0) {
    free(p);
  }
}
void bar(void *p, int x) {
  foo (p, x+1);
}

void init(struct s* p) {
  // x is allocated here but it doesn't escape
  int *x = (int*) malloc(sizeof(int));
  if (nd_int()) {
    *x = 5;
  } else {
    *x = 7;
  }
  p->f1 = *x;
  p->f2 = nd_int();
  p->f3 = *x;
  free(x);
}

int main () {
  // a memory block is allocated and passed to callee
  struct s *p = (struct s*) malloc(sizeof(struct s));
  __CRAB_assume(p > 0);  
  init(p);
  // inject bug
  foo(&(p->f2), 1);
  int x = p->f3;
  __CRAB_assert(x >= 5);
  return 0;
}
