// RUN: %clam -O0 --crab-inter --crab-dom=int --crab-track=mem --promote-malloc=false --crab-check=uaf-legacy --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %clam -O0 --crab-inter --crab-dom=int --crab-track=mem --promote-malloc=false --crab-check=uaf --crab-sanity-checks "%s" 2>&1 | OutputCheck %s

// CHECK: ^0  Number of total warning checks$

#include <stdlib.h>
#include "clam/clam.h"

struct s {
  int f1;
  int f2;
  int f3;
};

void * malloc_x(int sz) {
  void *p = malloc(sz);
  __CRAB_assume(p > 0);
  return p;
}

void free_x(void*p) {
  free(p);
}

void init(struct s* p) {
  p->f1 = nd_int();
  p->f2 = nd_int();
  p->f3 = 5;
}

int main () {
  struct s *p = (struct s*) malloc_x(sizeof(struct s));
  init(p);
  p->f3 = 5;
  free_x(p);    
  return 0;
}
