// RUN: %clam -O0 --crab-inter --crab-dom=zones --crab-track=mem --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$

#include "clam/clam.h"
#include "seadsa/sea_dsa.h"
#include <stdlib.h>

struct object {
  int x;
  int y;
};

struct object *mult_obj(struct object *obj, int k) {
  obj->x = k;
  obj->y = k;
  struct object *ret = (struct object *)malloc(sizeof(struct object));
  ret->x = k;
  ret->y = k;
  return ret;
}

int main() {
  int a = nd_int();
  __CRAB_assume(a == 3);
  struct object* obj = (struct object *)malloc(sizeof(struct object));
  obj->x = 4;
  obj->y = 5;
  struct object * ret = mult_obj(obj, a);
  sea_dsa_alias(ret, obj);
  __CRAB_assert(ret->x == 3);

  return 0;
}
