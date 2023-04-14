// RUN: %clam -O0 --crab-inter --crab-dom=int --crab-track=mem --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^3  Number of total safe checks$
// XFAIL: *

#include "clam/clam.h"
#include <stdlib.h>

/**
 * The current memory domain cannot prove the properties because of
 * its limited reasoning about strong updates
 **/
struct object {
  int x;
  int y;
  int z;
};

void pass_object(struct object *obj) {
  // (@V_3,@V_6,@V_8) declare pass_object(@V_2:ref,@V_4,@V_5,@V_7)
  // region_copy(@V_3, @V_4);
  // region_copy(@V_6, @V_5);
  // region_copy(@V_8, @V_7);
  // crab_intrinsic(dsa,@V_3,@V_6,@V_8);
  obj->x += 1;
  obj->y += 1;
  obj->z += 1;
}

int main() {
  // crab_intrinsic(dsa,@V_10,@V_11,@V_12);
  // crab_intrinsic(dsa,@V_13,@V_14,@V_15);
  // crab_intrinsic(dsa,@V_16,@V_17,@V_18);
  int a = nd_int();
  __CRAB_assume(a == 5);
  int b = nd_int();
  __CRAB_assume(b == 2);
  int c = nd_int();
  __CRAB_assume(c == 3);
  struct object *o1 = (struct object *)malloc(sizeof(struct object));
  // o1: @V_10,@V_11,@V_12
  o1->x = a; o1->y = a; o1->z = a;

  struct object *o2 = (struct object *)malloc(sizeof(struct object));
  o2->x = b; o2->y = b; o2->z = b;

  struct object *o3 = (struct object *)malloc(sizeof(struct object));
  o3->x = c; o3->y = c; o3->z = c;

  // (@V_13,@V_14,@V_15) = call pass_object(o2:ref,@V_13,@V_14,@V_15)
  pass_object(o2);
  __CRAB_assert(o2->x == b + 1);

  // (@V_16,@V_17,@V_18) = call pass_object(o3:ref,@V_16,@V_17,@V_18)
  pass_object(o3);
  __CRAB_assert(o3->x == c + 1);

  pass_object(o1);
  // (@V_10,@V_11,@V_12) = call pass_object(o1:ref,@V_10,@V_11,@V_12);
  __CRAB_assert(o1->x == a + 1);


  return 0;
}
