// RUN: %clam -O0 --crab-inter --crab-dom=zones --crab-track=mem --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$

#include "clam/clam.h"
#include <stdlib.h>

struct object {
  int x;
  int y;
  int z;
};

static int pos;		/* The offset of the current argument in ARGV. */
static int argc;	/* The number of arguments present in ARGV. */
static struct object *global;	/* A global object pointer */

static void pass_partial_flds()
// (@V_21,@V_23) declare advance(@V_9:ref,@V_12:ref,@V_15:ref,
//                                   @V_17,@V_18:region(ref),@V_19,@V_20,@V_22)
{
  // region_copy(@V_16, @V_17);
  // region_copy(@V_10:region(ref), @V_18:region(ref));
  // region_copy(@V_13, @V_19);
  // region_copy(@V_21, @V_20);
  // region_copy(@V_23, @V_22);
  // crab_intrinsic(dsa,@V_21,@V_23);
  int j = pos;

  global->y = global->y + 1; // update global object

  global->z = argc + j;
  __CRAB_assert(global->z >= 6);

}

int main() {
  // crab_intrinsic(dsa,@V_5,@V_6,@V_7);
  int a = nd_int();
  __CRAB_assume(a == 5);
  int b = nd_int();
  __CRAB_assume(b == 2);
  int c = nd_int();
  __CRAB_assume(c == 3);
  pos = nd_int(); argc = nd_int();
  __CRAB_assume(pos == 3);
  // assume(_store == 3);
  // store_to_ref(@V_2,pos:ref,_store:int32);
  // pos: @V_2
  __CRAB_assume(argc >= 3);
  // assume(_store3 >= 3);
  // store_to_ref(@V_3,argc:ref,_store3:int32);
  // argc: @V_3
  global = (struct object *)malloc(sizeof(struct object));
  // malloc1 := make_ref(@V_5,12,as_3);
  // store_to_ref(@V_4:region(ref),global:ref,malloc1:ref);
  global->x = a;
  global->y = b;
  global->z = c;
  // global: @V_5 = 5; @V_6 = 2; @V_7 = 3;
  pass_partial_flds();
  // (@V_6,@V_7)= call advance(True:bool,global:ref,pos:ref,argc:ref,
  //                                @V_2,@V_4:region(ref),@V_3,@V_6,@V_7);
  __CRAB_assert(global->x == a);
  return 0;
}
