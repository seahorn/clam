// RUN: %clam -O0  --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

#include <stdlib.h>
#include "clam/clam.h"

/* seadsa */
extern void sea_dsa_set_read(const void *p);
extern void sea_dsa_set_modified(const void *p);

/* Tag analysis */
typedef uint64_t tag_t;
extern void  __CRAB_intrinsic_add_tag(void *, tag_t);
extern void  __CRAB_intrinsic_remove_tag(void *, tag_t);
extern void  __CRAB_intrinsic_check_does_not_have_tag(void *, tag_t);
#define ADD_TAG(PTR, TAG)	      \
  __CRAB_intrinsic_add_tag(PTR, TAG); \
  sea_dsa_set_modified(PTR);

#define REMOVE_TAG(PTR, TAG)	         \
  __CRAB_intrinsic_remove_tag(PTR, TAG); \
  sea_dsa_set_modified(PTR);

#define CHECK_NOT_TAG(PTR, TAG) \
  __CRAB_intrinsic_check_does_not_have_tag(PTR ,TAG); \
  sea_dsa_set_read(PTR);

/* Program starts here */

/* Sanitizer support: removing a tag from a region that represents a
   single concrete object (a local variable) is a strong update, so
   the check on x is proven safe. y keeps its tag. */

int main(int argc, char**argv) {
  int x = 0;
  int y = 0;

  ADD_TAG(&x, 1);
  ADD_TAG(&y, 1);

  REMOVE_TAG(&x, 1);

  CHECK_NOT_TAG(&x, 1);
  CHECK_NOT_TAG(&y, 1);

  return 0;
}
