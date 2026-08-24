// RUN: %clam -O0  --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
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

/* Sanitizer support, weak-update case: the region summarizes several
   objects allocated at the same site, so removing the tag must be a
   no-op (the sanitizer only cleans the object p points to, not every
   object in the region). The check must remain a warning. */

int main(int argc, char**argv) {
  int *p = 0;
  int n = nd_int();
  __CRAB_assume(n > 1);

  for (int i = 0; i < n; i++) {
    p = (int*) malloc(sizeof(int));
    *p = 0;
  }

  ADD_TAG(p, 1);
  REMOVE_TAG(p, 1);
  CHECK_NOT_TAG(p, 1);

  return 0;
}
