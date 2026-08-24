// RUN: %clam -O0 --taint-config=%S/test6_config.yaml --crab-dom=int --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

#include <stdlib.h>
#include "clam/clam.h"

/* Tag analysis intrinsics. The taint instrumenter only resolves
   pre-existing declarations, and unreferenced extern declarations do
   not survive into LLVM IR, so we keep them alive explicitly. */
typedef uint64_t tag_t;
extern void __CRAB_intrinsic_add_tag(void *, tag_t);
extern void __CRAB_intrinsic_remove_tag(void *, tag_t);
extern void __CRAB_intrinsic_check_does_not_have_tag(void *, tag_t);

void *__attribute__((used)) keep_externals[] = {
    (void *)__CRAB_intrinsic_add_tag,
    (void *)__CRAB_intrinsic_remove_tag,
    (void *)__CRAB_intrinsic_check_does_not_have_tag,
};

/* Policy functions, modeled by test6_config.yaml:
   ext_read is a source, ext_sanitize a filter, ext_write a sink. */
extern void ext_read(int *p);
extern void ext_sanitize(int *p);
extern void ext_write(int v);

/* Program starts here */

int main(int argc, char **argv) {
  int a = 0;
  int b = 0;

  ext_read(&a);
  ext_read(&b);

  ext_sanitize(&a);

  ext_write(a); /* safe: a was sanitized */
  ext_write(b); /* warning: b is still tainted */

  return 0;
}
