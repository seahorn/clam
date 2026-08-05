#include "clam/clam.h"

/* c/loops/veris.c_NetBSD-libc__loop_true-unreach-call_true-termination.i */

// RUN: %clam -O0 --lower-select --crab-inter --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks --inline "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
// XFAIL: *

// XFAIL since the LLVM 15 port: https://github.com/seahorn/clam/issues/106
//
// Under opaque pointers, sea-instcombine's transformToIndexedCompare rewrites
// this loop's pointer induction variable into an integer index IV plus a scaled
// GEP, and hoists that GEP into the loop header. Zones cannot represent
// `p = pathbuf + 4*i`, so the guard `i <= 1` no longer bounds `p` by `bound`,
// and `p <= tmp` becomes a warning. The issue has the full diagnosis.

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}
typedef int Char;


Char *tmp;

int glob2 (Char *pathbuf, Char *pathlim)
{
  Char *p;

  for (p = pathbuf; p <= pathlim; p++) {

    __VERIFIER_assert(p<=tmp);
    *p = 1;
  }

  return 0;
}

int main ()
{
  Char pathbuf[1 +1];

  Char *bound = pathbuf + sizeof(pathbuf)/sizeof(*pathbuf) - 1;

  tmp = pathbuf + sizeof(pathbuf)/sizeof(*pathbuf) - 1;

  glob2 (pathbuf, bound);

  return 0;
}
