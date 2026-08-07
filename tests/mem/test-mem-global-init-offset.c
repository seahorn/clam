// RUN: %clam -m64 --crab-inter --crab-inter-entry-main=true --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-singleton-aliases --crab-dom=int --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK-NOT: CRAB ERROR
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
// XFAIL: *

// XFAIL since the LLVM 16 port.
//
// llvm-seahorn's dev16 branch re-imported LLVM 16's InstCombine and lost the
// AvoidUnsignedICmp guard at the end of foldAndOrOfICmpsUsingRanges. As a
// result sea-instcombine now collapses each pair of signed range comparisons
// below into a single unsigned comparison:
//
//   ; LLVM 15                            ; LLVM 16
//   %cmp2    = icmp slt i32 %1, 7        %narrow = icmp ult i32 %1, 7
//   %cmp.inv = icmp sgt i32 %1, -1
//   %narrow  = and i1 %cmp.inv, %cmp2
//
// Clam translates unsigned comparisons less precisely than signed ones (the
// reason it ships --lower-unsigned-icmp), so both assertions degrade from safe
// to warning. This is a precision regression only: Crab still analyses the
// program without a CRAB ERROR, so the bug this test was written for is not
// back. Restoring the guard in llvm-seahorn should make the test pass again.

#include "clam/clam.h"

extern void sink(const void *);

// Regression test for https://github.com/seahorn/clam/issues/100
//
// The initializer of a global is lowered into Crab by asking the heap
// abstraction for the region of each field, using the field's offset within
// the global. That offset is relative to the global, while a sea-dsa cell
// offset is relative to the *node* the global belongs to. Here `ef` is
// unified with `g_outer` at offset 8, so every field of `ef` used to be
// initialized in the region of the field 8 bytes before it: `ef.k` was stored
// into the region of `g_outer.p`, which is a pointer region, and Crab aborted
// with
//
//   CRAB ERROR: Region::ref_store: type of value 6 (int32) is not compatible
//               with region @V_2 (region(ref))
//
// The last field, `ef.f`, was silently dropped instead, because offset 20 of
// the node holds no value.

struct inner {
  int k;  // offset 0
  long u; // offset 8
  int t;  // offset 16
  int f;  // offset 20
};

struct outer {
  void *p;         // offset 0, a pointer
  struct inner in; // offset 8
};

static const struct inner ef = {6, 0, -1, -1};
static struct outer g_outer;

int main(void) {
  const struct inner *q;

  // Keep `ef` from being folded away by the pre-processor.
  sink(&ef);

  // Unify `ef` with `g_outer` at offset 8.
  if (nd_int()) {
    q = &ef;
  } else {
    q = &g_outer.in;
  }

  // Make offset 0 of the node be accessed as a pointer.
  g_outer.p = (void *)&g_outer;

  // Keep `q` a real pointer, so that the pre-processor cannot fold the two
  // sides of the branch above into a select over already-loaded values.
  sink(q);

  // q is either `ef` or the zero-initialized `g_outer.in`, so the intervals
  // below hold only if every field of `ef` was initialized in its own region.
  __CRAB_assert(q->k >= 0 && q->k <= 6);
  __CRAB_assert(q->f >= -1 && q->f <= 0);
  return 0;
}
