#include "clam/clam.h"

// The "types" resolver groups candidate callees into alias sets keyed by the
// function signature: add_one and add_two share one, two_args does not. The
// indirect call must therefore be promoted to add_one and add_two only.
//
// Under LLVM 15 opaque pointers the signature cannot be recovered from the
// type of the called operand any more -- it is just `ptr`. A resolver that
// keys on that type puts every address-taken function in a single alias set,
// and two_args would be promoted here too.

int add_one(int x) { return x + 1; }
int add_two(int x) { return x + 2; }
int two_args(int x, int y) { return x + y; }

// volatile so that neither the stores nor the load below are folded away:
// the callee has to stay opaque to LLVM, otherwise it re-folds whatever the
// devirtualization pass emitted and the promoted callees cannot be observed.
int (*volatile int_fn)(int);
int (*volatile int2_fn)(int, int);

int main() {
  int x = nd_int();
  __CRAB_assume(x >= 0);

  // Address-taken, so a candidate callee; only its signature keeps it out of
  // int_fn's alias set.
  int2_fn = two_args;

  if (x > 0)
    int_fn = add_one;
  else
    int_fn = add_two;

  return (*int_fn)(x);
}
