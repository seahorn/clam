#include "clam/clam.h"

// Same signature for both callees, so the "types" resolver would offer both.
// The sea-dsa resolver looks at what the function pointer can actually hold
// and must promote the call to chosen only.

int chosen(int x) { return x + 1; }
int not_chosen(int x) { return x + 2; }

// volatile so that neither the store nor the load below is folded away before
// the devirtualization pass runs.
int (*volatile fn)(int);
int (*volatile decoy_fn)(int);

int main() {
  int x = nd_int();
  __CRAB_assume(x >= 0);

  // Address-taken, and never reachable from fn.
  decoy_fn = not_chosen;
  fn = chosen;

  return (*fn)(x);
}
