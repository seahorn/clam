#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"

// RUN: %clam -O0 --crab-inter --crab-dom=soct --promote-malloc=false --crab-track=mem --crab-check=is-deref "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

extern bool sea_is_dereferenceable(const void *ptr, intptr_t offset);

int main(void) {
    int cap = nd_int();
    int min = 3;
    int max = 10;
    __CRAB_assume(cap > min);
    __CRAB_assume(cap <= max);

    int *out = malloc(sizeof(int) * cap);
    // clam adds an assertion so no need to add __CRAB_assert.
    sea_is_dereferenceable(out, min);
    //__CRAB_assert(sea_is_dereferenceable(out, min));
    return 0;
}
