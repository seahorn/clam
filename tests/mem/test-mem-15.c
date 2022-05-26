#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"

// RUN: %clam -O0 --crab-inter --crab-dom=pk --promote-malloc=false --crab-track=mem --crab-check=bounds "%s" 2>&1 | OutputCheck %s
// CHECK: ^4  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

// Domain: polka
int main(void) {
    int cap = nd_int();
    int max = 10;
    __CRAB_assume(cap > 0);
    __CRAB_assume(cap <= max);

    int *out = malloc(sizeof(int) * cap);
    for (int i = 0; i < cap; ++i) {
        out[i] = i;
    }
    for (int i = 0; i < cap; ++i) {
        // load reference is safe
        __CRAB_assert(out[i] >= 0); // no element determined
    }
    return 0;
}
