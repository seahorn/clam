#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

// RUN: %clam -O0 --crab-inter --crab-dom=oct --promote-malloc=false --crab-track=mem --crab-check=bounds "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

/* Externalize Helper Function */
extern void __CRAB_assert(int);
extern void __CRAB_assume(int);
extern int int_nd(void);

extern bool sea_is_dereferenceable(const void *ptr, intptr_t offset);

// Domain: polka
int main(void) {
    int cap = int_nd();
    int min = 3;
    int max = 10;
    __CRAB_assume(cap > min);
    __CRAB_assume(cap <= max);

    int *out = malloc(sizeof(int) * cap);
    __CRAB_assert(sea_is_dereferenceable(out, min));
    return 0;
}