#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-dom=pk --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

/* Externalize Helper Function */
extern size_t size_t_nd(void);
extern bool bool_nd(void);
extern int int_nd(void);
extern void __CRAB_assert(int);
extern void __CRAB_assume(int);

int main(void) {
    size_t len = size_t_nd();
    size_t cap = size_t_nd();
    size_t max = 10;
    // _1 = cap >= 0
    // _2 = cap <= 10
    // _3 = _1 & _2
    __CRAB_assume(cap >= 0);
    __CRAB_assume(cap <= max);
    // __CRAB_assume(cap >= 0 && cap <= max);
    /* INVARIANTS: ({_3 -> true}, {cap <= 10; cap >= 0}) */
    // _4 = len >= 0
    // _5 = len <= cap
    // _6 = _4 & _5
    __CRAB_assume(len >= 0);
    __CRAB_assume(len <= cap);
    /* INVARIANTS: 
        ({_3 -> true; _6 -> true}, 
        {cap <= 10; cap >= 0; len >= 0; len <= cap})
    */

    if (cap == 0) {
        /* INVARIANTS: ({_3 -> true; _6 -> true}, {cap = 0; len = 0;}) */
        __CRAB_assert(len == 0);
    }
    else {
        /* INVARIANTS: ({_3 -> true; _6 -> true}, {cap > 0; len >= 0; len <= cap}) */
        __CRAB_assert(len <= cap);
    }
    __CRAB_assert(cap >= 0 && cap <= max);
    return 0;
}