#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"

// RUN: %clam -O0 --crab-inter --crab-dom=soct --promote-malloc=false --crab-track=mem --crab-check=bounds "%s" 2>&1 | OutputCheck %s 
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

bool uninterpreted_predicate_fn(uint8_t value) {
    return nd_bool();
}

int main() {
    int len = nd_int();
    __CRAB_assume(len > 0);
    int max_len = 1024;
    __CRAB_assume(len < max_len);
    int data_type = 1;
    uint8_t *ptr = malloc(len * data_type);
    while (len > 0) {
        uint8_t *p2 = ptr + (len - 1);
        uint8_t tmp = *p2;
        if (uninterpreted_predicate_fn(tmp)) break;
        -- len;
    }
   return 0;
}
