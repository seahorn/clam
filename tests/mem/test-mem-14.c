#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

// RUN: %clam -O0 --crab-inter --crab-dom=oct --crab-track=mem --crab-check=bounds "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
  
extern void __CRAB_assert(int);
extern void __CRAB_assume(int);
extern int int_nd(void);
extern bool bool_nd();

extern int nd();

bool uninterpreted_predicate_fn(uint8_t value) {
    return bool_nd();
}

int main() {
    int len = int_nd();
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
