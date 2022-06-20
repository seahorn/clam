#include "clam/clam.h"

// RUN: %clam -O0 --inline --crab-lower-unsigned-icmp  --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

extern int8_t nd_int8_t(void);

uint8_t uint8_t_nd(void) {
    int8_t res = nd_int8_t();
    __CRAB_assume(res > 0);
    return res;
}

uint8_t call_f(uint8_t max_len) {
    uint8_t len = uint8_t_nd();
    __CRAB_assume(len <= max_len);
    return len;
}

int main(void) {
    int8_t max_len = 0b10000000; // unsigned: 128, signed: -128
    uint8_t res_f = call_f(max_len);
    __CRAB_assert(res_f <= 128);
    __CRAB_assert(res_f > 0);
    return 0;
}
