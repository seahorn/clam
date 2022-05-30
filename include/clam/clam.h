#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>


#ifdef __cplusplus
extern "C" {
#endif

#define NONDET_FN_ATTR __declspec(noalias)
#define VERIFIER_FN_ATTR __declspec(noalias)  

/**
 * Crab verifier functions
 */  
extern VERIFIER_FN_ATTR void __CRAB_assert(int);
extern VERIFIER_FN_ATTR void __CRAB_assume(int);
extern __attribute__((noreturn)) void __VERIFIER_error (void);

/**
 * Non-determinstic functions 
 */
extern NONDET_FN_ATTR bool nd_bool(void);
extern NONDET_FN_ATTR int nd_int(void);
extern NONDET_FN_ATTR size_t nd_size_t(void);
extern NONDET_FN_ATTR int16_t nd_int16_t(void);
extern NONDET_FN_ATTR int32_t nd_int32_t(void);
extern NONDET_FN_ATTR int64_t nd_int64_t(void);
extern NONDET_FN_ATTR int8_t nd_int8_t(void);
extern NONDET_FN_ATTR uint16_t nd_uint16_t(void);
extern NONDET_FN_ATTR uint32_t nd_uint32_t(void);
extern NONDET_FN_ATTR uint64_t nd_uint64_t(void);
extern NONDET_FN_ATTR uint8_t nd_uint8_t(void);
extern void *nd_voidp(void) __attribute__((malloc));


/* Return true if offset number of bytes of p ptr are allocated */
extern bool sea_is_dereferenceable(const void *ptr, intptr_t offset);
/* Print invariants projected onto specific variables */
extern void __CRAB_intrinsic_print_invariants(int,...);
/* Enable disjunctive invariants based on the values on the variable */  
extern void __CRAB_intrinsic_value_partition_start(int,...);
/* Disable disjunctive invariants based on the values on the variable */    
extern void __CRAB_intrinsic_value_partition_end(int,...);

#define CRAB_PRINT_INVARIANTS __CRAB_intrinsic_print_invariants  
#define CRAB_VALUE_PARTITION_START __CRAB_intrinsic_value_partition_start
#define CRAB_VALUE_PARTITION_END __CRAB_intrinsic_value_partition_end

  
#ifdef __cplusplus
}
#endif
  
