// RUN: %clam --inline --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa-types --crab-lower-unsigned-icmp=true --crab-check=assert  --crab-disable-warnings --crab-inter  --entry=entrypoint "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

/**
 * @brief A program demonstrating the transfer of lamports
 */

#include <stdint.h>
#include <stdbool.h>
#include "clam/clam.h"

typedef struct {
  //SolPubkey *key;      /** Public key of the account */
  uint64_t *lamports;  /** Number of lamports owned by this account */
  uint64_t data_len;   /** Length of data in bytes */
  uint8_t *data;       /** On-chain data within this account */
  //SolPubkey *owner;    /** Program that owns this account */
  uint64_t rent_epoch; /** The epoch at which this account will next owe rent */
  bool is_signer;      /** Transaction was signed by this account's key? */
  bool is_writable;    /** Is the account writable? */
  bool executable;     /** This account's data contains a loaded program (and is now read-only) */
} SolAccountInfo;


typedef struct {
  SolAccountInfo* ka; /** Pointer to an array of SolAccountInfo, must already
                          point to an array of SolAccountInfos */
  uint64_t ka_num; /** Number of SolAccountInfo entries in `ka` */
  const uint8_t *data; /** pointer to the instruction data */
  uint64_t data_len; /** Length in bytes of the instruction data */
  //const SolPubkey *program_id; /** program_id of the currently executing program */
} SolParameters;


extern NONDET_FN_ATTR uint64_t __CVT_nondet_uint64(void);
extern NONDET_FN_ATTR int64_t __CVT_nondet_int64(void);  
extern bool __CVT_deserialize(const uint8_t *input, SolParameters *output, uint64_t ka_num);  

#define DESERIALIZE __CVT_deserialize
#define ASSERT __CRAB_assert
#define ASSUME __CRAB_assume
#define SOL_ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))
#define ERROR_INVALID_ARGUMENT 9999999
#define SUCCESS 0

uint64_t CVT_nondet_uint64(void) {
  int64_t res = __CVT_nondet_int64();
  if (res >= 0) {
    return res;
  }
  ASSUME(0);
  return 0;
}


void init_account(SolAccountInfo *account, uint64_t val) {
  *(account->lamports) = val;
}

void transfer(SolParameters *params, uint64_t k) {
  // As part of the program specification the first account is the source
  // account and the second is the destination account
  SolAccountInfo *source_info = &params->ka[0];
  SolAccountInfo *destination_info = &params->ka[1];

  // Withdraw five lamports from the source
  *source_info->lamports -= k;
  // Deposit five lamports into the destination
  *destination_info->lamports += k;
}

uint64_t entrypoint(const uint8_t *input) {
  
  SolAccountInfo accounts[2];
  SolParameters params = (SolParameters){.ka = accounts};
  
  if (!DESERIALIZE(input, &params, SOL_ARRAY_SIZE(accounts))) {
    return ERROR_INVALID_ARGUMENT;
  }

  SolAccountInfo *source_info = &params.ka[0];
  SolAccountInfo *destination_info = &params.ka[1];
  
  
  // Initialization of the source account
  {
    uint64_t init_val = CVT_nondet_uint64();
    ASSUME(init_val <= 5000);
    init_account(source_info, init_val);
  }
  // Initialization of the destination account
  {
    uint64_t init_val = CVT_nondet_uint64();
    ASSUME(init_val <= 5000);
    init_account(destination_info, init_val);
  }

  uint64_t src_lamport_before = *(source_info->lamports);
  uint64_t dst_lamport_before = *(destination_info->lamports);  

  // actual transaction: transfer val from src to dst
  uint64_t val = CVT_nondet_uint64();
  ASSUME(val <= 1000);  
  transfer(&params, val);

  
  uint64_t src_lamport_after = *(source_info->lamports);
  uint64_t dst_lamport_after = *(destination_info->lamports);

  // provable with zones
  ASSERT(src_lamport_before >= src_lamport_after);
  ASSERT(dst_lamport_before <= dst_lamport_after);
  
  // provable with pk
  //ASSERT(src_lamport_before == src_lamport_after + val);
  //ASSERT(dst_lamport_after  == dst_lamport_before + val);
  
  return SUCCESS;
}
