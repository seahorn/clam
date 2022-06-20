// RUN: %clam --crab-dom=zones --crab-track=mem --inline --crab-heap-analysis=cs-sea-dsa --lower-unsigned-icmp --crab-lower-with-overflow-intrinsics=true --crab-check=assert --crab-dom-param="region.is_dereferenceable=true" "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$


#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include "clam/clam.h"


#define MAX_ARRAY_LENGTH 10
#define MAX_ARRAY_ITEM_SIZE 2

struct aws_array_list {
    size_t current_size;
    size_t length;
    size_t item_size;
    void *data;
};

size_t size_t_nd(void) {
    int res = nd_int();
    __CRAB_assume(res >= 0);
    return res;
}

bool aws_array_list_is_bounded(const struct aws_array_list *const list,
                               const size_t max_initial_item_allocation,
                               const size_t max_item_size) {
  bool item_size_is_bounded = list->item_size == max_item_size;
  bool length_is_bounded = list->length <= max_initial_item_allocation;
  return item_size_is_bounded && length_is_bounded;
}


void initialize_bounded_array_list(struct aws_array_list *const list, bool split_assume) {
  list->current_size = size_t_nd();
  list->item_size = size_t_nd();
  list->length = size_t_nd();
  list->data = malloc(list->current_size);
  if (split_assume) {
    __CRAB_assume(list->item_size == MAX_ARRAY_ITEM_SIZE);
    __CRAB_assume(list->length <= MAX_ARRAY_LENGTH);
    __CRAB_assume(list->current_size >= list->item_size * list->length);
  }
  else {
    __CRAB_assume(aws_array_list_is_bounded(list, MAX_ARRAY_LENGTH, MAX_ARRAY_ITEM_SIZE));
  }
}

int main() {
    /* data structure */
    struct aws_array_list list;
    initialize_bounded_array_list(&list, true);
    
    /* assertion checks */
    __CRAB_assert(list.length <= MAX_ARRAY_LENGTH);
    __CRAB_assert(list.item_size == MAX_ARRAY_ITEM_SIZE);
    
    return 0;
}
