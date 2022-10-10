#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0 --crab-lower-unsigned-icmp --crab-inter --crab-dom=zones --crab-track=mem --llvm-peel-loops=1 --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^6  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

typedef struct node{
  int f;
  int s;
  struct node *n;
} *List;

#define N 10000

int main() {

  /* The malloc is converted to an alloca by the LLVM frontend */

  /* create the list from the end to the start*/
  List l = 0;
  int i;
  for (i=0; i<N;i++) {
    List tmp = (List) malloc(sizeof(struct node));
    tmp->f = i;
    if (nd_int()) {
      tmp->s = N*2;
    } else {
      tmp->s = N*3;
    }
    tmp->n = l ;
    l = tmp;
  }

  /* read the list */
  List aux = l;
  int acc = 0;
  while (aux) {
    __CRAB_assert(aux->f <= N-1); // SAFE
    __CRAB_assert(aux->s >= N*2); // SAFE
    __CRAB_assert(aux->s <= N*3); // SAFE
     acc += aux->f;
    aux = aux->n;
  }
  
  return acc;
}
