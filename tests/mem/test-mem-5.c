#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0  --crab-inter --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^3  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

typedef struct node{
  int f;
  int s;
  struct node *n;
} *List;

#define N 10000

List mk_list(int n) {
  List l = 0;
  
  /* first loop unrolling */
  List tmp = (List) malloc(sizeof(struct node));
  tmp->f = 0;
  if (nd_int()) {
    tmp->s = n*2;
  } else {
    tmp->s = n*3;
  }
  tmp->n = l ;
  l = tmp;
  
  /* n-1 loop iterations */
  int i;
  for (i=1; i<n;i++) {
    List tmp = (List) malloc(sizeof(struct node));
    tmp->f = i;
    if (nd_int()) {
      tmp->s = n*2;
    } else {
      tmp->s = n*3;
    }
    tmp->n = l ;
    l = tmp;
  }
  return l;
}

int main() {

  /* read the list */
  List aux = mk_list(N);
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
