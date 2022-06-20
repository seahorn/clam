#include <stdlib.h>
#include "clam/clam.h"

// RUN: %clam -O0 --crab-inter --crab-dom=sign-const --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

typedef struct node{
  int f;
  int* s;
  struct node *n;
} *List;

#define N 10000

List mk_list(int n, int*p, int*q) {
  List l = 0;
  
  /* first loop unrolling */
  List tmp = (List) malloc(sizeof(struct node));
  __CRAB_assume(tmp > 0);
  
  tmp->f = 0;
  if (nd_int()) {
    tmp->s = p;
  } else {
    tmp->s = q;
  }
  tmp->n = l ;
  l = tmp;
  
  /* n-1 loop iterations */
  int i;
  for (i=1; i<n;i++) {
    List tmp = (List) malloc(sizeof(struct node));
    __CRAB_assume(tmp > 0);    
    
    tmp->f = i;
    if (nd_int()) {
      tmp->s = p;
    } else {
      tmp->s = q;
    }
    tmp->n = l ;
    l = tmp;
  }
  return l;
}

int main() {

  int x;
  int y;
  
  /* read the list */
  List aux = mk_list(N, &x, &y);
  int acc = 0;
  while (aux) {
    __CRAB_assert(aux->f <= N-1); // SAFE but sign-const cannot prove it
    __CRAB_assert(aux->s > 0);    // SAFE
    acc += aux->f;
    aux = aux->n;
  }
  
  return acc;
}
