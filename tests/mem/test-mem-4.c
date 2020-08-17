#include <stdlib.h>

// RUN: %clam -O0  --crab-inter --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^1  Number of total warning checks$

extern void __CRAB_assert(int);
extern int int_nd(void);

/** 
 * Clam cannot prove the upper bound on aux->f. The problem is that
 * typically narrowing does a pretty good job recovering upper-bounds
 * after widening. However, at the time narrowing is trigerred only
 * weak updates are possible and those are not strong enough to
 * recover the upper-bound for aux->f.
 */

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
    if (int_nd()) {
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
    __CRAB_assert(aux->f <= N-1); // WARNING
    __CRAB_assert(aux->s >= N*2); // SAFE
    __CRAB_assert(aux->s <= N*3); // SAFE
     acc += aux->f;
    aux = aux->n;
  }
  
  return acc;
}
