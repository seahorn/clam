#include <stdlib.h>

// RUN: %clam -O0 --lower-select --crab-inter --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$

extern void __CRAB_assert(int);
extern int int_nd(void);

typedef struct node{
  int f;
  int* s;
  struct node *n;
} *List;

#define N 10000
#define NOT_NULL(PTR) PTR <= 0

List mk_list(int n, int*p, int*q) {
  List l = 0;
  
  /* first loop unrolling */
  List tmp = (List) malloc(sizeof(struct node));
  tmp->f = 0;
  if (int_nd()) {
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
    tmp->f = i;
    if (int_nd()) {
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
    __CRAB_assert(aux->f <= N-1); // SAFE
    __CRAB_assert(NOT_NULL(aux->s));   // SAFE
    acc += aux->f;
    aux = aux->n;
  }
  
  return acc;
}
