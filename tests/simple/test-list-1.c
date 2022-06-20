#include <stdlib.h>
#include "clam/clam.h"

typedef struct node{
  int f;
  int s;
  struct node *n;
} *List;

int main() {

  /* create the list */
  List l = 0;
  int i;
  for (i=-17; i<42;i++) {
    List tmp = (List) malloc(sizeof(struct node));
    tmp->f = i;
    tmp->s = 500;
    tmp->n = l ;
    l = tmp;
  }

  /* read the list */
  List aux = l;
  int acc = 0;
  while (aux) {
    acc += aux->f;
    aux = aux->n;
  }
  
  return acc;
}
