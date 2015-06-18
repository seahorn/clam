#include <stdlib.h>

typedef struct node{
  int f;
  int s;
  struct node *n;
} *List;

int main ()
{

  List l = 0;
  int i;
  for (i=-17; i<42;i++)
  {
    List tmp = (List) malloc(sizeof(struct node));
    tmp->f = i;
    tmp->s = 500;
    tmp->n = l ;
    l = tmp;
  }
  return 42;
}
