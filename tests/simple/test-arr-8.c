#include <stdlib.h>
#include "clam/clam.h"

int* mymalloc (int *a) {
  //return (int*) calloc (10, sizeof(int));
  return a;
}

int main ()
{
  int *_a = (int*) calloc (10, sizeof(int));
  int *a = mymalloc (_a);
  int b[10];
  int i;
  for (i=0;i<10;i++)
    b[i] = a[i];

  int x = a[i-1]  + b[i-1];
  if (x == 0)
    return 42;
  else
    return 0;
}
