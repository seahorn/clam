
#include <stdlib.h>
#include "clam/clam.h"

int* foo (int *x)
{
  int * m = (int*) malloc (sizeof (int) * 10);
  int i;
  for (i=0;i<10;i++)
  {
    m[i] = x[i];
    x[i] = 0;
  }
  return m;
}

int a[10];
int b[10];

int main ()
{
  int i;
  for (i=0;i<10;i++)
    b[i] = 5;
  int * c = foo (a); 
  int res = c[9];
  return res;
}
