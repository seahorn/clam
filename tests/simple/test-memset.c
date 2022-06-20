#include <stdio.h>
#include <string.h>
#include "clam/clam.h"

int main ()
{
  int a[5],b[5];
  int i;
  memset(a, 5, sizeof(int)*5);

  for (i=0;i<5;i++)
  {
    if (nd_int())
      b[i] =a[i];
    else 
      b[i] =a[i]+1;
  }

  int res = b[i-1];
  return res;
}
