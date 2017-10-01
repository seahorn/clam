#include <stdio.h>

int main ()
{
  int i,j;
  for (i=0; i < 10; i++)
  {
    for (j=0; j < 10; j++)
      printf("%d-%d ",i,j);
  }
  return 42;
}
