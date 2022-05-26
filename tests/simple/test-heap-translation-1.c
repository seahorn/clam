#include "clam/clam.h"

int x = 5;
int y = 3;

void foo ()
{
  x++;
}

void bar ()
{
  y++;
}

int a[10];

int main ()
{
  int i;
  foo ();
  for (i=0;i<10;i++)
  {
    if (nd_int())
      a[i] =y;
    else 
      a[i] =x;
  }
  bar ();
  int res = a[i-1];
  __CRAB_assert(res >= 0 && res <= 6);
  return res;
}
