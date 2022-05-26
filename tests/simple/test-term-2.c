#include "clam/clam.h"

int main()
{
  int x,y,z;
  x=0;
  y=0;
  z=0;
  while (nd_int ())
  {
    x ++;
    y++;
    z = z -2;
  }
  while (nd_int ())
  {
    x = x - 1;
    y = y - 3;
    z = z + 2;
  }

  if (nd_int ())
    return x+y+z;
  else 
    return 42;
}
