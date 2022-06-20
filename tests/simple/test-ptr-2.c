#include "clam/clam.h"

int a[10];

/* Example of overlapping cells.
   The final dsa node looks like this:
    flags=[MR] size=8 types={0:i32,2:i16,4:i32} links=[]
   
   EXPECTED: we won't produce an array for offsets a[0] or p but we
   should for a[1].
*/
int main() {
  int n=10;
  
  if (nd_int())
    a[0] = 5;
  if (nd_int())
    a[1] = 10;
  
  short* p = (short*) &a[0];
  short x = 1;
  return p[x];  
}
