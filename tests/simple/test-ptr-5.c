#include "clam/clam.h"

char a[40];

/* 
   Similar to test-ptr-2.c and test-ptr-4.c
   The DSA node looks like this:
      flags=[AMR] size=4 types={0:i32,3:i8} links=[].

   EXPECTED: we do not produce an array because we might have
   overlapping cells. Even if we would replace a[3] with a[4] we would
   not generate an array.
 */
int main() {
  int n=10;

  if (nd_int())
    a[3] = 10;

  int* p = (int*) &a[0];
  int i;
  for(i=1;i<10;++i) { 
    p[i] = 7;
  }
  return p[1];  
}
