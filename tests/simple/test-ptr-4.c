#include "clam/clam.h"

short a[10];

/* Example of converting non-array dsa node to an array node.

   The final dsa node looks like this:
        types={0:i16|i32,2:i16} flags=[AMR], size=4, links=[]

   EXPECTED: no crab arrays extracted from dsa node due to overlapping
   cells.
 */
int main() {
  int n=10;

  /* The node is initially marked as non-array by sea-dsa */
  if (nd_int())
    a[0] = 5;
  if (nd_int())
    a[1] = 10;
  if (nd_int())
    a[2] = 20;

  /* After this loop, the node is marked as array because of symbolic
     accesses.  However, sea-dsa will NOT collapse the node because
     the size when it was a non-array size (2) fits into the size of
     the node as an array (4).
   */
  int* p = (int*) &a[0];
  int i;
  for(i=0;i<5;++i) { 
    p[i] = 7;
  }
  return p[1];  
}
