#include "clam/clam.h"

int a[10];

/* 
   Example of converting non-array dsa node to an array node.
   The final dsa node gets collapsed (see explanation below)
*/
int main() {
  int n=10;

  /* The node is initially marked as non-array by sea-dsa */
  if (nd_int())
    a[0] = 5;
  if (nd_int())
    a[1] = 10;

  /* After this loop, the node is marked as array because of symbolic
     accesses.  Moreover, sea-dsa will collapse the node because the
     size when it was a non-array size (4) does not fit into the size
     of the node as an array (2).
   */
  short* p = (short*) &a[0];
  int i;
  for(i=0;i<10;++i) { 
    p[i] = 7;
  }
  return p[1];  
}
