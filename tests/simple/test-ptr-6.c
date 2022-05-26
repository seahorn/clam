#include "clam/clam.h"

short a[40];

/* 
   Smaller array unified with larger array.
   The a and p are unified and collapsed.
 */
int main() {
  int i;

  for(i=0;i<20;++i) { 
    a[i] = 7;
  }
  
  int* p = (int*) &a[0];
  for(i=0;i<10;++i) { 
    p[i] = 7;
  }
  
  return p[1] + a[3];  
}
