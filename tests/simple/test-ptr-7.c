#include "clam/clam.h"

int a[40];

/* 
   Larger array unified with smaller array.
   The a and p are unified and collapsed.
 */

int main() {
  int i;

  for(i=0;i<10;++i) { 
    a[i] = 7;
  }

  short* p = (short*) &a[0];
  for(i=0;i<20;++i) {
    p[i] = 8; 
  } 
  
  return p[1];   
}
