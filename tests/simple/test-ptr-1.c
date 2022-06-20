#include "clam/clam.h"

int a[10];

/* 
   Pointer arithmetic example. sea-dsa is fine because the array is
   never accessed. 
*/
int main() {
  int* p;
  
  if (nd_int()) {
    p = &a[0] + 2;
  }
  else {
    p = &a[0] + 3;
  }

  int res;
  if ( p == &a[0]+1) {
    res = 0;
  }   else {
    res = 1;
  }
  return res;
}
