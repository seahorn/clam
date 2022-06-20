#include "clam/clam.h"

int a[10];
int b[10];

int main () {
  
  int i;
  for (i=0; i < 10; i++){
    a[i] = b[i];
  }

  int j = nd_int ();
  if (j >= 0 && j < 10)
    return a[j];
}
