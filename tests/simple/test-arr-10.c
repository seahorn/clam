#include "clam/clam.h"

int a[10];

int main (){
  int *p = &a[0];
  while (1) {
    *p = 5;
    p++;
    if (p >= 10)
      break;
  }
  int *q = &a[0];
  return *(q+4);
}
