#include "clam/clam.h"

int a[10];
int main (){
  int i;
  for (i=0;i<10;i++) {
    if (nd_int ())
      a[i]=0;
    else 
      a[i]=5;
  }
  int res = a[i-1];
  return res;
}
