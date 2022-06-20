#include <stdio.h>
#include "clam/clam.h"

#define N 10
// To test loops that decrements a counter
int main(int argc, char** argv) {
  int i;
  int a[N];
  for (i = N - 1; i >= 0; i--) {
    a[i] = i;
  }

  printf("%d\n", a[i + 1]);
  printf("%d\n", a[N - 1]);
  return 0;
}

