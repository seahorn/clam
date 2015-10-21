#include <stdlib.h>

extern void __VERIFIER_assume (int);
extern void __VERIFIER_error (void);

#define N 3
int* foo () {
  //int* res = (int*) calloc (N, sizeof(int));
  //res[0] = 0;
  int* res = (int*) malloc (N* sizeof(int));
  res[0] = 0;
  return res;
  
}

int main ()
{
  int *a = foo ();
  int b[N];
  int i;
  for (i=0;i<N;i++)
    b[i] = a[i];

  int x = a[i-1]  + b[i-1];

  if (x > 0)
    __VERIFIER_error ();
  
  return 42;
  /* if (x == 0) */
  /*   return 42; */
  /* else */
  /*   return 0; */
}
