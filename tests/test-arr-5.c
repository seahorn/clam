// RUN: %crabllvm -O0 --crab-dom=int --crab-track=arr --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
extern int nd ();
extern void __CRAB_assert(int);

int a[10];
int b[10];

int main ()
{
  int i;
  for (i=0;i<10;i++)
  {
    if (nd ())
      a[i] =0;
    else 
      a[i] =5;
  }

  for (i=0;i<10;i++)
  {
    if (nd ())
      b[i] =20;
    else 
      b[i] =25;
  }

  int x = a[i-1]  + b[i-1];
  __CRAB_assert (x >= 0 && x<=30);

  if (x >= 0 && x<=30)
    return 42;
  else
    return 0;
}
