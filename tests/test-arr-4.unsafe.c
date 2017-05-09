// RUN: %crabllvm -O0 --crab-inter --crab-dom=int --crab-track=arr --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
extern int nd ();
extern void __CRAB_assert(int);

int x = 5;
int y = 3;

void foo ()
{
  x++;
}

void bar ()
{
  y++;
}

int a[10];

int main ()
{
  int i;
  foo ();
  for (i=0;i<10;i++)
  {
    if (nd ())
      a[i] =y;
    else 
      a[i] =x;
  }
  bar ();
  int res = a[i-1];
  __CRAB_assert(res >= 7);
  return res;
}
