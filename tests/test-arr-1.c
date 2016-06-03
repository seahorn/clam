extern int nd ();
extern void __CRAB_assert(int);

int a[10];

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

  int res = a[i-1];
  __CRAB_assert(res >= 0 && res <= 5);
  return res;
}
