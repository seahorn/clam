extern int nd ();
int main ()
{
  int a[10];
  int i;
  for (i=0;i<10;i++)
  {
    if (nd ())
      a[i] =0;
    else 
      a[i] =5;
  }

  int res = a[i-1];
  return res;
}
