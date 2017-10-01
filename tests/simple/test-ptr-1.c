extern int nd ();

int a[10];

int main ()
{
  int* p;
  //int* q;

  if (nd ())
  {
    p = &a[0] + 2;
    //q = &a[0] + 3;
  }
  else
  {
    p = &a[0] + 3;
    //q = &a[0] + 2;
  }

  int res;
  if ( p == &a[0]+1)
    res = 0;
  else
    res = 1;
  return res;
}
