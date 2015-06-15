
extern int nd ();

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

int main ()
{
  int a[10];
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
  return res;
}
