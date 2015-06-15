
extern int nd ();

int x = 5;

int main ()
{
  int a[10];
  int i;
  x++;
  for (i=0;i<10;i++)
  {
    a[i] =x;
  }
  int res = a[i-1];
  return res;
}
