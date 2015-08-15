
extern int nd ();

int x = 5;

int a[10];

int main ()
{
  int i;
  x++;
  for (i=0;i<10;i++)
  {
    a[i] =x;
  }
  int res = a[i-1];
  return res;
}
