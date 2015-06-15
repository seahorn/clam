
extern int nd ();

int x = 5;
int y = 3;


int main ()
{
  int a[10];
  int i;
  int *p =&x;
  *p= *p + 1;
  for (i=0;i<10;i++)
  {
    if (nd ())
      a[i] =y;
    else 
      a[i] =x;
  }
  y++;
  int res = a[i-1];
  return res;
}
