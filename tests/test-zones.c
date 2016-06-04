extern void __CRAB_assert(int);
extern void __SEAHORN_error(int);

int main (){

  int x,y,i;
  x=0;
  y=0;
  for (i=0;i< 10;i++) {
    x++;
    y++;
  }

  __CRAB_assert(x==y);

  if (x != y) __SEAHORN_error(5);

  return x+y;
}
