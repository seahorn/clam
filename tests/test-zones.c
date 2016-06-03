extern void __CRAB_assert(int);

int main (){

  int x,y,i;
  x=0;
  y=0;
  for (i=0;i< 10;i++) {
    x++;
    y++;
  }
  __CRAB_assert(x==y);
  return x+y;
}
