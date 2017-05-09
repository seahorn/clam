// RUN: %crabllvm -O0 --crab-dom=zones-split --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total error checks$

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

  __CRAB_assert(x> y); //error

  return x+y;
}
