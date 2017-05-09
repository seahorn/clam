// RUN: %crabllvm -O0 --crab-dom=zones-split --crab-check=assert "%s" 2>&1 | OutputCheck %s
// CHECK: ^2  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$

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

  // domains might have problems with disequality when proving the
  // assertion
  //__CRAB_assert(x==y);
  
  __CRAB_assert(x>=y);
  __CRAB_assert(y>=x);

  //if (x != y) __SEAHORN_error(5);

  return x+y;
}
