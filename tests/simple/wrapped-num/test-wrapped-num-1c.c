// RUN: %crabllvm -O0 --crab-dom=w-zones --crab-check=assert "%s" 2>&1 | OutputCheck -l debug %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^1  Number of total warning checks$

extern void __CRAB_assert(int);
extern void __SEAHORN_error(int);

int main (){

  char x;
  int i, y;
  x=0;
  y=0;
  for (i=0;i < 128;i++) {
    x++;
    y++;
  }

  /*
    i = 128
    y = 128
    x = -128
   */


  // RES: false
  __CRAB_assert((unsigned) x <= (unsigned) y-1);   
  return x+y;
}
