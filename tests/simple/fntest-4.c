#include "clam/clam.h"

int x =2;
int y =5;

int addTwo(int a) {
  x+=2;
  y++;
  return a+1;
}

int addOne(int a) {
  x++;
  int b=0;
  if (nd_int())
    b = addTwo (a);
  return a+b+1; // return value is lost from the summary
}


int addThree(int a) {
  x+=3;
  return a+1;
}

int main() {

  int a = 2;
  int b = addOne (a);
  int c = addTwo (b);
  int d = addThree (c);

  return x+y+d;
}
