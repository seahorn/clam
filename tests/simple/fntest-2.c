#include "clam/clam.h"

int addOne(int x) {
  return x + 1;
}

int addTwo(int x) {
  return x + 2;
}

int addThree(int x) {
  return x + 3;
}

int main() {
  //int (*fPtr_array[2])(int);
  int (*f) (int);
  int x = 8;
  if (nd_int()) {
    //fPtr_array[0] = &addOne;
    f = &addOne;
  } else if (nd_int()) {
    //fPtr_array[1] = &addTwo;
    f = &addTwo;
  } else {
    f = &addThree;
    //fPtr_array[1] = &addThree;
  }

  //int res1 = fPtr_array[0](x);
  //int res2 = fPtr_array[1](x);
  // assert(res1 == 9);
  // assert(res2 >= 10 && res2 <= 11);
  return f (x);
}
