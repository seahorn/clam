extern void __CRAB_assert(int);
extern int int_nd(void);

// RUN: %clam -O0  --crab-inter --crab-dom=zones --crab-track=mem --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^3  Number of total warning checks$


// Clam cannot prove anything because the two stores in f are modeled
// as weak updates.


// if p and q alias then *p=2 and *q=2
// else *p=1 and *q=2
void f(int* p, int* q) {
  if (int_nd()) {
    p = q;
  }    
  *p = 1;
  *q = 2;

}

 

int main() {
  volatile int x;
  volatile int y;
  f(&x, &y);

  __CRAB_assert(x >= 1);
  __CRAB_assert(x <= 2);
  __CRAB_assert(y == 2);
  return 0;
}
