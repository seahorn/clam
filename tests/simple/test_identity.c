// RUN: %clam --crab-dom=int --crab-inter --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
extern int int_nd(void);
extern void __CRAB_assert(int);

int f(int x) {
   return x;
}

 

int main(void) {

   int y = f(0);
   __CRAB_assert(y == 0);
   return 0;

}
