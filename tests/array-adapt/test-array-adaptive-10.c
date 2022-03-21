// RUN: %clam -m32 --crab-inter --crab-track=sing-mem --crab-dom=int --crab-check=assert --crab-sanity-checks --crab-heap-analysis=cs-sea-dsa-types "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^4  Number of total warning checks$
extern int int_nd(void);
extern char* name_nd(void);

extern void __CRAB_assume(int);
extern void __CRAB_assert(int);

#define sassert(X) __CRAB_assert(X)


void check(int* s, int flag) {
  if (flag > 0) {
    sassert(s[4] >= 0);
    sassert(s[4] <= 1);
  } else {
    sassert(s[8] >= 0);
    sassert(s[8] <= 1);
  }
}


int main(){

  int a[10];
  
  if (int_nd()) 
    a[0] = 1;
  if (int_nd())
    a[1] = 1;
  if (int_nd())  
    a[2] = 1;
  if (int_nd())  
    a[3] = 1;
  if (int_nd())  
    a[4] = 1;
  if (int_nd())
    a[5] = 1;
  if (int_nd())  
    a[6] = 1;
  if (int_nd())  
    a[7] = 1;
  if (int_nd())
    a[8] = 1;
  if (int_nd())  
    a[9] = 1;  

  check(&a[0], int_nd());
  
  return 0;
}
