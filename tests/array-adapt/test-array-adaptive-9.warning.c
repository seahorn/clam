// RUN: %clam -m32 --crab-inter --crab-track=arr --crab-dom=int --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^0  Number of total safe checks$
// CHECK: ^4  Number of total warning checks$

extern int int_nd(void);
extern char* name_nd(void);

extern void __CRAB_assume(int);
extern void __CRAB_assert(int);

int a[10];

void check(int* s, int n) {
  __CRAB_assert(s[n] >= 0);
  __CRAB_assert(s[n] <= 1);
}

int main(){

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

  check(&a[2], 4); // <- we lose precision if we take the address of a
		   // non-zero index.
  check(&a[2], 6);  
  
  return 0;
}
