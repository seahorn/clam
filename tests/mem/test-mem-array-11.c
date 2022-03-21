// RUN: %clam -m32 --crab-inter --crab-track=mem --crab-dom=int --crab-check=assert --crab-sanity-checks --crab-heap-analysis=cs-sea-dsa-types "%s" 2>&1 | OutputCheck %s
// CHECK: ^8  Number of total safe checks$
// CHECK: ^0  Number of total warning checks$
extern int int_nd(void);
extern char* name_nd(void);

extern void __CRAB_assume(int);
extern void __CRAB_assert(int);

typedef struct {
  int x;
  int y;
} S1;

#define N 20
S1 a[N];
int b[10];

void check(S1* s1, int *s2, int flag) {
  if (flag > 0) {
    __CRAB_assert(s1[4].x >= 0);
    __CRAB_assert(s1[4].x <= 1);
    __CRAB_assert(s2[5] >= 0);
    __CRAB_assert(s2[5] <= 1);
    
  } else {
    __CRAB_assert(s1[8].y >= 0);
    __CRAB_assert(s1[8].y <= 1);
    __CRAB_assert(s2[9] >= 0);
    __CRAB_assert(s2[9] <= 1);
    
  }
}

int main(){

  int i;
  for(i=0;i<N;++i) {
    if (int_nd()) {
      a[i].x = 1;
    }
  }
  
  if (int_nd()) 
    a[0].y = 1;
  if (int_nd())
    a[1].y = 1;
  if (int_nd())  
    a[2].y = 1;
  if (int_nd())  
    a[3].y = 1;
  if (int_nd())  
    a[4].y = 1;
  if (int_nd())
    a[5].y = 1;
  if (int_nd())  
    a[6].y = 1;
  if (int_nd())  
    a[7].y = 1;
  if (int_nd())
    a[8].y = 1;
  if (int_nd())  
    a[9].y = 1;  

  if (int_nd()) 
    b[0] = 1;
  if (int_nd())
    b[1] = 1;
  if (int_nd())  
    b[2] = 1;
  if (int_nd())  
    b[3] = 1;
  if (int_nd())  
    b[4] = 1;
  if (int_nd())
    b[5] = 1;
  if (int_nd())  
    b[6] = 1;
  if (int_nd())  
    b[7] = 1;
  if (int_nd())
    b[8] = 1;
  if (int_nd())  
    b[9] = 1;  
  
  check(&a[0], &b[0], int_nd());
  
  return 0;
}
