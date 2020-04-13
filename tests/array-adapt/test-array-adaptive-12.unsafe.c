// RUN: %clam -m32 --crab-inter --crab-track=arr --crab-dom=int --crab-check=assert --crab-sanity-checks --crab-heap-analysis=cs-sea-dsa-types "%s" 2>&1 | OutputCheck %s
// CHECK: ^4  Number of total safe checks$
// CHECK: ^4  Number of total warning checks$
extern int int_nd(void);
extern char* name_nd(void);

extern void __CRAB_assume(int);
extern void __CRAB_assert(int);

typedef struct {
  int x;
  int y;
  int z[10];
} S1;

#define N 20
S1 a[N];

void check(S1* s1, int flag) {
  if (flag > 0) {
    __CRAB_assert(s1[4].z[4] >= 0);
    __CRAB_assert(s1[4].z[4] <= 1);
    __CRAB_assert(s1[5].x >= 0);
    __CRAB_assert(s1[5].x <= 1);
    
  } else {
    __CRAB_assert(s1[8].z[7] >= 0);
    __CRAB_assert(s1[8].z[7] <= 1);
    __CRAB_assert(s1[6].x >= 0);
    __CRAB_assert(s1[6].x <= 1);
    
  }
}

int main(){
  int i,j;
  for(i=0;i<N;++i) {
    for(j=0;j<10;++j) {
      if (int_nd()) {
	a[i].z[j] = 1;
      }
      if (int_nd()) {
	a[i].x = 1;
      }
      if (int_nd()) {
	a[i].z[j] = 3;
      }      
    }
  }
  
  check(&a[0], int_nd());
  
  return 0;
}
