// RUN: %clam --crab-dom=int --crab-lower-unsigned-icmp --crab-check=assert %s 2>&1 | OutputCheck %s
// RUN: %clam --crab-dom=int --lower-unsigned-icmp --crab-check=assert %s 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$


extern void __CRAB_assume(int cond);
extern int __CRAB_nd(void);
extern void __CRAB_assert(int);

int N;

int main() {
  N = __CRAB_nd();

  __CRAB_assume(N != -2147483648 && N != 2147483647);

  for (int i = 0; i < N; i++) {
    __CRAB_assert(N >= -1);
  }
  
  return 0;
} 
