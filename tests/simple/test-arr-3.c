// RUN: %crabllvm -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=arr --crab-heap-analysis=llvm-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %crabllvm -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=arr --crab-heap-analysis=ci-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// RUN: %crabllvm -O0 --lower-unsigned-icmp --crab-dom=int --crab-track=arr --crab-heap-analysis=cs-sea-dsa --crab-check=assert --crab-sanity-checks "%s" 2>&1 | OutputCheck %s
// CHECK: ^1  Number of total safe checks$
// CHECK: ^0  Number of total error checks$
// CHECK: ^0  Number of total warning checks$
extern int nd ();
extern void __CRAB_assert(int);

int x = 5;

int a[10];

int main ()
{
  int i;
  x++;
  for (i=0;i<10;i++)
  {
    a[i] =x;
  }
  int res = a[i-1];
  __CRAB_assert(res >= 0 && res <= 9);
  return res;
}
