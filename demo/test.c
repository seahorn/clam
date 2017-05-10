extern void __CRAB_assume (int);
extern void __CRAB_assert(int);
extern int  __CRAB_nd();

int main() {
  int k = __CRAB_nd();
  int n = __CRAB_nd();
  __CRAB_assume (k > 0);
  __CRAB_assume (n > 0);
  
  int x = k;
  int y = k;
  while (x < n) {
    x++;
    y++;
  }
  __CRAB_assert (x >= y);
  __CRAB_assert (x <= y);  
  return 0;
}
