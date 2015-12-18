extern int nd ();

int a[2];

int main() {
  if (nd ())
    a[0] = 5;
  if (nd ())
    a[1] = 10;
  
  char* p = (char*) &a[0];
  p[nd ()] = 7;

  // kills a[0] but not a[1] but array abstraction will ignore the
  // whole thing.
  return a[1];
}
