extern int nd ();
extern void __CRAB_assert(int);

int main()
{
  int u,y,z,x;

  if (nd ())
    u = 0;
  else 
    u = 10;

  if (nd ())
    y = 0;
  else 
    y = 10;

  if (nd ())
    z = 0;
  else 
    z = 10;

  if (nd ())
    x = u+y;
  else 
    x = u+z;

  if (x < 3)
    u = u + 3;
  else
    u = 3;

  //__CRAB_assert(u >= 3 && u <= 13);
  // w/ --crab-dom=term-int or --crab-dom=term-dis-int
  __CRAB_assert(u >= 3 && u <= 5);
  if (nd ())
    return 42;
  else 
    return u;
}
