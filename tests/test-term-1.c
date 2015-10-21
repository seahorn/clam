extern int nd ();

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
  
  // w/ intervals u = [3,13]
  if (nd ())
    return 42;
  else 
    return u;
}
