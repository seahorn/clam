#include "clam/clam.h"

int main () {
  int i,x,y,offset_x,offset_y;
  x=0;
  y=0;
  for (i=0; i < nd_int (); i++){
    offset_x = nd_int ();
    offset_x += i+4;

    offset_y = nd_int ();
    offset_y += i+4;

    x+=offset_x;
    y+=offset_y;
  }
  return 42;
}
