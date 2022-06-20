// RUN: %clam -O0 --crab-only-cfg --crab-print-cfg=true "%s" 2>&1 | OutputCheck %s
// CHECK: crab_intrinsic
#include "clam/clam.h"

extern int __CRAB_intrinsic_foo(int, int);
int main (){

  int x,y,z, i;
  x=0;
  y=0;
  z=0;
  for (i=0;i< 10;i++) {
    x++;
    y++;
    z = __CRAB_intrinsic_foo(x, y);    
  }

  __CRAB_assert(x>=y);
  __CRAB_assert(y>=x);
  __CRAB_assert(z>=x);  

  return x+y;
}


/* _ret:int declare main() */
/* _br: */
/*   .03 = 0; */
/*   .02 = 0; */
/*   .01 = 0; */
/*   .0 = 0; */
/*   goto _.03; */
/* _.03: */
/*   goto __@bb_5,__@bb_6; */
/* __@bb_5: */
/*   assume(.0 <= 9); */
/*   goto _4; */
/* _4: */
/*   _5 = .01+1; */
/*   _6 = .02+1; */
/*   _7 = crab_intrinsic(foo,_5:int,_6:int); */
/*   _br2 = .0+1; */
/*   .03 = _7; */
/*   .02 = _6; */
/*   .01 = _5; */
/*   .0 = _br2; */
/*   goto _.03; */
/* __@bb_6: */
/*   assume(-.0 <= -10); */
/*   goto _9; */
/* _9: */
/*   _10 = (.02-.01 <= 0); */
/*   assert(_10); */
/*   _12 = (-.02+.01 <= 0); */
/*   assert(_12); */
/*   _14 = (-.03+.01 <= 0); */
/*   assert(_14); */
/*   _ret = .01+.02; */
/*   return _ret; */

