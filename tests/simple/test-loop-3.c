/* Challenging example: 
   To keep the relationship x = y - 1 while x <= 31 is easy.
   The tricky part is when x = 32:
     - We add one to x so the relationship between x and y is x = y so
       the assertion still holds.
     - Set err to 1.
   Once err is one we don't update x or y anymore.

   After the switch we lose whether err is 0 or 1 so we will
   keep incrementing x and y. 
*/
#include "clam/clam.h"

#define assert(X) if(!(X)){__VERIFIER_error();}

int main(){
    // Constructor
    int x = 0;
    int y = 1;

    while (nd_int()) {
        int fid = nd_int();
        // We create snapshots for all state variables.
        int nx = x;
        int ny = y;
        int err = 0;
        switch (fid) {
        case 0:
	  if (nx < 32) {
	    nx += 2;
	    ny += 2;
	  } else {
	    nx++;
	    err = 1;
	  }
	  break;
	default:;;
        }
        if (err == 0) {
            // Commit state changes.
            x = nx;
            y = ny;
        }
    }
    assert(x <= y);
    return 0;
}
