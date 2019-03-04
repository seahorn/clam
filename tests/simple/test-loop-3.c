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
extern int nd(void);
extern void __VERIFIER_error(void) __attribute__((noreturn));
#define assert(X) if(!(X)){__VERIFIER_error();}
extern void __VERIFIER_assume (int v);
#define assume(X) __VERIFIER_assume(X)


int main(){
    // Constructor
    int x = 0;
    int y = 1;

    while (nd()) {
        int fid = nd();
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
