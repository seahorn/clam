extern int __VERIFIER_NONDET();
extern void __VERIFIER_error() __attribute__((noreturn));

int e=0;
int s=2;  

// we should get e=[0,2] and s=[2,5] without applying widening 

void foo () {

  while (__VERIFIER_NONDET()) {
    if (s == 2){
      if (e ==0) e=1;
      s = 3;
    }
    else if (s == 3){
      if (e ==1) e=2;
      s=4;
    }
    else if (s == 4){
      if (e == 3) {
         __VERIFIER_error();
      }
      s=5;
    }
  }

}


int main(){
  foo ();
  return 42;
}
