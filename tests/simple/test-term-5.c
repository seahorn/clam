#include "clam/clam.h"

int foo(int* p, const int* A, const int* B, int n) {
  int numNonZero = n - 1;
  while (numNonZero--) {
    *p *= (*A++) * (*B++);
  }
  return *p;
}
int main(int argc, char** argv) {
  int p;
  int A[10];
  int B[10];
  return (int)foo(&p, &A, &B, 10);
}
