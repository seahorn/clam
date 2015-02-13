# Llvm-Ikos #

Llvm-Ikos is a static analyzer that computes inductive invariants
using Ikos (a library of abstract domains and fixpoint algorithms
developed at NASA Ames) from LLVM-based languages.

Llvm-Ikos provides two tools: `llvmpp` and `llvmikos`. The former is a
LLVM bytecode preprocessor that applies optimizations to make easier
the task of static analysis. The later converts LLVM bitecode into a
custom CFG and computes invariants on it. The use of `llvmpp` is
optional but highly recommended with real programs.

# Prerequisites #

- The C++ compiler must support c++11

- Boost and gmp

# Installation #

`
mkdir build && cd build  && cmake -G Ninja ../
`

# Usage #

- `clang -c -emit-llvm file.c -o file.bc` 

- `llvmpp file.bc -o file.pp.bc` (optional)

- `llvmpp file.bc -ikos-inline-all -o file.pp.bc` (for fully inlining)

- `llvmikos file.pp.bc -ikos-domain=ZONES -ikos-live -ikos-answer`


#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)
* [Arie Gurfinkel](arieg.bitbucket.org)
* [Temesghen Kahsai](http://www.lememta.info/)