# Ikos-llvm#

Ikos-llvm is a static analyzer that computes inductive invariants
using Ikos (a library of abstract domains and fixpoint algorithms
developed at NASA Ames) from LLVM-based languages.

Ikos-llvm provides two standalone tools: `llvmpp` and `llvmikos`:

- `llvmpp`: is a LLVM bytecode preprocessor that applies optimizations to make easier
the task of static analysis. 

- `llvmikos`:  converts LLVM bitecode into a language-independent CFG and computes invariants on it. 

The use of `llvmpp` is optional but highly recommended with large programs.

# Prerequisites #

- The C++ compiler must support c++11

- Boost and gmp

# Installation #

`
mkdir build && cd build  && cmake -G Ninja ../
`

# Usage #

First, we need to compile a program into `LLVM` bitecode.
 
- `clang -c -emit-llvm file.c -o file.bc` 

We are using currently `LLVM 3.2` so the version of `clang`must be compatible with it.
Note that we can also use `gcc` together with `DragonEgg`.

Then, we can optionally run the preprocessor on the generated bitecode:

- `llvmpp file.bc -o file.pp.bc` 

The preprocessor can also inline all functions to provide context-sensitivity:

- `llvmpp file.bc -ikos-inline-all -o file.pp.bc` 

Finally, we can analyze the program by choosing a particular abstract domain and by considering only live variables:

- `llvmikos file.pp.bc -ikos-domain=ZONES -ikos-live -ikos-answer`

The option `-ikos-answer` displays all the invariants inferred for each basic block in the `LLVM` bitecode.

#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)
* [Arie Gurfinkel](arieg.bitbucket.org)
* [Temesghen Kahsai](http://www.lememta.info/)