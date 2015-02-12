# Llvm-Ikos #

Llvm-Ikos is a static analyzer that computes inductive invariants
using Ikos (a library of abstract domains and fixpoint algorithms
developed at NASA Ames) from LLVM-based languages.

# Prerequisites #

- The C++ compiler must support c++11

- Boost and gmp

- This projects uses another project Ikos-core which is currently
private. Although this will change soon, for being able to download
ikos-core you need to (a) be given permissions, and (b)  generate a RSA ID
in order to use ssh with bitbucket without being prompting with your
bitbucket password. To create your own RSA ID follow this link:
`
https://bitbucket.org/samirmenon/scl-manips-group/wiki/install/ssh_access.wiki 
`

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