# LlvmIkos #

This project computes inductive invariants using Ikos (a library of
abstract domains and fixpoint algorithms developed at NASA Ames) from
LLVM-based languages.

# Prerequisites #

- The C++ compiler must support c++11

- Boost and gmp

- This projects uses another project ikos-core which is currently
private. Although this will change soon, for being able to download
ikos-core you need (a) give permissions, and (b) to generate a RSA ID
in order to use ssh with bitbucket without being prompting with your
bitbucket password. To create your own RSA ID follow this link:

`
https://bitbucket.org/samirmenon/scl-manips-group/wiki/install/ssh_access.wiki 
`

To ask for permissions contact me.


# Installation #

`
mkdir build && cd build  && cmake -G Ninja ../
`

# Usage #

- `clang -c -emit-llvm file.c -o file.bc`

- `llvmpp file.bc -o file.pp.bc` (optional)

- `llvmikos file.pp.bc -ikos-domain=ZONES`


#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)

#Credits#

The preprocessor is from
[SeaHorn](https://bitbucket.org/lememta/seahorn/wiki/Home) developed
together with Arie Gurfinkel and Temesghen Kahsai