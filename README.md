#Ikos-llvm#

Ikos-llvm is a static analyzer that computes inductive invariants
using Ikos (a library of abstract domains and fixpoint algorithms
developed at NASA Ames) from LLVM-based languages.

Ikos-llvm provides two standalone tools: `llvmpp` and `llvmikos`:

- `llvmpp`: is a LLVM bytecode preprocessor that applies optimizations
to make easier the task of static analysis.

- `llvmikos`: converts LLVM bitecode into a language-independent CFG
  and computes invariants on it.

The use of `llvmpp` is optional but highly recommended with large
programs.

#Prerequisites#

- The C++ compiler must support c++11

- Boost and gmp

*This branch is using a private version of `ikos-core`*. We will merge
 the private version with the public one in github as soon as
 possible.

#Compilation#

First, if you want `Ikos-llvm` to reason about pointers and arrays you
need to download the following package at the root directory:

* [dsa-seahorn](https://github.com/seahorn/dsa-seahorn): ``` git clone https://github.com/seahorn/dsa-seahorn.git ```

Then, the compilation steps are:

1. ```mkdir build ; cd build```
2. ```cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=my_install_dir ../```

This will install in `my_install_dir/bin` two executables: `llvmikos`
and `llvmpp`.

#Usage#

First, we need to compile a program into `LLVM` bitecode.
 
- `clang -c -emit-llvm file.c -o file.bc` 

The version of `clang` must be compatible with the `LLVM` version used
to build `llvmikos` and `llvmpp`. To see the `LLVM` version, type
`llvmikos --version`.

Note that we can also use `gcc` together with `DragonEgg`.

Then, we can optionally run the preprocessor on the generated
bitecode:

- `llvmpp file.bc -o file.pp.bc` 

The preprocessor can also inline all functions to provide
context-sensitivity:

- `llvmpp file.bc -ikos-inline-all -o file.pp.bc` 

We can analyze the program by choosing a particular abstract domain
and by considering only live variables:

- `llvmikos file.pp.bc -ikos-domain=ZONES -ikos-live -ikos-answer`

The option `-ikos-answer` displays all the invariants inferred for
each basic block in the `LLVM` bitecode.

We also provide the option `-ikos-track-lvl` to indicate the level of
precision. The possible values are: `reg`, `ptr`, and `mem`. The level
`reg` reasons about integer scalars. The level `ptr` reasons about
pointer addresses while the level `mem` reasons about the contents of
pointers and arrays. If the latter is selected, `llvmikos` inserts
`assume` instructions in the bitecode representing instantiations of
universally quantified invariants. Although very simple this allows us
to pass this invariants to SeaHorn.

We have an experimental option `ikos-concur` that allows us to analyze
concurrent programs using a rely-guarantee approach. The concurrency
model is based on threads that can only communicate via global
variables. Currently, we apply a global fixpoint between threads where
interferences are updated in a sound manner. Currently, interferences
are abstracted in a flow-insensitive manner.

Finally, the option `-ikos-inter-proc` builds CFGs including
information about function definitions and call sites. Note that it
does not perform inter-procedural analysis by itself. In fact,
inter-procedural analyses beyond inlining (e.g., based on summaries)
are not currently implemented.

#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)
* [Arie Gurfinkel](arieg.bitbucket.org)
* [Temesghen Kahsai](http://www.lememta.info/)
