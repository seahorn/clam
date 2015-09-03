#Crab-llvm#

Crab-llvm is a static analyzer that computes inductive invariants
based on [Crab](https://github.com/seahorn/crab) from LLVM-based
languages.

Crab-llvm provides two standalone tools: `llvmpp` and `crabllvm`:

- `llvmpp`: is a LLVM bytecode preprocessor that applies optimizations
to make easier the task of static analysis.

- `crabllvm`: converts LLVM bitecode into a language-independent CFG
  and computes invariants from it.

The use of `llvmpp` is optional but highly recommended with large
programs.

#Prerequisites#

- The C++ compiler must support c++11

- Boost and gmp


#Compilation#

First, if you want `Crab-llvm` to reason about pointers and arrays you
need to download the following package at the root directory:

* [dsa-seahorn](https://github.com/seahorn/dsa-seahorn): ``` git clone https://github.com/seahorn/dsa-seahorn.git ```

Then, the compilation steps are:

1. ```mkdir build ; cd build```
2. ```cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=my_install_dir ../```

This will install in `my_install_dir/bin` two executables: `crabllvm`
and `llvmpp`.

#Usage#

First, we need to compile a program into `LLVM` bitecode.
 
- `clang -c -emit-llvm file.c -o file.bc` 

The version of `clang` must be compatible with the `LLVM` version used
to build `crabllvm` and `llvmpp`. To see the `LLVM` version, type
`crabllvm --version`.

Note that we can also use `gcc` together with `DragonEgg`.

Then, we can optionally run the preprocessor on the generated
bitecode:

- `llvmpp file.bc -o file.pp.bc` 

The preprocessor can also inline all functions to provide
context-sensitivity:

- `llvmpp file.bc -crab-inline-all -o file.pp.bc` 

We can analyze the program by choosing a particular abstract domain
and by considering only live variables:

- `crabllvm file.pp.bc -crab-domain=ZONES -crab-live -crab-answer`

The option `-crab-answer` displays all the invariants inferred for
each basic block in the `LLVM` bitecode.

We also provide the option `-crab-track-lvl` to indicate the level of
precision. The possible values are: `reg`, `ptr`, and `mem`. The level
`reg` reasons about integer scalars. The level `ptr` reasons about
pointer addresses while the level `mem` reasons about the contents of
pointers and arrays. If the latter is selected, `crabllvm` inserts
`assume` instructions in the bitecode representing instantiations of
universally quantified invariants. Although very simple this allows us
to pass array invariants to SeaHorn.

Finally, the option `-crab-inter-proc` builds CFGs including
information about function definitions and call sites. Note that it
does not perform inter-procedural analysis by itself. In fact,
inter-procedural analyses beyond inlining (e.g., based on summaries)
are not currently implemented.

#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)
* [Arie Gurfinkel](arieg.bitbucket.org)
* [Temesghen Kahsai](http://www.lememta.info/)
