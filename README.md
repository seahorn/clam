#Crab-llvm#

Crab-llvm is a static analyzer that computes inductive invariants
based on [Crab](https://github.com/seahorn/crab) for LLVM-based
languages.

Crab-llvm provides two standalone tools: `llvmpp` and `crabllvm`:

- `llvmpp`: is a LLVM bytecode preprocessor that applies optimizations
to make easier the task of static analysis.

- `crabllvm`: converts LLVM bitcode into a language-independent CFG
  and computes invariants from it.

The use of `llvmpp` is optional but highly recommended with large
programs.

## License ##

Crab-llvm is distributed under MIT license. See
[LICENSE.txt](LICENSE.txt) for details.

#Installation#

Crab-llvm is written in C++ and uses heavily the Boost library. You will need:

- C++ compiler supporting c++11
- Boost and Gmp

If you want Crab-llvm to reason about pointers and arrays you need to
download the following package at the root directory:

* [dsa-seahorn](https://github.com/seahorn/dsa-seahorn): ``` git clone https://github.com/seahorn/dsa-seahorn.git ```

Then, the compilation steps are:

1. ```mkdir build ; cd build```
2. ```cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=my_install_dir ../```

#Usage#

Crab-llvm provides a python script called `crabllvm.py` to interact
with users. Given a C program, users just need to type: `crabllvm.py
file.c --crab-answer`.

- The option `--crab-answer` displays all the invariants inferred for
each basic block in the `LLVM` bitcode.

- Users can also choose the abstract domain by typing the option
`--crab-domain`. The possible values are:

    - `int`: classical intervals
	- `ric`: reduced product of intervals and congruences
	- `zones`: difference-bound matrices
    - `term`: intervals with uninterpreted functions

- We also provide the option `--crab-track-lvl` to indicate the level
of precision. The possible values are: 

    - `reg`: reasons about integer scalars (LLVM registers).
	- `ptr`: reasons about pointer addresses.	
    - `mem`: reasons about the contents of pointers and
arrays.

If the level is `mem` then Crab-llvm simply uses an array smashing
domain whose base domain is the one selected by option
`--crab-domain`.

- By default, all the analyses are run in an intra-procedural
  manner. Enable the option `--crab-inter` to run the inter-procedural
  version. Crab-llvm implements a standard two-phase algorithm in
  which the call graph is first traversed from the leaves to the root
  while computing summaries and then from the root the leaves reusing
  summaries. Each function is executed only once. The analysis is
  sound with recursive functions but very imprecise.
  
#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)
* [Arie Gurfinkel](arieg.bitbucket.org)
* [Temesghen Kahsai](http://www.lememta.info/)
