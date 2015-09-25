#Crab-llvm#

<img src="https://upload.wikimedia.org/wikipedia/en/4/4c/LLVM_Logo.svg" alt="llvm logo" width=280 height=240 /> 
<img src="http://i.imgur.com/IDKhq5h.png" alt="crab logo" width=280 height=200 /> 



#About#

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

#License#

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
file.c --crab-print-invariants`.

- The option `--crab-print-invariants` displays all the invariants
inferred for each basic block in the `LLVM` bitcode.

- Users can also choose the abstract domain by typing the option
`--crab-domain`. The possible values are:

    - `int`: classical intervals
	- `ric`: reduced product of intervals and congruences
	- `zones`: difference-bound matrices
    - `term`: intervals with uninterpreted functions

- We also provide the option `--crab-track-lvl` to indicate the level
of precision. The possible values are: 

    - `int`: reasons about integer scalars (LLVM registers).
	- `ptr`: reasons about pointer addresses.	
    - `arr`: reasons about the contents of pointers and arrays.

   If the level is `ptr` then Crab-llvm reasons about pointer
   arithmetic but it does not translate neither LLVM loads nor stores.
   
   If the level is `arr` then Crab-llvm uses the heap analysis
   provided by `dsa-seahorn` to partition the heap into disjoint
   arrays (i.e., sequence of consecutive bytes). Each LLVM load and
   store is translated to an array read and write operation,
   respectively. Then, it will use an array domain provided by Crab
   whose base domain is the one selected by option
   `--crab-domain`. Unlike `int` and `ptr` which produce a concrete
   semantics (up to the selected level of precision), the level `arr`
   produces an abstract semantics where all memory contents are
   already over-approximated. Nevertheless, Crab's analyses can always
   refine this abstract semantics by using more precise pointer and/or
   value analyses.

   Regardless the level of precision, Crab-llvm can try to resolve
   indirect calls (i.e., function pointers) if `dsa-seahorn` is
   installed and enable option `--crab-devirt`.

- By default, all the analyses are run in an intra-procedural
  manner. Enable the option `--crab-inter` to run the inter-procedural
  version. Crab-llvm implements a standard two-phase algorithm in
  which the call graph is first traversed from the leaves to the root
  while computing summaries and then from the root the leaves reusing
  summaries. Each function is executed only once. The analysis is
  sound with recursive functions but very imprecise. The option
  `--crab-print-summaries` displays the summaries for each
  function. The inter-procedural analysis is specially important if
  reasoning about memory contents is desired.
  
#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)
* [Arie Gurfinkel](arieg.bitbucket.org)
* [Temesghen Kahsai](http://www.lememta.info/)
