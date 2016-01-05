#Crab-llvm#

<img src="https://upload.wikimedia.org/wikipedia/en/4/4c/LLVM_Logo.svg" alt="llvm logo" width=280 height=200 /> 
<img src="http://i.imgur.com/IDKhq5h.png" alt="crab logo" width=280 height=200 /> 

#About#

Crab-llvm is a static analyzer that computes inductive invariants
based on [Crab](https://github.com/seahorn/crab) for LLVM-based
languages.

Crab-llvm provides two standalone tools: `crabllvmpp` and `crabllvm`:

- `crabllvmpp`: is a LLVM bytecode preprocessor that applies optimizations
to make easier the task of static analysis.

- `crabllvm`: converts LLVM bitcode into a language-independent CFG
  and computes invariants from it.

The use of `crabllvmpp` is optional but highly recommended with large
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

DSA (Data Structure Analysis) is a heap analysis described
[here](http://llvm.org/pubs/2003-11-15-DataStructureAnalysisTR.ps).

Another optional component used by `crabllvmpp` is:

* [llvm-seahorn](https://github.com/seahorn/llvm-seahorn): ``` git clone https://github.com/seahorn/llvm-seahorn.git```

`llvm-seahorn` provides specialized versions of `InstCombine` and
`IndVarSimplify` LLVM passes as well as a LLVM pass to convert
undefined values into nondeterministic calls.

Then, the compilation steps are:

1. ```mkdir build ; cd build```
2. ```cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=my_install_dir ../```

If you want to use the boxes domain then add to step 2 the option `-DUSE_LDD=ON`.

If you want to use the apron domains then add to step 2 the option `-DUSE_APRON=ON`.

#How Crab-llvm works#

At its core, Crab-llvm is simply a translator from LLVM bitecode to
the CFG language understood by
[Crab](https://github.com/seahorn/crab). The actual analysis work is
delegated to Crab.

The main task of the translation is to replace PHI nodes with Crab
assignments and LLVM Branch instructions into Crab assume
statements. This part of the translation is quite standard in abstract
interpreters which are usually unaware of PHI nodes.

The translation also performs *code abstractions* based on the neat
idea of *Abstract Compilation*
([Global Flow Analysis as Practical Compilation Tool](http://oa.upm.es/14288/1/HERME_A_1992-1.pdf)),
an application of Abstract Interpretation where instead of analyzing a
program by executing its concrete code over abstract data the code
itself is abstracted into *abstract code*.

The main benefit of abstract compilation is that part of the reasoning
can be done at compile time instead of analysis time. This sometimes
produces shorter analysis time and it can also simplify the
implementation of the underlying static analyzer. The main code
abstractions done by Crab-llvm can be chosen by the user through the
option `crab-track` (see next section for details) and are:

- If the abstraction level includes only integers the translation will
  cover only instructions with operands of integer type.

- If the abstraction level includes pointers then in addition to
  integer scalars it will translate instructions that compute pointer
  numerical offsets. All pointers are abstracted to their numerical
  offsets *ignoring* their addresses.
	  
- Finally, if the abstraction level includes memory contents, in
  addition to the previous abstractions, load and stores are
  translated using a memory abstraction based on DSA. This abstraction
  consists of partitioning the heap into a finite set of disjoint
  heaplets so that an array abstract domain can be used to reason
  about heaplets by mapping each heaplet to an array.

Note that these code abstractions complement to existing abstract
domains so they are not replacements. Note also that the code
abstraction for memory contents can be as powerful as DSA is. For
instance, DSA is less powerful than a shape analysis. Moreover, DSA is
flow-insensitive. Therefore, any flow-sensitive pointer abstract
domain can produce more precise results. Another important restriction
of this code abstraction is that heaplets that have only compatible
types and are aligned are translated.

#Usage#

Crab-llvm provides a python script called `crabllvm.py` to interact
with users. Given a C program, users just need to type: `crabllvm.py
file.c --crab-print-invariants`.

- The option `--crab-print-invariants` displays all the invariants
inferred for each basic block in the `LLVM` bitcode.

- Users can also choose the abstract domain by typing the option
`--crab-domain`. The possible values are:

    - `int`: classical intervals
	- `ric`: intervals with congruences
	- `zones`: zones using sparse difference-bound matrices
	- `szones`: zones using split difference-bound matrices 
	- `opt-oct-apron`: octagons (only if `-DUSE_APRON=ON`)
	- `pk-apron`: polyhedra (only if `-DUSE_APRON=ON`)
	- `boxes`: disjunctive intervals based on ldds (only if `-DUSE_LDD=ON`)
	- `dis-int`: disjunctive intervals based on disjunctive completion
    - `term-int`: intervals with uninterpreted functions
	- `term-dis-int`: disjunctive intervals with uninterpreted functions
	- `num`: choose (based on the number of live variables) between `int` and `zones`

	For domains without narrowing operator (for instance currently
    `boxes`, `dis-int`, and `pk-apron`), you need to set the option:
	
       - `--crab-narrowing-iterations=N`

       where `N` is the number of descending iterations (e.g., `N=2`).

	You may want also to set the option:
	
	   - `--crab-widening-threshold=N`

       where `N` is the number of fixpoint iterations before
       triggering widening (e.g., `N=1`). 
	   
    The widening operators do not use thresholds by default. To use
    them, type the option:

       - `--crab-widening-jump-set=N`

       where `N` is the maximum number of thresholds. 

- We also provide the option `--crab-track` to indicate the level of
abstraction. The possible values are:

    - `int`: reasons about integer scalars (LLVM registers).
	- `ptr`: reasons about integers and pointer offsets.	
    - `arr`: reasons about integers, offsets, and contents of pointers and arrays.

   If the level is `ptr` then Crab-llvm reasons about pointer
   offsets but it will abstract away pointer addresses.
   
   If the level is `arr` then Crab-llvm uses the heap analysis
   provided by `dsa-seahorn` to partition the heap into disjoint
   heaplets. Each heaplet is mapped to an array, and each LLVM load
   and store is translated to an array read and write operation,
   respectively. Then, it will use an array domain provided by Crab
   whose base domain is the one selected by option `--crab-domain`. If
   option `--crab-singleton-aliases` is enabled then Crab-Llvm
   translates global singleton heaplets to scalar variables.

- Crab-llvm can resolve indirect calls by enabling option `--crab-devirt`.

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

- To make easier the communication with other LLVM-based tools,
  Crab-llvm can output the invariants by inserting them into the LLVM
  bitecode via `verifier.assume` instructions. The option
  `--crab-add-invariants-at-entries` injects the invariants that hold
  at each basic block entry while option
  `--crab-add-invariants-after-loads` injects the invariants that hold
  right after each LLVM load instruction. To see the final LLVM
  bitecode just add the option `-o out.bc`.
  
Take the following program:

```c

    extern int nd ();
    int a[10];
    int main (){
       int i;
       for (i=0;i<10;i++) {
         if (nd ())
            a[i]=0;
         else 
            a[i]=5;
	   }		 
       int res = a[i-1];
       return res;
    }
```

and type `crabllvm.py test.c --crab-live --crab-track=arr --crab-add-invariants-at-entries --crab-add-invariants-after-loads -o test.crab.bc`. The content of `test.crab.bc` should be similar to this:


```

    define i32 @main() #0 {
    entry:
       br label %loop.header
    loop.header:   ; preds = %loop.body, %entry
       %i.0 = phi i32 [ 0, %entry ], [ %_br2, %loop.body ]
       %crab_2 = icmp ult i32 %i.0, 11
       call void @verifier.assume(i1 %crab_2) #2
       %_br1 = icmp slt i32 %i.0, 10
       br i1 %_br1, label %loop.body, label %loop.exit
    loop.body:   ; preds = %loop.header
       call void @verifier.assume(i1 %_br1) #2
       %crab_14 = icmp ult i32 %i.0, 10
       call void @verifier.assume(i1 %crab_14) #2
       %_5 = call i32 (...)* @nd() #2
       %_6 = icmp eq i32 %_5, 0
       %_7 = sext i32 %i.0 to i64
       %_. = getelementptr inbounds [10 x i32]* @a, i64 0, i64 %_7
       %. = select i1 %_6, i32 5, i32 0
       store i32 %., i32* %_., align 4
       %_br2 = add nsw i32 %i.0, 1
       br label %loop.header
    loop.exit:   ; preds = %loop.header
       %_11 = add nsw i32 %i.0, -1
       %_12 = sext i32 %_11 to i64
       %_13 = getelementptr inbounds [10 x i32]* @a, i64 0, i64 %_12
       %_ret = load i32* %_13, align 4
       %crab_23 = icmp ult i32 %_ret, 6
       call void @verifier.assume(i1 %crab_23) #2
       ret i32 %_ret
    }
```

The special thing about the above LLVM bitecode is the existence of
`@verifier.assume` instructions. For instance, the instruction
`@verifier.assume(i1 %crab_2)` indicates that `%i.0` is between 0 and
10 at the loop header. Also, `@verifier.assume(i1 %crab_23)` indicates
that the result of the load instruction at block `loop.exit` is
between 0 and 5.


#Known Limitations#

- Variadic functions are ignored.
- Floating point operations are ignored.
- Pointer addresses are abstracted by their numerical offsets. This
  means that if a shape/pointer abstract domain (e.g., Crab provides a
  pointer abstract domain) wants to be used the translation must be
  extended.
- ...

#People#

* [Jorge Navas](http://ti.arc.nasa.gov/profile/jorge/)
* [Arie Gurfinkel](arieg.bitbucket.org)
* [Temesghen Kahsai](http://www.lememta.info/)
