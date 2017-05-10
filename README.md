# Crab-llvm #

<a href="https://travis-ci.org/caballa/crab-llvm"><img src="https://travis-ci.org/caballa/crab-llvm.svg?branch=master" title="Ubuntu 12.04 LTS 64bit, g++-4.8"/></a>

<img src="https://upload.wikimedia.org/wikipedia/en/4/4c/LLVM_Logo.svg" alt="llvm logo" width=280 height=200 /><img src="http://i.imgur.com/IDKhq5h.png" alt="crab logo" width=280 height=200 /> 

Crab-llvm is a static analyzer that computes inductive invariants for
LLVM-based languages based on the
[Crab](https://github.com/seahorn/crab) library.

# Installation #

Crab-llvm is written in C++ and uses heavily the Boost library. The
main requirements are:

- C++ compiler supporting c++11
- Boost
- GMP 
- MPFR (if `-DUSE_APRON=ON`)

In linux, you can install requirements typing the commands:

     sudo apt-get install libboost-all-dev libboost-program-options-dev
     sudo apt-get install libgmp-dev
     sudo apt-get install libmpfr-dev	

Then, the basic compilation steps are:

     mkdir build && cd build
     cmake -DCMAKE_INSTALL_PREFIX=_DIR_ ../
     cmake --build . --target crab && cmake ..
     cmake --build . --target llvm && cmake ..      
     cmake --build . --target install 


If you want Crab-llvm to reason about pointers and arrays you need to
download the following package at the root directory:

* [dsa-seahorn](https://github.com/seahorn/dsa-seahorn): ``` git clone https://github.com/seahorn/dsa-seahorn.git ```

DSA (Data Structure Analysis) is a heap analysis described
[here](http://llvm.org/pubs/2003-11-15-DataStructureAnalysisTR.ps).

Another optional but very recommended component is:

* [llvm-seahorn](https://github.com/seahorn/llvm-seahorn): ``` git clone https://github.com/seahorn/llvm-seahorn.git```

`llvm-seahorn` provides specialized versions of `InstCombine` and
`IndVarSimplify` LLVM passes as well as a LLVM pass to convert
undefined values into nondeterministic calls.

To include `dsa-seahorn` and `llvm-seahorn`, type instead:

     mkdir build && cd build
     cmake -DCMAKE_INSTALL_PREFIX=_DIR_ ../
     cmake --build . --target extra            
     cmake --build . --target crab && cmake ..
     cmake --build . --target llvm && cmake ..           
     cmake --build . --target install 

If you want to use the boxes domain then add `-DUSE_LDD=ON`.

If you want to use the apron domains then add `-DUSE_APRON=ON`.

To install `crab-llvm` with Boxes and Apron:

     mkdir build && cd build
     cmake -DCMAKE_INSTALL_PREFIX=_DIR_ -DUSE_LDD=ON -DUSE_APRON=ON ../
     cmake --build . --target extra                 
     cmake --build . --target crab && cmake ..
     cmake --build . --target ldd && cmake ..
     cmake --build . --target apron && cmake ..
     cmake --build . --target llvm && cmake ..                
     cmake --build . --target install 


To run some regression tests:

     cmake --build . --target tests

To run tests you need to install `lit` and `OutputCheck`. In Linux:

```
$ apt-get install python-pip
$ pip install lit
$ pip install OutputCheck
```

# Crab-llvm architecture #

![Crab-Llvm Architecture](https://github.com/caballa/crab-llvm/blob/master/CrabLlvm_arch.jpg?raw=true "Crab-Llvm Architecture")

# Usage #

Consider the program `test.c`:

```c	 
    extern void __CRAB_assume (int);
    extern int __CRAB_nd();
    extern void __CRAB_nonop2(int, int);

    int main() {
       int k = __CRAB_nd();
       int n = __CRAB_nd();
       __CRAB_assume (k > 0);
       __CRAB_assume (n > 0);

       int x = k;
       int y = k;
       while (x < n) {
           x++;
           y++;
       }

       // We would like to see the relationship between x and y
       // at the end of this function. However, at this program
       // point x and y are dead so LLVM will kill them. One trick
       // is to use an external function that takes x and y as
       // parameters to keep them alive.
	   
       __CRAB_nonop2(x,y);
       return 0;
    }
```

Crab-llvm provides a Python script called `crabllvm.py`. Type the
command:

    crabllvm.py test.c

The output should be something like this:

	Function main
	
	_1: {_3-x.0<=0, y.0-x.0<=0, x.0-_3<=0, y.0-_3<=0, _3-y.0<=0, x.0-y.0<=0}
	_x.0: {_3-x.0<=0, y.0-x.0<=0, _3-y.0<=0, x.0-y.0<=0}
	_12: {_br2-y.0<=0, _13-y.0<=0, _3-y.0<=-1, x.0-y.0<=0, x.0-_5<=0, _3-_5<=-1, y.0-_5<=0, _13-_5<=0,
          _br2-_5<=0, x.0-_13<=0, _3-_13<=-1, y.0-_13<=0, _br2-_13<=0, y.0-_br2<=0, _3-_br2<=-1, x.0-_br2<=0,
          _13-_br2<=0, _13-x.0<=0, _br2-x.0<=0, _3-x.0<=-1, y.0-x.0<=0}
	_y.0.lcssa: {_3-x.0<=0, y.0-x.0<=0, _5-x.0<=0, y.0.lcssa-x.0<=0, x.0.lcssa-x.0<=0, _3-y.0<=0, x.0-y.0<=0,
                 _5-y.0<=0, y.0.lcssa-y.0<=0, x.0.lcssa-y.0<=0, y.0-y.0.lcssa<=0, _3-y.0.lcssa<=0,
                 x.0-y.0.lcssa<=0, _5-y.0.lcssa<=0, x.0.lcssa-y.0.lcssa<=0, x.0-x.0.lcssa<=0, _3-x.0.lcssa<=0,
                 y.0-x.0.lcssa<=0, _5-x.0.lcssa<=0, y.0.lcssa-x.0.lcssa<=0}

It shows the invariants inferred for function `main`. The format of
this output is:

	bb_1: invariants_1
	...
	bb_n: invariants_n

where `bb` is a basic block identifier and `invariants` is a
conjunction of linear constraints over program variables that hold at
the entry of the basic block `bb`.

Note that unfortunately there is not a direct translation from the
basic block identifiers to the original C program. Moreover, the
variable names might not correspond to the C variable names. The
reason is that Crab-llvm does not analyze C but instead the
corresponding [LLVM](http://llvm.org/) bitcode generated after
compiling the C program with [Clang](http://clang.llvm.org/).

To help users understanding the invariants Crab-llvm provides an
option to visualize the CFG of the function described in terms of the
LLVM bitcode. Type the command:

    crabllvm.py test.c --llvm-view-cfg

and you should see a screen with a similar CFG to this one:

   <img src="http://seahorn.github.io/images/test.c.dot.png" alt="LLVM CFG of test.c" width=375 height=400 />

Since we are interested at the relationships between `x` and `y` after
the loop, the LLVM basic block of interest is `_y.0.lcssa` and the
variables are `x.0.lcssa` and `y.0.lcssa`, which are simply renamings
of the loop variables `x.0` and `y.0`, respectively.

With this information, we can look back at the invariants inferred by
our tool and see the linear constraints:

    x.0.lcssa-y.0.lcssa<=0, ... , y.0.lcssa-x.0.lcssa<=0

that implies the desired invariant `x.0.lcssa` = `y.0.lcssa`.
	

Crab-llvm analyzes programs with the Zones domains as default abstract
domain. Users can choose the abstract domain by typing the option
`--crab-domain=VAL`. The possible values of `VAL` are:

- `int`: intervals
- `ric`: reduced product of `int` and congruences
- `term-int`: `int` with uninterpreted functions
- `zones-sparse`: zones domain using sparse difference-bound matrices (DBM)
- `zones-split`: zones domain using sparse DBM in split normal form
- `opt-oct-apron`: Elina's optimized octagon domains (only if `-DUSE_APRON=ON`)
- `pk-apron`: Apron's polka domain (only if `-DUSE_APRON=ON`)
- `boxes`: disjunctive intervals based on LDDs (only if `-DUSE_LDD=ON`)
- `dis-int`: disjunctive intervals based on Clousot's DisInt domain
- `term-dis-int`: `dis-int` with uninterpreted functions
- `rtz`: reduced product of `term-dis-int` with `zones-split`.
- `adapt-rtz`: choose, based on the number of live variables, between `int` and `rtz`.

For domains without narrowing operator (for instance `boxes`,
`dis-int`, and `pk-apron`), you need to set the option:
	
    --crab-narrowing-iterations=N

where `N` is the number of descending iterations (e.g., `N=2`).

You may want also to set the option:
	
	--crab-widening-delay=N

where `N` is the number of fixpoint iterations before triggering
widening (e.g., `N=1`).
	   
The widening operators do not use thresholds by default. To use them,
type the option

	--crab-widening-jump-set=N

where `N` is the maximum number of thresholds.

We also provide the option `--crab-track=VAL` to indicate the level of
abstraction. The possible values of `VAL` are:

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
   option `--crab-singleton-aliases` is enabled then Crab-llvm
   translates global singleton heaplets to scalar variables.

Crab-llvm can resolve indirect calls by enabling option
`--devirt-functions`. For C++ programs, it might be useful the option
`--lower-invoke`.

By default, all the analyses are run in an intra-procedural
manner. Enable the option `--crab-inter` to run the inter-procedural
version. Crab-llvm implements a standard two-phase algorithm in which
the call graph is first traversed from the leaves to the root while
computing summaries and then from the root the leaves reusing
summaries. Each function is executed only once. The analysis is sound
with recursive functions but very imprecise. The option
`--crab-print-summaries` displays the summaries for each function. The
inter-procedural analysis is specially important if reasoning about
memory contents is desired.

Finally, to make easier the communication with other LLVM-based tools,
Crab-llvm can output the invariants by inserting them into the LLVM
bitcode via `verifier.assume` instructions. The option
`--crab-add-invariants=block-entry` injects the invariants that hold
at each basic block entry while option
`--crab-add-invariants=after-load` injects the invariants that hold
right after each LLVM load instruction. The option `all` injects
invariants in all above locations. To see the final LLVM bitcode just
add the option `-o out.bc`.
  
Consider the next program:

```c
    extern int __CRAB_nd();
    int a[10];
    int main (){
       int i;
       for (i=0;i<10;i++) {
         if (__CRAB_nd ())
            a[i]=0;
         else 
            a[i]=5;
       }
       int res = a[i-1];
       return res;
    }
```

and type

    crabllvm.py test.c --crab-live --crab-track=arr --crab-add-invariants=all -o test.crab.bc
    llvm-dis test.crab.bc

The content of `test.crab.bc` should be similar to:

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
       %_5 = call i32 (...)* @__CRAB_nd() #2
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

The special thing about the above LLVM bitcode is the existence of
`@verifier.assume` instructions. For instance, the instruction
`@verifier.assume(i1 %crab_2)` indicates that `%i.0` is between 0 and
10 at the loop header. Also, `@verifier.assume(i1 %crab_23)` indicates
that the result of the load instruction at block `loop.exit` is
between 0 and 5.

# Known limitations of the translation from bitcode to Crab CFG #

- Translation only covers integers and we use unlimited integers so no
  machine arithmetic is considered.
  
- The translation abstracts pointer operations to arithmetic
  operations that keep track only of numerical offsets. This is
  because the original use was proving absence of buffer overflows. We
  are working on having a more general translation. 
