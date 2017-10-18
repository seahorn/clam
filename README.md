# Crab-llvm #

<a href="https://travis-ci.org/seahorn/crab-llvm"><img src="https://travis-ci.org/seahorn/crab-llvm.svg?branch=master" title="Ubuntu 12.04 LTS 64bit, g++-5"/></a>

<img src="https://upload.wikimedia.org/wikipedia/en/4/4c/LLVM_Logo.svg" alt="llvm logo" width=280 height=200 /><img src="http://i.imgur.com/IDKhq5h.png" alt="crab logo" width=280 height=200 /> 

Crab-llvm is a static analyzer that computes inductive invariants for
LLVM-based languages based on
the [Crab](https://github.com/seahorn/crab) library. It currently
supports LLVM 3.8.

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


The next two packages are highly recommended:

* [llvm-dsa](https://github.com/seahorn/llvm-dsa): ``` git clone https://github.com/seahorn/llvm-dsa.git ```

  `llvm-dsa` is the legacy DSA implementation
  from [PoolAlloc](https://llvm.org/svn/llvm-project/poolalloc/). DSA
  (Data Structure Analysis) is a heap analysis
  described
  [here](http://llvm.org/pubs/2003-11-15-DataStructureAnalysisTR.ps)
  and it is used by Crab-llvm to disambiguate the heap.
  
* [llvm-seahorn](https://github.com/seahorn/llvm-seahorn): ``` git clone https://github.com/seahorn/llvm-seahorn.git```

`llvm-seahorn` provides specialized versions of `InstCombine` and
`IndVarSimplify` LLVM passes as well as a LLVM pass to convert
undefined values into nondeterministic calls.

To include `llvm-dsa` and `llvm-seahorn`, type instead:

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

     cmake --build . --target test-simple

To run tests you need to install `lit` and `OutputCheck`. In Linux:

```
$ apt-get install python-pip
$ pip install lit
$ pip install OutputCheck
```

# Crab-llvm architecture #

![Crab-Llvm Architecture](https://github.com/seahorn/crab-llvm/blob/master/CrabLlvm_arch.jpg?raw=true "Crab-Llvm Architecture")

# Demo 1 #

Consider the program `test.c`:

```c
extern void __CRAB_assume (int);
extern void __CRAB_assert(int);
extern int  __CRAB_nd(void);

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
  __CRAB_assert (x >= y);
  __CRAB_assert (x <= y);  
  return 0;
}

```

Crab-llvm provides a Python script called `crabllvm.py`. Type the
command:

    crabllvm.py test.c

**Important:** the first thing that `crabllvm.py` does is to compile
  the C program into LLVM bitcode by using Clang. Since Crab-llvm is
  based on LLVM 3.8, the version of clang must be 3.8 as well. 


If the above command succeeds, then the output should be something
like this:

```
Invariants for main
_1:
/**
  INVARIANTS: ({}, {})
**/
  _2 =* ;
  _3 =* ;
  _4 = (-_2 <= -1);
  zext _4:1 to _call:32;
  _6 = (-_3 <= -1);
  zext _6:1 to _call1:32;
  x.0 = _2;
  y.0 = _2;
/**
  INVARIANTS: ({}, {_call -> [0, 1], _call1 -> [0, 1], _2-x.0<=0, y.0-x.0<=0, x.0-_2<=0, y.0-_2<=0, _2-y.0<=0, x.0-y.0<=0})
**/
--> [_x.0;]
_x.0:
/**
  INVARIANTS: ({}, {_call -> [0, 1], _call1 -> [0, 1], _2-x.0<=0, y.0-x.0<=0, _2-y.0<=0, x.0-y.0<=0})
**/
--> [__@bb_1;__@bb_2;]
__@bb_1:
  assume (-_3+x.0 <= -1);
--> [_10;]
_10:
/**
  INVARIANTS: ({}, {_call -> [0, 1], _call1 -> [0, 1], _2-x.0<=0, y.0-x.0<=0, _2-y.0<=0, x.0-y.0<=0, x.0-_3<=-1, _2-_3<=-1, y.0-_3<=-1})
**/
  _11 = x.0+1;
  _br2 = y.0+1;
  x.0 = _11;
  y.0 = _br2;
/**
  INVARIANTS: ({}, {_call -> [0, 1], _call1 -> [0, 1], _br2-y.0<=0, _11-y.0<=0, _2-y.0<=-1, x.0-y.0<=0, x.0-_3<=0, _2-_3<=-1, y.0-_3<=0, _11-_3<=0, _br2-_3<=0, x.0-_11<=0, _2-_11<=-1, y.0-_11<=0, _br2-_11<=0, y.0-_br2<=0, _2-_br2<=-1, x.0-_br2<=0, _11-_br2<=0, _11-x.0<=0, _br2-x.0<=0, _2-x.0<=-1, y.0-x.0<=0})
**/
--> [_x.0;]
__@bb_2:
  assume (_3-x.0 <= 0);
  y.0.lcssa = y.0;
  x.0.lcssa = x.0;
--> [_y.0.lcssa;]
_y.0.lcssa:
/**
  INVARIANTS: ({}, {_call -> [0, 1], _call1 -> [0, 1], _2-x.0<=0, y.0-x.0<=0, _3-x.0<=0, y.0.lcssa-x.0<=0, x.0.lcssa-x.0<=0, _2-y.0<=0, x.0-y.0<=0, _3-y.0<=0, y.0.lcssa-y.0<=0, x.0.lcssa-y.0<=0, y.0-y.0.lcssa<=0, _2-y.0.lcssa<=0, x.0-y.0.lcssa<=0, _3-y.0.lcssa<=0, x.0.lcssa-y.0.lcssa<=0, x.0-x.0.lcssa<=0, _2-x.0.lcssa<=0, y.0-x.0.lcssa<=0, _3-x.0.lcssa<=0, y.0.lcssa-x.0.lcssa<=0})
**/
  _14 = (y.0.lcssa-x.0.lcssa <= 0);
  zext _14:1 to _call3:32;
  assert (-_call3 <= -1);
  _16 = (-y.0.lcssa+x.0.lcssa <= 0);
  zext _16:1 to _call4:32;
  assert (-_call4 <= -1);
  @V_17 = 0;
  return @V_17;
/**
  INVARIANTS: ({_14 -> true; _16 -> true}, {_call -> [0, 1], _call1 -> [0, 1], _call3 -> [1, 1], _call4 -> [1, 1], @V_17 -> [0, 0], _2-x.0<=0, y.0-x.0<=0, _3-x.0<=0, y.0.lcssa-x.0<=0, x.0.lcssa-x.0<=0, _2-y.0<=0, x.0-y.0<=0, _3-y.0<=0, y.0.lcssa-y.0<=0, x.0.lcssa-y.0<=0, y.0-y.0.lcssa<=0, _2-y.0.lcssa<=0, x.0-y.0.lcssa<=0, _3-y.0.lcssa<=0, x.0.lcssa-y.0.lcssa<=0, x.0-x.0.lcssa<=0, _2-x.0.lcssa<=0, y.0-x.0.lcssa<=0, _3-x.0.lcssa<=0, y.0.lcssa-x.0.lcssa<=0})
**/
--> []
```

It shows the Control-Flow Graph analyzed by Crab together with the invariants inferred for function `main` that hold at the entry and and the exit of each basic block. 

Note that Crab-llvm does not provide a translation from the basic
block identifiers and variable names to the original C program. The
reason is that Crab-llvm does not analyze C but instead the
corresponding [LLVM](http://llvm.org/) bitcode generated after
compiling the C program with [Clang](http://clang.llvm.org/). To help
users understanding the invariants Crab-llvm provides an option to
visualize the CFG of the function described in terms of the LLVM
bitcode:

    crabllvm.py test.c --llvm-view-cfg

and you should see a screen with a similar CFG to this one:

   <img src="https://github.com/seahorn/crab-llvm/blob/master/demo/test.c.dot.png" alt="LLVM CFG of test.c" width=375 height=400 />

Since we are interested at the relationships between `x` and `y` after
the loop, the LLVM basic block of interest is `_y.0.lcssa` and the
variables are `x.0.lcssa` and `y.0.lcssa`, which are simply renamings
of the loop variables `x.0` and `y.0`, respectively.

With this information, we can look back at the invariants inferred by
our tool and see the linear constraints:

    x.0.lcssa-y.0.lcssa<=0, ... , y.0.lcssa-x.0.lcssa<=0

that implies the desired invariant `x.0.lcssa` = `y.0.lcssa`.


# Crab Options #


Crab-llvm analyzes programs with the `zones` domain as the default
abstract domain. Users can choose the abstract domain by typing the
option `--crab-domain=VAL`. The possible values of `VAL` are:

- `int`: intervals
- `ric`: reduced product of `int` and congruences
- `term-int`: `int` with uninterpreted functions
- `zones`: zones domain using sparse DBM in split normal form
- `oct`: Elina's optimized octagon domains (only if `-DUSE_APRON=ON`)
- `pk`: Apron's polka domain (only if `-DUSE_APRON=ON`)
- `boxes`: disjunctive intervals based on LDDs (only if `-DUSE_LDD=ON`)
- `dis-int`: disjunctive intervals based on Clousot's DisInt domain
- `term-dis-int`: `dis-int` with uninterpreted functions
- `rtz`: reduced product of `term-dis-int` with `zones-split`.

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

- `int`: reasons about integer and boolean scalars (LLVM registers).
- `ptr`: reasons about `int` and pointer offsets.	
- `arr`: reasons about `ptr` and contents of pointers and arrays.

   If the level is `arr` then Crab-llvm uses the heap abstraction
   provided by `llvm-dsa` to partition the heap into disjoint
   regions. Each region is mapped to an array, and each LLVM load
   and store is translated to an array read and write operation,
   respectively. Then, it will use an array domain provided by Crab
   whose base domain is the one selected by option `--crab-domain`. If
   option `--crab-singleton-aliases` is enabled then Crab-llvm
   translates global singleton regions to scalar variables.

By default, all the analyses are run in an intra-procedural
manner. Enable the option `--crab-inter` to run the inter-procedural
version. Crab-llvm implements a standard two-phase algorithm in which
the call graph is first traversed from the leaves to the root while
computing summaries and then from the root the leaves reusing
summaries. Each function is executed only once. The analysis is sound
with recursive functions but imprecise. The option
`--crab-print-summaries` displays the summaries for each function. The
inter-procedural analysis is specially important if reasoning about
memory contents is desired.

Crab-llvm provides the option `--crab-backward` to enable an iterative
forward-backward analysis that might produce more precise results. The
backward analysis computes *necessary preconditions* of the error
states (if program is annotated with assertions) which are used to
refine the set of initial states so that the forward analysis can
refine its results.

Note that apart from inferring invariants or preconditions, Crab-llvm
allows checking for assertions. To do that, programs must be annotated
with `__CRAB_assert(c)` where `c` is any expression that evaluates to
a boolean. Note that `__CRAB_assert` must be defined as an `extern`
function so that Clang does not complain:

    extern void __CRAB_assert(int);

Then, you can type:

    crabllvm.py test.c --crab-check=assert

and you should see something like this:

    user-defined assertion checker using SplitDBM
    2  Number of total safe checks
    0  Number of total error checks
    0  Number of total warning checks

Finally, to make easier the communication with other LLVM-based tools,
Crab-llvm can output the invariants by inserting them into the LLVM
bitcode via `verifier.assume` instructions. The option
`--crab-add-invariants=block-entry` injects the invariants that hold
at each basic block entry while option
`--crab-add-invariants=after-load` injects the invariants that hold
right after each LLVM load instruction. The option `all` injects
invariants in all above locations. To see the final LLVM bitcode just
add the option `-o out.bc`.

# Demo 2 #

Consider the next program:

```c
    extern int __CRAB_nd(void);
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

    crabllvm.py test.c --crab-track=arr --crab-add-invariants=all -o test.crab.bc
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

- Ignore floating point operations.

- Interval arithmetic ignores wraparound which obviously affects the
  soundness of the analyzer.

# Analysis limitations #

Well, there are many. Most of these limitations are coming from
Crab. Here some of them:

- Crab numerical domains mostly reason about linear arithmetic.
 
- There are several Crab numerical domains that compute disjunctive
  invariants but they are still limited in terms of expressiveness to
  keep them tractable.

- The interprocedural analysis is summary-based but it's
  context-insensitive.
  
- Crab does not provide any pointer or shape analysis but it provides
  a simple nullity analysis that can tell whether pointer may be null
  or not.

- Crab-llvm can reason about pointer's contents only if `llvm-dsa` can
  infer statically that a pointer points to a memory region that
  behaves as a C arrays (i.e., consecutive sequence of bytes where
  elements must have compatible types and offset must be multiple of
  the element type size). Once a logical array has been identified,
  Crab-llvm uses one of the Crab array domains to reason about their
  contents. Currently, it only supports array smashing.
	  
  
