# Clam: Llvm front-end for Crab #

<a href="https://github.com/seahorn/crab-llvm/actions"><img src="https://github.com/seahorn/crab-llvm/workflows/CI/badge.svg" title="Ubuntu 18.04 LTS 64bit, g++-6"/></a>


<img src="https://llvm.org/img/LLVMWyvernSmall.png" alt="llvm logo" width=280 height=200 /><img src="http://i.imgur.com/IDKhq5h.png" alt="crab logo" width=280 height=200 /> 

# Description # 

Clam is a static analyzer that computes inductive invariants for
LLVM-based languages based on
the [Crab](https://github.com/seahorn/crab) library. This branch
supports LLVM 10.

The available documentation can be found in
our [wiki](https://github.com/seahorn/clam/wiki/Home) and Crab [wiki](https://github.com/seahorn/crab/wiki).

# Docker #

You can get the latest binary
from [Docker Hub](https://hub.docker.com/) (nightly built) using the
command:

     docker pull seahorn/clam-llvm10:nightly

# Requirements for compiling from sources #

Clam is written in C++ and uses heavily the Boost library. The
main requirements are:

- Modern C++ compiler supporting c++14 
- Boost >= 1.65
- GMP 
- MPFR (if `-DCRAB_USE_APRON=ON` or `-DCRAB_USE_ELINA=ON`)
- Python >= 3.6

In linux, you can install requirements typing the commands:

     sudo apt-get install libboost-all-dev libboost-program-options-dev
     sudo apt-get install libgmp-dev
     sudo apt-get install libmpfr-dev	

## Tests ##

Testing infrastructure depends on several Python packages. 
To run tests you need to install `lit` and `OutputCheck`:

     pip3 install lit
     pip3 install OutputCheck

# Compiling from sources and installation # 

The basic compilation steps are:

     mkdir build && cd build
     cmake -DCMAKE_INSTALL_PREFIX=_DIR_ ../
     cmake --build . --target crab && cmake ..
     cmake --build . --target llvm && cmake ..      
     cmake --build . --target install 


Clam provides several components that are installed via the `extra`
target. These components can be used by other projects outside of
Clam. 
  
* [sea-dsa](https://github.com/seahorn/sea-dsa): ```git clone https://github.com/seahorn/sea-dsa.git```

  `sea-dsa` is the heap analysis used to translate LLVM memory
  instructions. Details can be
  found [here](https://jorgenavas.github.io/papers/sea-dsa-SAS17.pdf)
  and [here](https://jorgenavas.github.io/papers/tea-dsa-fmcad19.pdf).
  
* [llvm-seahorn](https://github.com/seahorn/llvm-seahorn): ``` git clone https://github.com/seahorn/llvm-seahorn.git```

   `llvm-seahorn` provides specialized versions of `InstCombine` and
   `IndVarSimplify` LLVM passes as well as a LLVM pass to convert undefined values into nondeterministic calls.

The component `sea-dsa` is mandatory and `llvm-seahorn` is optional but highly
recommended. To include these external components, type instead:

     mkdir build && cd build
     cmake -DCMAKE_INSTALL_PREFIX=_DIR_ ../
     cmake --build . --target extra            
     cmake --build . --target crab && cmake ..
     cmake --build . --target llvm && cmake ..           
     cmake --build . --target install 

The Boxes/Apron/Elina domains require third-party libraries. To avoid
the burden to users who are not interested in those domains, the
installation of the libraries is optional.

- If you want to use the Boxes domain then add `-DCRAB_USE_LDD=ON` option.

- If you want to use the Apron library domains then add
  `-DCRAB_USE_APRON=ON` option.

- If you want to use the Elina library domains then add
  `-DCRAB_USE_ELINA=ON` option.

**Important:** Apron and Elina are currently not compatible so you
cannot enable `-DCRAB_USE_APRON=ON` and `-DCRAB_USE_ELINA=ON` at the same time. 

For instance, to install Clam with Boxes and Apron:

     mkdir build && cd build
     cmake -DCMAKE_INSTALL_PREFIX=_DIR_ -DCRAB_USE_LDD=ON -DCRAB_USE_APRON=ON ../
     cmake --build . --target extra                 
     cmake --build . --target crab && cmake ..
     cmake --build . --target ldd && cmake ..
     cmake --build . --target apron && cmake ..
     cmake --build . --target llvm && cmake ..                
     cmake --build . --target install 
     
## Compiling Clam as a shared library ##

1. Compile LLVM with cmake flags `-DLLVM_LINK_LLVM_DYLIB=ON -DLLVM_BUILD_LLVM_DYLIB=ON`
2. Compile Clam with cmake flags `-DCLAM_BUILD_LIBS_SHARED=ON -DCRAB_USE_APRON=OFF -DCRAB_USE_ELINA=OFF`

## Checking installation ## 

To run some regression tests:

     cmake --build . --target test-simple
     
# Usage #
	
Clam provides a Python script called `clam.py` to interact with
users. The simpler command is `clam.py test.c`. Type `clam.py --help`
for all options and read our [wiki](https://github.com/seahorn/clam/wiki/ClamUsage) 

  
  
