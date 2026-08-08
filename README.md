# Clam: LLVM front-end for Crab #

Clam is an [Abstract Interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation)-based static analyzer that computes inductive invariants for
LLVM bitcode based on
the [Crab](https://github.com/seahorn/crab) library.

The available documentation can be found in both
Clam [wiki](https://github.com/seahorn/clam/wiki/Home) and Crab [wiki](https://github.com/seahorn/crab/wiki).

# LLVM version #

**This branch targets LLVM 18.**

LLVM bitcode is not compatible across major releases, so the `clang` that
compiles your input and the Clam tools that read the resulting bitcode must both
come from that release: a newer `clang` (Apple's system `clang`, or any `clang`
from a later LLVM) emits bitcode the Clam tools cannot read, and every analysis
fails while reading it.

The commands below are the only ones in this file that name a version. The rest
of the file refers to them as *the LLVM release this branch targets*:

     docker pull seahorn/clam-llvm18:nightly   # prebuilt nightly image
     brew install llvm@18                      # macOS (Homebrew), keg-only
     apt-get install clang-18                  # Debian/Ubuntu

> **Migrating to a newer LLVM release.** Bump `CLAM_LLVM_VERSION` in
> `CMakeLists.txt` — it is the single source of truth for the build and drives
> `find_package(LLVM)`, the `dev<N>` branches of sea-dsa and llvm-seahorn, the
> versioned `clang` the test suite looks for, and `clam.py` — then update this
> section. Set `CLAM_LLVM_VERSION_MINOR` too if the new release series is not
> numbered `<major>.0.x`: LLVM's package version file only accepts a request
> whose major *and* minor match, and LLVM 18 ships as 18.1.x. Nothing else in
> this file needs an edit.

# Docker #

Nightly images are built and pushed to [Docker Hub](https://hub.docker.com/);
see [LLVM version](#llvm-version) for the `docker pull` command.

# Requirements #

Clam is written in C++ and uses heavily the Boost library. The
main requirements are:

- Modern C++ compiler (c++17 or newer; the exact standard is set by
  `CMAKE_CXX_STANDARD` in `CMakeLists.txt` and tracks the targeted LLVM release)
- Boost >= 1.65
- GMP 
- MPFR (only if `-DCRAB_USE_APRON=ON` or `-DCRAB_USE_ELINA=ON`)
- FLINT (only if `-DCRAB_USE_PPLITE=ON`) 
- Python >= 3.6

In linux, you can install requirements typing the commands:

     sudo apt-get install libboost-all-dev libboost-program-options-dev
     sudo apt-get install libgmp-dev
     sudo apt-get install libmpfr-dev	
     sudo apt-get install libflint-dev

## Tests ##

Testing infrastructure depends on several Python packages. 
To run tests you need to install `lit` and `OutputCheck`:

     pip3 install lit
     pip3 install OutputCheck

The tests also require a `clang` from
[the LLVM release this branch targets](#llvm-version). Each test compiles a C
input to LLVM bitcode with `clang` and then feeds that bitcode to the Clam tools
(`clam-pp`/`clam`), so a mismatched `clang` makes every test fail while reading
the bitcode.

CMake automatically looks for a suitably versioned `clang` in the usual
locations (Homebrew's keg-only `llvm@` formula, the Debian/Ubuntu `clang`
packages, MacPorts, ...). If none is found it prints a warning and the tests
fall back to whatever `clang` is on `PATH`. Install a matching toolchain with
one of the commands in [LLVM version](#llvm-version), or, if the compiler lives
somewhere non-standard, point CMake at it explicitly:

     cmake -DCLAM_TEST_CLANG=/path/to/clang ../

# Compilation and installation # 

The basic compilation steps are:

    1. mkdir build && cd build
    2. cmake -DCMAKE_INSTALL_PREFIX=$DIR ../
    3. cmake --build . --target crab && cmake ..   
    4. cmake --build . --target extra && cmake ..                  
    5. cmake --build . --target install 

The command at line 2 will try to find
[the required LLVM](#llvm-version) from standard paths.
If you installed it in a non-standard path, then add option
`-DLLVM_DIR=$LLVM_INSTALL_DIR/lib/cmake/llvm` to line 2.  The
command at line 3 will download Crab and compile it from sources.
Clam uses two external components that are installed via the `extra`
target at line 4. These components are:
  
* [sea-dsa](https://github.com/seahorn/sea-dsa) is the heap analysis used to translate LLVM memory
  instructions. Details can be
  found [here](https://jorgenavas.github.io/papers/sea-dsa-SAS17.pdf)
  and [here](https://jorgenavas.github.io/papers/tea-dsa-fmcad19.pdf).
  
* [llvm-seahorn](https://github.com/seahorn/llvm-seahorn) provides specialized versions of LLVM components to make them more amenable for verification. `llvm-seahorn` is optional but hightly recommended.  

The Boxes/Apron/Elina/PPLite domains require third-party libraries. To avoid
the burden to users who are not interested in those domains, the
installation of the libraries is optional.

- If you want to use the Boxes domain then add `-DCRAB_USE_LDD=ON` option.

- If you want to use the Apron library domains then add
  `-DCRAB_USE_APRON=ON` option.

- If you want to use the Elina library domains then add
  `-DCRAB_USE_ELINA=ON` option.

- If you want to use the PPLite library domains then add
  `-DCRAB_USE_APRON=ON -DCRAB_USE_PPLITE=ON` options.

**Important:** Apron and Elina are currently not compatible so you
cannot enable `-DCRAB_USE_APRON=ON` and `-DCRAB_USE_ELINA=ON` at the same time. 

For instance, to install Clam with Boxes and Apron:

    1. mkdir build && cd build
    2. cmake -DCMAKE_INSTALL_PREFIX=$DIR -DCRAB_USE_LDD=ON -DCRAB_USE_APRON=ON ../
    3. cmake --build . --target crab && cmake ..
    4. cmake --build . --target extra && cmake ..                
    5. cmake --build . --target ldd && cmake ..
    6. cmake --build . --target apron && cmake ..             
    7. cmake --build . --target install 

For instance, lines 5 and 6 will download, compile and install the
Boxes and Apron libraries, respectively.  If you have already compiled
and installed these libraries in your machine then skip commands at
line 5 and 6 and add the following options at line 2.

- For Apron: `-DAPRON_ROOT=$APRON_INSTALL_DIR`
- For Elina: `-DELINA_ROOT=$ELINA_INSTALL_DIR`
- For Boxes: `-DCUDD_ROOT=$CUDD_INSTALL_DIR -DLDD_ROOT=$LDD_INSTALL_DIR`
- For PPLite: `-DPPLITE_ROOT=$PPLITE_INSTALL_DIR -DFLINT_ROOT=$FLINT_INSTALL_DIR`

## Checking installation ## 

To run some regression tests:

     cmake --build . --target test-simple

These tests need a matching `clang` on the machine; see the
[Tests](#tests) section above for why and how to provide one.

# Usage #

Clam provides a Python script called `clam.py` (located at `$DIR/bin` where `$DIR` is the directory where Clam was installed) to interact with
users. The simplest command is `clam.py test.c`. Type `clam.py --help`
for all options and read
our [wiki](https://github.com/seahorn/clam/wiki/ClamUsage).
