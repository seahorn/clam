# find lit. Assume we are using external lit installed from pip
find_program(LIT_COMMAND lit REQUIRED)

set(LLVM_EXTERNAL_LIT ${LIT_COMMAND})

# The clam tools are built against LLVM ${CLAM_LLVM_VERSION}. The lit tests
# compile their C inputs to bitcode with clang and hand it to clam-pp/clam, so
# that clang must be of the same LLVM release: a newer clang (e.g. Apple clang)
# emits bitcode the clam tools cannot read, causing every test to fail.
# Locate a compatible clang (a versioned name so we never grab the system
# clang) and pass its directory to lit, which puts it first on PATH (lit.cfg).
# Every name and path below is derived from CLAM_LLVM_VERSION (see the top-level
# CMakeLists.txt), so a new LLVM release needs no edit here. Override with
# -DCLAM_TEST_CLANG=/path/to/clang if auto-detection is wrong.
set(V ${CLAM_LLVM_VERSION})
find_program(CLAM_TEST_CLANG
  NAMES clang-${V} clang-mp-${V}.0 clang-${V}.0
  HINTS
    ${LLVM_TOOLS_BINARY_DIR}
    /opt/homebrew/opt/llvm@${V}/bin      # Homebrew (Apple Silicon), keg-only
    /usr/local/opt/llvm@${V}/bin         # Homebrew (Intel), keg-only
    /usr/lib/llvm-${V}/bin               # Debian/Ubuntu llvm-${V} packages
    /opt/local/libexec/llvm-${V}.0/bin   # MacPorts
  DOC "LLVM-${V} clang used to compile the C inputs of the test suite")

if (CLAM_TEST_CLANG)
  get_filename_component(CLAM_TEST_CLANG_DIR "${CLAM_TEST_CLANG}" DIRECTORY)
  message(STATUS "Clam tests: compiling test inputs with ${CLAM_TEST_CLANG}")
else()
  set(CLAM_TEST_CLANG_DIR "")
  message(WARNING
    "Clam tests: could not find an LLVM-${V} clang (e.g. clang-${V}). The tests "
    "will fall back to whatever 'clang' is on PATH, which may emit bitcode "
    "incompatible with the LLVM-${V} clam tools and make every test fail. "
    "Install one (e.g. 'brew install llvm@${V}' or the llvm-${V} system package) "
    "or set -DCLAM_TEST_CLANG=/path/to/clang-${V}.")
endif()

# lit params shared by every test suite below. clam_dir points lit at the
# clam.py installed by this build (see getClam in lit.cfg) so it does not pick
# up a clam from PATH that was built against a different LLVM; it means the
# tests must be run after 'ninja install'.
set(CLAM_LIT_PARAMS
  clang_dir=${CLAM_TEST_CLANG_DIR}
  clam_dir=${CMAKE_INSTALL_PREFIX}/bin)

add_lit_testsuite(test-simple "Run simple tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/simple  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/simple
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

add_lit_testsuite(test-inter "Run inter-procedural tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/inter  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/inter
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

add_lit_testsuite(test-array-adapt "Run tests for the array adaptive domain"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/array-adapt  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/array-adapt
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

add_lit_testsuite(test-mem "Run tests for --crab-track=mem"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/mem  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/mem
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

add_lit_testsuite(test-uaf "Run tests for use-after-free checker"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/uaf  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/uaf
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

add_lit_testsuite(test-tag "Run tests for the tag analysis"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/tag-analysis  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/tag-analysis
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

add_lit_testsuite(test-opt "Run Optimizer tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/opt  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/opt
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

add_lit_testsuite(test-readme "Run README.md tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/demo  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/demo
  ${CLAM_LIT_PARAMS}
  DEPENDS clam)

if (CRAB_USE_LDD)
  add_lit_testsuite(test-ssh "Run SVCOMP ssh benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ssh  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ssh
    ${CLAM_LIT_PARAMS}
  DEPENDS clam)
  
  add_lit_testsuite(test-ssh-simplified "Run SVCOMP ssh-simplified benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ssh-simplified  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ssh-simplified
    ${CLAM_LIT_PARAMS}
  DEPENDS clam)

  add_lit_testsuite(test-ntdrivers-simplified "Run SVCOMP ntdrivers-simplified benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ntdrivers-simplified  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ntdrivers-simplified
    ${CLAM_LIT_PARAMS}
  DEPENDS clam)
  
endif()
