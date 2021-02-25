# find lit. Assume we are using external lit installed from pip
find_program(LIT_COMMAND lit REQUIRED)

set(LLVM_EXTERNAL_LIT ${LIT_COMMAND})

add_lit_testsuite(test-simple "Run simple tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/simple  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/simple
  DEPENDS clam)

add_lit_testsuite(test-inter "Run inter-procedural tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/inter  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/inter
  DEPENDS clam)

add_lit_testsuite(test-array-adapt "Run tests for the array adaptive domain"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/array-adapt  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/array-adapt
  DEPENDS clam)

add_lit_testsuite(test-mem "Run tests for --crab-track=mem"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/mem  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/mem
  DEPENDS clam)

add_lit_testsuite(test-uaf "Run tests for use-after-free checker"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/uaf  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/uaf
  DEPENDS clam)

add_lit_testsuite(test-tag "Run tests for the tag analysis"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/tag-analysis  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/tag-analysis
  DEPENDS clam)

add_lit_testsuite(test-opt "Run Optimizer tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/opt  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/opt
  DEPENDS clam)

add_lit_testsuite(test-readme "Run README.md tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/demo  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/demo
  DEPENDS clam)

if (CRAB_USE_LDD)
  add_lit_testsuite(test-ssh "Run SVCOMP ssh benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ssh  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ssh
    DEPENDS clam)
  
  add_lit_testsuite(test-ssh-simplified "Run SVCOMP ssh-simplified benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ssh-simplified  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ssh-simplified
    DEPENDS clam)

  add_lit_testsuite(test-ntdrivers-simplified "Run SVCOMP ntdrivers-simplified benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ntdrivers-simplified  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ntdrivers-simplified
    DEPENDS clam)
  
endif()
