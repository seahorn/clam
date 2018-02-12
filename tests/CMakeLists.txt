# find lit. Assume we are using external lit installed from pip
find_program(LIT_COMMAND lit REQUIRED)

add_lit_testsuite(test-simple "Run simple tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/simple  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/simple
  DEPENDS crabllvm)

add_lit_testsuite(test-readme "Run README.md tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/demo  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/demo
  DEPENDS crabllvm)

if (USE_LDD)
  add_lit_testsuite(test-ssh "Run SVCOMP ssh benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ssh  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ssh
    DEPENDS crabllvm)
  
  add_lit_testsuite(test-ssh-simplified "Run SVCOMP ssh-simplified benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ssh-simplified  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ssh-simplified
    DEPENDS crabllvm)

  add_lit_testsuite(test-ntdrivers-simplified "Run SVCOMP ntdrivers-simplified benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ntdrivers-simplified  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ntdrivers-simplified
    DEPENDS crabllvm)
  
endif()
