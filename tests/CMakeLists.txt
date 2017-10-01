# find lit. Assume we are using external lit installed from pip
find_program(LIT_COMMAND lit REQUIRED)

# add the test suite
add_lit_testsuite(test-simple "Run simple tests"
  -v
  ${CMAKE_CURRENT_SOURCE_DIR}/simple  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}/simple
  DEPENDS crabllvm)

if (USE_LDD)
  # add the test suite
  add_lit_testsuite(test-ssh "Run SVCOMP ssh-simplified benchmarks"
    -v 
    ${CMAKE_CURRENT_SOURCE_DIR}/ssh-simplified  ## where .cfg file is located
    PARAMS
    test_dir=${CMAKE_CURRENT_BINARY_DIR}/ssh-simplified
    DEPENDS crabllvm)
endif()
