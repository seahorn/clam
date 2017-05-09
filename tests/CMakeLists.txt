# find lit. Assume we are using external lit installed from pip
find_program(LIT_COMMAND lit REQUIRED)

# add the test suite
add_lit_testsuite(tests "Run simple tests"
  -v 
  ${CMAKE_CURRENT_SOURCE_DIR}  ## where .cfg file is located
  PARAMS
  test_dir=${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS crabllvm
  )
