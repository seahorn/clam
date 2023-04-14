set(SEAHORN_LLVM_SOURCE_DIR "${CMAKE_SOURCE_DIR}/llvm-seahorn" CACHE STRING "seahorn-llvm source directory")
if (TopLevel)
  set(SEAHORN_LLVM_REPO "https://github.com/seahorn/llvm-seahorn" CACHE STRING "seahorn-llvm repo")
  add_custom_target(seahorn-llvm-git
    ${GIT_EXECUTABLE} clone -b dev14 ${SEAHORN_LLVM_REPO} ${SEAHORN_LLVM_SOURCE_DIR})
endif()
