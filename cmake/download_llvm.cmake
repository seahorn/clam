set (LLVM_TAG "llvmorg-10.0.1" CACHE STRING "LLVM tag")  
add_custom_target (download-llvm-src
  # HACK: remove all sources so that git-clone does not fail
  # HACK: a proper solution is to work arround this as in ExternalProject
  ${CMAKE_COMMAND} -E remove_directory ${CMAKE_SOURCE_DIR}/ext/llvm
  COMMAND
  ${GIT_EXECUTABLE} clone --depth=1 --branch=${LLVM_TAG}
  https://github.com/llvm/llvm-project.git ${CMAKE_SOURCE_DIR}/ext/llvm-project
  # HACK: I cannot find an official repo with only llvm-10 so we
  # bring the whole llvm-project repo, copy llvm directory and then
  # remove the rest.
  COMMAND
  ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/ext/llvm-project/llvm ${CMAKE_SOURCE_DIR}/ext/llvm
  COMMAND
  ${CMAKE_COMMAND} -E remove_directory ${CMAKE_SOURCE_DIR}/ext/llvm-project)

# if top-level, offer to build llvm
ExternalProject_Add (llvm
  DEPENDS download-llvm-src
  DOWNLOAD_COMMAND ""    
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/ext/llvm
  INSTALL_DIR ${CMAKE_BINARY_DIR}/llvm-install
  CMAKE_ARGS
  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
  -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
  -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  -DLLVM_TARGETS_TO_BUILD:STRING=X86 -DWITH_POLY:BOOL=OFF
  -DLLVM_ENABLE_PEDANTIC=OFF 
  -DLLVM_ENABLE_PIC=ON 
  -DLLVM_BUILD_LLVM_DYLIB:BOOL=${CLAM_BUILD_LIBS_SHARED}
  -DLLVM_INCLUDE_TESTS:BOOL=OFF
  -DLLVM_INCLUDE_GO_TESTS=OFF
  -DLLVM_INCLUDE_EXAMPLES=OFF
  -DLLVM_INCLUDE_DOCS=OFF    
  -DLLVM_BINDINGS_LIST=" "
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1)
