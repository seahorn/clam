project(IkosLlvm)
cmake_minimum_required(VERSION 2.8.11)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR )
  message (FATAL_ERROR
    "In-source builds are not allowed. Please clean your source tree and try again.")
endif()

# determine if this is top-level or embedded project
if (PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME)
  set (TopLevel TRUE)
else()
  set (TopLevel FALSE)
endif()


# put static libraries first
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ${CMAKE_FIND_LIBRARY_SUFFIXES})

# Add path for custom modules
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

#option (LLVMIKOS_STATIC_EXE "Static executable." OFF)
if (TopLevel)
  set (CUSTOM_BOOST_ROOT "" CACHE PATH "Path to custom boost installation.")
  if (CUSTOM_BOOST_ROOT)
    set (BOOST_ROOT ${CUSTOM_BOOST_ROOT})
    set (Boost_NO_SYSTEM_PATHS "ON")
    
  endif()

  # put static libraries first
  #set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ${CMAKE_FIND_LIBRARY_SUFFIXES})

  set (Boost_USE_STATIC_LIBS ON)
  find_package (Boost 1.55 REQUIRED COMPONENTS system unit_test_framework)
  IF (Boost_FOUND)
    include_directories (${Boost_INCLUDE_DIRS})
  endif ()
endif ()

include(ExternalProject)
set_property(DIRECTORY PROPERTY EP_STEP_TARGETS configure build test)

set (IKOS_CORE_TAG "origin/master" CACHE STRING "ikos-core tag to use")
set (IKOS_CORE_REPO "https://github.com/seahorn/ikos-core.git"
  CACHE STRING "ikos-core repository")
ExternalProject_Add(ikos-core
  GIT_REPOSITORY ${IKOS_CORE_REPO}
  GIT_TAG ${IKOS_CORE_TAG}
  PREFIX ${CMAKE_BINARY_DIR}/ikos-core
  INSTALL_DIR ${CMAKE_BINARY_DIR}/run
  CMAKE_ARGS
  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
  -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
  -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  LOG_DOWNLOAD 1
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1)

find_package (IKOS)
if (NOT IKOS_FOUND)
  ExternalProject_Get_Property (ikos-core INSTALL_DIR)
  set (IKOS_ROOT ${INSTALL_DIR} CACHE FILEPATH "Forced location of ikos-core" FORCE)
  message(WARNING "No ikos found. Run \n\tcmake --build . --target ikos-core && cmake ${CMAKE_SOURCE_DIR}")
  return()
else()
  if (NOT TopLevel)
    # propagate variables to the caller
    set (IKOS_FOUND ${IKOS_FOUND} PARENT_SCOPE)
    set (IKOS_INCLUDE_DIR ${IKOS_INCLUDE_DIR} PARENT_SCOPE)
  endif()
endif()  

include_directories (${IKOS_INCLUDE_DIR})
set_target_properties(ikos-core PROPERTIES EXCLUDE_FROM_ALL ON)

if (TopLevel)
  # if top-level, ofer to build llvm
  ExternalProject_Add (llvm
    SVN_REPOSITORY http://llvm.org/svn/llvm-project/llvm/tags/RELEASE_360/final/
    SOURCE_DIR ${CMAKE_SOURCE_DIR}/ext/llvm
    INSTALL_DIR ${CMAKE_BINARY_DIR}/run
    CMAKE_ARGS
    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
    -DLLVM_TARGETS_TO_BUILD:STRING=X86 -DWITH_POLY:BOOL=OFF
    -DLLVM_ENABLE_PEDANTIC=OFF 
    -DLLVM_ENABLE_PIC=ON -DLLVM_REQUIRES_RTTI:BOOL=TRUE
    LOG_CONFIGURE 1
    LOG_BUILD 1
    LOG_INSTALL 1)

  find_package (LLVM 3.6.0 CONFIG NO_DEFAULT_PATH)
  if (NOT LLVM_FOUND)
    ExternalProject_Get_Property (llvm INSTALL_DIR)
    set (LLVM_ROOT ${INSTALL_DIR})
    set (LLVM_DIR ${LLVM_ROOT}/share/llvm/cmake CACHE PATH
      "Forced location of LLVM cmake config" FORCE)
    message (WARNING "No llvm found. Run \n\tcmake --build . && cmake ${CMAKE_SOURCE_DIR}")
    return()
  else()
    set_target_properties(llvm PROPERTIES EXCLUDE_FROM_ALL ON)
  endif()
  
  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
  message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
  
  # We incorporate the CMake features provided by LLVM:
  list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
  include(AddLLVM)

  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LLVM_CXXFLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LLVM_LDFLAGS}")

  set(LLVM_REQUIRES_RTTI TRUE)
  set(LLVM_REQUIRES_EH TRUE)
  
  include_directories(${LLVM_INCLUDE_DIRS})
  link_directories(${LLVM_LIBRARY_DIRS})
  add_definitions(${LLVM_DEFINITIONS})

  find_library(RT_LIB NAMES rt)
  if (NOT RT_LIB)
    set(RT_LIB "")
  endif()
  mark_as_advanced(RT_LIB)

  find_package(Curses)

  find_package(Gmp REQUIRED)
  if (GMP_FOUND)
    include_directories (${GMP_INCLUDE_DIR})
    include_directories (${GMPXX_INCLUDE_DIR})
  else()
    set(GMP_LIB "")
    set(GMPXX_LIB "")
  endif()
endif()

include_directories(${IkosLlvm_SOURCE_DIR}/include)

add_definitions(-std=c++11)
add_definitions(-Wno-redeclared-class-member -Wno-sometimes-uninitialized)
add_definitions(-Wno-covered-switch-default)

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
  add_definitions( -Wno-unused-local-typedefs)
endif ()

if (TopLevel)
  include_directories (${CMAKE_BINARY_DIR}/include)
  add_subdirectory(lib)
  add_subdirectory(tools)
endif()

configure_file( include/ikos_llvm/config.h.cmake
                ${CMAKE_BINARY_DIR}/include/ikos_llvm/config.h )

install(DIRECTORY include/ikos_llvm DESTINATION include/ikos_llvm)
