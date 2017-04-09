project(CrabLlvm)

cmake_minimum_required(VERSION 2.8.11)

set (CrabLlvm_VERSION_MAJOR 0)
set (CrabLlvm_VERSION_MINOR 1)
set (CrabLlvm_VERSION_PATCH 0)
#set (CrabLlvm_VERSION_TWEAK "rc1")

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR )
  message (FATAL_ERROR
    "In-source builds are not allowed. Please clean your source tree and try again.")
endif()

enable_testing()
include (CTest)

# determine if this is top-level or embedded project
if (PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME)
  set (TopLevel TRUE)
else()
  set (TopLevel FALSE)
endif()

if (TopLevel)
  if (NOT PACKAGE_VERSION)
     set(PACKAGE_VERSION
       "${CrabLlvm_VERSION_MAJOR}.${CrabLlvm_VERSION_MINOR}.${CrabLlvm_VERSION_PATCH}")
     if (DEFINED CrabLlvm_VERSION_TWEAK)
       set (PACKAGE_VERSION "${PACKAGE_VERSION}-${CrabLlvm_VERSION_TWEAK}")
     endif()
     set (CrabLlvm_VERSION_INFO ${PACKAGE_VERSION})
  endif()
endif ()

option (USE_LDD   "Enable ldd" OFF)
option (USE_APRON "Enable Apron library" OFF)
option (USE_STATS "Enable statistics about the analysis" ON)
option (USE_LOG   "Enable logging for debugging purposes" ON)
option (USE_COTIRE "Use cotire to speed up compilation time" OFF)
option (BUILD_CRABLLVM_LIBS_SHARED "Build all CrabLlvm libraries shared." OFF)

# Add path for custom modules
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

if (BUILD_CRABLLVM_LIBS_SHARED)
  message (STATUS "All crabllvm libraries will be built as shared libraries")
  set(CRABLLVM_LIBS_TYPE SHARED)
else()
  message (STATUS "All crabllvm libraries will be built statically")
  set(CRABLLVM_LIBS_TYPE STATIC)
endif ()

# so that executables outside the build tree can find later shared
# libraries
# Even if CRABLLVM_LIBS_TYPE=STATIC we could have a mix of shared and
# static libraries
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set(CMAKE_MACOSX_RPATH TRUE)
if (NOT CMAKE_INSTALL_RPATH)
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif ()

if (TopLevel)
  set (CUSTOM_BOOST_ROOT "" CACHE PATH "Path to custom boost installation.")
  if (CUSTOM_BOOST_ROOT)
    set (BOOST_ROOT ${CUSTOM_BOOST_ROOT})
    set (Boost_NO_SYSTEM_PATHS "ON")
  endif()

  option (CRABLLVM_STATIC_EXE "Static executable." OFF)

  if (NOT BUILD_CRABLLVM_LIBS_SHARED)  
    set (Boost_USE_STATIC_LIBS ON)
  endif ()  
  option (UNIT_TESTS "Compile unit tests" OFF)
  if (UNIT_TESTS)
    find_package (Boost 1.55 REQUIRED COMPONENTS system unit_test_framework)
  else ()
    find_package (Boost 1.55 REQUIRED COMPONENTS system)
  endif ()

  if (Boost_FOUND)
    include_directories (${Boost_INCLUDE_DIRS})
  endif ()

  # put static libraries first unless we want shared libraries
  if (NOT BUILD_CRABLLVM_LIBS_SHARED)
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ${CMAKE_FIND_LIBRARY_SUFFIXES})
  endif ()   
endif ()

if (USE_COTIRE)
  # Required by cotire
  set (ENV{CCACHE_SLOPPINESS} "pch_defines,time_macros")
  include(cotire)
endif ()

include(ExternalProject)
set_property(DIRECTORY PROPERTY EP_STEP_TARGETS configure build test)

find_package (Git)

if (GIT_FOUND)
  if (TopLevel)
    set (DSA_SEAHORN_REPO "https://github.com/seahorn/dsa-seahorn"
      CACHE STRING "dsa-seahorn repo")
    set (SEAHORN_LLVM_REPO "https://github.com/seahorn/llvm-seahorn"
      CACHE STRING "seahorn-llvm repo")
    add_custom_target (dsa-seahorn-git
      ${GIT_EXECUTABLE} clone ${DSA_SEAHORN_REPO} ${CMAKE_SOURCE_DIR}/dsa-seahorn)
    add_custom_target (seahorn-llvm-git
      ${GIT_EXECUTABLE} clone ${SEAHORN_LLVM_REPO} ${CMAKE_SOURCE_DIR}/llvm-seahorn)
    add_custom_target (extra DEPENDS dsa-seahorn-git seahorn-llvm-git)
  endif ()
else ()
  if (TopLevel)
    message (STATUS "Could not find git. Not adding 'extra' target.")
  endif ()
endif ()

if (USE_LDD)
  if (GIT_FOUND)
    if (NOT HAVE_LDD)
       set (LDD_TAG "origin/master" CACHE STRING "ldd tag to use")
       set (LDD_REPO "http://git.code.sf.net/p/lindd/code" CACHE STRING "ldd repository")
       ExternalProject_Add(ldd
         GIT_REPOSITORY ${LDD_REPO}
         GIT_TAG ${LDD_TAG}
         PREFIX ${CMAKE_BINARY_DIR}/ldd
         INSTALL_DIR ${CMAKE_BINARY_DIR}/run/ldd
         CMAKE_ARGS
         -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
         -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
         -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
	 # XXX: ldd is always compiled statically.
	 # We add -fPIC flag so it can be linked with a shared library
	 -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON	 
         TEST_AFTER_INSTALL 1
         TEST_COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_LIST_FILE}
         LOG_DOWNLOAD 1
         LOG_CONFIGURE 1
         LOG_BUILD 1
         LOG_INSTALL 1)
    endif ()
  else ()
    message (STATUS "Could not find git. Not downloading ldd")
  endif()

  find_package(Ldd)
  if (LDD_FOUND)
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LDD_CXXFLAGS}")
    set (LDD_LIBS ${LDD_LIBRARY})
    set (HAVE_LDD TRUE CACHE BOOL "Ldd found")
    message(STATUS "Found Ldd ${LDD_INCLUDE_DIR}")
    if (NOT TopLevel)
      # propagate variables to the caller
      set (HAVE_LDD TRUE PARENT_SCOPE)
      set (LDD_INCLUDE_DIR ${LDD_INCLUDE_DIR} PARENT_SCOPE)
      set (LDD_LIBS ${LDD_LIBS} PARENT_SCOPE)
    endif()
  else()
    ExternalProject_Get_Property (ldd INSTALL_DIR)
    set (LDD_ROOT ${INSTALL_DIR} CACHE FILEPATH "Forced location of ldd" FORCE)
    message (WARNING "No ldd found. Run \n\tcmake --build . --target ldd && cmake ${CMAKE_SOURCE_DIR}")
    return()
  endif()
endif ()

include_directories (${LDD_INCLUDE_DIR})

if (USE_APRON)
   if (GIT_FOUND)

     find_package(Gmp REQUIRED)
     get_filename_component (GMP_SEARCH_PATH ${GMP_INCLUDE_DIR} PATH)

     find_package(MPFR REQUIRED)
     if (MPFR_FOUND)
       get_filename_component (MPFR_SEARCH_PATH ${MPFR_INCLUDES} PATH)
     endif ()

     find_package(SSE)
     if (AVX_FOUND)
       message(STATUS "Building opt-oct with AVX")
       set (IS_VECTOR "IS_VECTOR=-DVECTOR")
     else ()
       if (SSE2_FOUND OR SSE3_FOUND OR SSSE3_FOUND OR SSE4_1_FOUND)
          message(STATUS "Building opt-oct with SSE")
	set (IS_VECTOR "IS_VECTOR=-DVECTOR")
          set (IS_SSE "IS_SSE=-DSSE")
       else ()
          message(STATUS "Building opt-oct without vectorized operations.")
       endif ()
     endif ()

     if (NOT HAVE_APRON)
       set (APRON_TAG "origin/elina" CACHE STRING "apron tag to use")
       ExternalProject_Add(apron
         GIT_REPOSITORY https://jorgenavas@bitbucket.org/jorgenavas/apron.git
         GIT_TAG ${APRON_TAG}
         INSTALL_DIR ${CMAKE_BINARY_DIR}/run/apron
         CONFIGURE_COMMAND echo "Apron does not need a configure"
         BUILD_IN_SOURCE 1
         BUILD_COMMAND make CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER} ${IS_VECTOR} ${IS_SSE}
         APRON_PREFIX=<INSTALL_DIR> GMP_PREFIX=${GMP_SEARCH_PATH} MPFR_PREFIX=${MPFR_SEARCH_PATH} 
         INSTALL_COMMAND 
         make APRON_PREFIX=<INSTALL_DIR> GMP_PREFIX=${GMP_SEARCH_PATH} MPFR_PREFIX=${MPFR_SEARCH_PATH} install
         LOG_CONFIGURE 1
         LOG_INSTALL 1
         LOG_BUILD 1)
     endif ()
   else ()
     message (STATUS "Could not find git. Not downloading apron")
   endif()
   
   find_package(Apron)
   if (APRON_FOUND)
     #include_directories (${APRON_INCLUDE_DIR})
     set (APRON_LIBS ${APRON_LIBRARY} ${MPFR_LIBRARIES})
     set (HAVE_APRON TRUE CACHE BOOL "Apron found")
     message(STATUS "Found Apron ${APRON_INCLUDE_DIR}")
     if (NOT TopLevel)
       # propagate variables to the caller
       set (HAVE_APRON TRUE PARENT_SCOPE)
       set (APRON_INCLUDE_DIR ${APRON_INCLUDE_DIR} PARENT_SCOPE)
       set (APRON_LIBS ${APRON_LIBS} PARENT_SCOPE)
     endif()
   else()
     ExternalProject_Get_Property (apron INSTALL_DIR)
     set (APRON_ROOT ${INSTALL_DIR} CACHE FILEPATH "Forced location of apron" FORCE)
     message (WARNING "No apron found. Run \n\tcmake --build . --target apron && cmake ${CMAKE_SOURCE_DIR}")
     return()
   endif()
endif()

include_directories (${APRON_INCLUDE_DIR})

if (USE_LDD)
  message (STATUS "The boxes domain will be available")  
else ()
  message (STATUS "The boxes domain will not be available")  
endif ()

if (USE_APRON)
  message (STATUS "The apron domains will be available")
else ()
  message (STATUS "The apron domains will not be available")
endif ()

if (USE_STATS)
  message (STATUS "Analysis statistics is enabled")
  set (HAVE_STATS TRUE CACHE BOOL "Stats enabled")
else ()
  message (STATUS "Analysis statistics is disabled")
endif ()

if (GIT_FOUND)
  set (CRAB_CORE_TAG "origin/crab-llvm" CACHE STRING "crab tag to use")
  set (CRAB_CORE_REPO "https://github.com/caballa/crab.git"
       CACHE STRING "crab repository")
  ExternalProject_Add(crab
    GIT_REPOSITORY ${CRAB_CORE_REPO}
    GIT_TAG ${CRAB_CORE_TAG}
    PREFIX ${CMAKE_BINARY_DIR}/crab
    INSTALL_DIR ${CMAKE_BINARY_DIR}/run
    CMAKE_ARGS
    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
    -DBUILD_CRAB_LIBS_SHARED=${BUILD_CRABLLVM_LIBS_SHARED}
    -DCMAKE_USE_LDD=${USE_LDD}
    -DCMAKE_LDD_INCLUDE_DIR=${LDD_INCLUDE_DIR}
    -DCMAKE_LDD_LIBRARY=${LDD_LIBRARY}
    -DCMAKE_CUDD_FOUND=${CUDD_FOUND}
    -DCMAKE_USE_APRON=${USE_APRON}
    -DCMAKE_APRON_INCLUDE_DIR=${APRON_INCLUDE_DIR}
    -DCMAKE_APRON_LIBRARY=${APRON_LIBRARY}
    -DCMAKE_USE_STATS=${USE_STATS}
    -DCMAKE_USE_LOG=${USE_LOG}
    TEST_AFTER_INSTALL 1
    TEST_COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_LIST_FILE}
    LOG_DOWNLOAD 1
    LOG_CONFIGURE 1
    LOG_BUILD 1
    LOG_INSTALL 1)
else ()
    message (STATUS "Could not find git. Not downloading crab")
endif()

  
find_package (Crab)
if (NOT CRAB_FOUND)
  ExternalProject_Get_Property (crab INSTALL_DIR)
  set (CRAB_ROOT ${INSTALL_DIR} CACHE FILEPATH "Forced location of crab" FORCE)
  message(WARNING "No crab found. Run \n\tcmake --build . --target crab && cmake ${CMAKE_SOURCE_DIR}")
  return()
else()
  if (NOT TopLevel)
    # propagate variables to the caller
    set (CRAB_FOUND ${CRAB_FOUND} PARENT_SCOPE)
    set (CRAB_INCLUDE_DIR ${CRAB_INCLUDE_DIR} PARENT_SCOPE)
    set (CRAB_LIBS ${CRAB_LIBS} PARENT_SCOPE)
  endif()
endif()  

include_directories (${CRAB_INCLUDE_DIR})
configure_file(${CRAB_INCLUDE_DIR}/crab/config.h.cmake
               ${CMAKE_BINARY_DIR}/include/crab/config.h )
set_target_properties(crab PROPERTIES EXCLUDE_FROM_ALL ON)

if (TopLevel)
  # if top-level, offer to build llvm
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
  include(HandleLLVMOptions)
  set(LLVM_RUNTIME_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin)
  set(LLVM_LIBRARY_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib)

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

add_definitions(-std=c++11)
add_definitions(-Wno-redeclared-class-member -Wno-sometimes-uninitialized)
#add_definitions(-Wno-covered-switch-default)

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
  add_definitions( -Wno-unused-local-typedefs)
  else ()
   if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedef")
     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-inconsistent-missing-override")
   endif ()
endif ()

if (TopLevel)
  if (IS_DIRECTORY ${CMAKE_SOURCE_DIR}/dsa-seahorn AND
      EXISTS ${CMAKE_SOURCE_DIR}/dsa-seahorn/lib/DSA/Steensgaard.cpp)
    include_directories (AFTER dsa-seahorn/include)
    add_subdirectory (dsa-seahorn/lib/AssistDS)
    add_subdirectory (dsa-seahorn/lib/DSA)
    set (DSA_LIBS DSA AssistDS)
    set (HAVE_DSA TRUE)
  else()
    message (WARNING "No DSA found in ${CMAKE_SOURCE_DIR}/dsa-seahorn")
    set (DSA_LIBS "")
  endif()
endif ()


if (TopLevel)
  if (IS_DIRECTORY ${CMAKE_SOURCE_DIR}/llvm-seahorn AND
      EXISTS ${CMAKE_SOURCE_DIR}/llvm-seahorn/lib/CMakeLists.txt)
    include_directories (BEFORE llvm-seahorn/include)
    add_subdirectory(llvm-seahorn/lib)
    add_subdirectory(llvm-seahorn/tools)
    set (HAVE_LLVM_SEAHORN TRUE)
    set (LLVM_SEAHORN_LIBS SeaInstCombine SeaLoops SeaInstNondet)
  endif()
endif ()

include_directories(${CrabLlvm_SOURCE_DIR}/include)

if (TopLevel)
  include_directories (${CMAKE_BINARY_DIR}/include)
  add_subdirectory(lib)
  add_subdirectory(tools)
  add_subdirectory(py)
  add_subdirectory(tests)
endif()

configure_file( include/crab_llvm/config.h.cmake
                ${CMAKE_BINARY_DIR}/include/crab_llvm/config.h )


if (TopLevel)
   set (PACKAGE_NAME CrabLlvm)
   set (PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")

   # Configure CPack.
   include(TargetArch)
   target_architecture(CMAKE_TARGET_ARCH)
   message ("-- arch: ${CMAKE_TARGET_ARCH}")
   set(CPACK_PACKAGE_INSTALL_DIRECTORY "CrabLlvm")
   set(CPACK_PACKAGE_VENDOR "CrabLlvm")
   set(CPACK_PACKAGE_VERSION_MAJOR ${CrabLlvm_VERSION_MAJOR})
   set(CPACK_PACKAGE_VERSION_MINOR ${CrabLlvm_VERSION_MINOR})
   set(CPACK_PACKAGE_VERSION_PATCH ${CrabLlvm_VERSION_PATCH})
   set(CPACK_PACKAGE_VERSION ${PACKAGE_VERSION})
   #set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
   set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
   set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
   if(CMAKE_BUILD_TYPE STREQUAL Release)
     set(CPACK_PACKAGE_FILE_NAME
         "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION}-${CMAKE_SYSTEM_NAME}-${CMAKE_TARGET_ARCH}")
   else()
     set(CPACK_PACKAGE_FILE_NAME
         "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION}-${CMAKE_SYSTEM_NAME}-${CMAKE_TARGET_ARCH}-${CMAKE_BUILD_TYPE}")
   endif()
   if(WIN32 AND NOT UNIX)
     set(CPACK_NSIS_MODIFY_PATH "ON")
     set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL "ON")
     set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS
       "ExecWait '$INSTDIR/tools/msbuild/install.bat'")
     set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS
       "ExecWait '$INSTDIR/tools/msbuild/uninstall.bat'")
   endif()
   include(CPack)

   # install all the header files
   install(DIRECTORY include/crab_llvm DESTINATION include)

   ## config.h
   install(DIRECTORY ${CMAKE_BINARY_DIR}/include/crab DESTINATION include
     FILES_MATCHING
     PATTERN "*.hpp"
     PATTERN "*.hh"
     PATTERN "*.h"
     PATTERN "CMakeFiles" EXCLUDE
     PATTERN ".svn" EXCLUDE
   )

   ## config.h
   install(DIRECTORY ${CMAKE_BINARY_DIR}/include/crab_llvm DESTINATION include
     FILES_MATCHING
     PATTERN "*.hpp"
     PATTERN "*.hh"
     PATTERN "*.h"
     PATTERN "CMakeFiles" EXCLUDE
     PATTERN ".svn" EXCLUDE
   )

   install(DIRECTORY ${CMAKE_BINARY_DIR}/run/include/crab DESTINATION include
     FILES_MATCHING
     PATTERN "*.hpp"
     PATTERN "*.hh"
     PATTERN "CMakeFiles" EXCLUDE
     PATTERN ".svn" EXCLUDE
   )

   install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/README.md DESTINATION .)
   # install all the licenses
   # install(FILES LICENSE DESTINATION share/doc
   #         RENAME crabllvm_license.txt)       

   if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/ext/llvm/LICENSE.TXT)
       install (FILES ext/llvm/LICENSE.TXT
         DESTINATION share/doc
         RENAME llvm_license.txt)
   endif()
   
endif ()
