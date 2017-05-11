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

# Default is release with debug info
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
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

#### crab-llvm options ######
option (USE_COTIRE "Use cotire to speed up compilation time" OFF)
option (BUILD_CRABLLVM_LIBS_SHARED "Build all CrabLlvm libraries shared." OFF)
#### end crab-llvm options ######

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
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(CMAKE_MACOSX_RPATH TRUE)  
endif ()  
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
  
  find_package (Boost 1.55)
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

  set (CRAB_REPO "https://github.com/caballa/crab.git" CACHE STRING "crab repo")
  
  if (TopLevel)
    set (CRAB_ROOT "${CMAKE_SOURCE_DIR}/crab" CACHE STRING "crab root directory")
  else ()
    set (CRAB_ROOT "${CMAKE_SOURCE_DIR}/crab-llvm/crab" CACHE STRING "crab root directory")
  endif ()
  
  add_custom_target (crab-git
     		     ${GIT_EXECUTABLE} clone -b "crab-llvm" ${CRAB_REPO} ${CRAB_ROOT})
  
  add_custom_target (crab ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_LIST_FILE}
                     DEPENDS crab-git)    

else ()
  if (TopLevel)
    message (STATUS "Could not find git. Not adding 'extra' and 'crab' targets.")
  endif ()
endif ()


#### begin crab stuff ####
if (IS_DIRECTORY ${CRAB_ROOT} AND EXISTS ${CRAB_ROOT}/CMakeLists.txt)
 add_subdirectory(${CRAB_ROOT})
 include_directories (${CRAB_INCLUDE_DIRS})
else()
  message (WARNING "No crab found. Run \n\tcmake --build . --target crab && cmake ${CMAKE_SOURCE_DIR}")
  return ()
endif()
#### end crab stuff ####

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
    -DLLVM_BUILD_LLVM_DYLIB:BOOL=${BUILD_CRABLLVM_LIBS_SHARED}
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

  #find_library(RT_LIB NAMES rt)
  #if (NOT RT_LIB)
  #  set(RT_LIB "")
  #endif()
  #mark_as_advanced(RT_LIB)
  #find_package(Curses)
endif()

add_definitions(-std=c++11)

add_definitions(-Wno-redeclared-class-member)
add_definitions(-Wno-sometimes-uninitialized)
add_definitions(-Wno-deprecated-declarations)
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
include_directories (${CMAKE_BINARY_DIR}/include)
add_subdirectory(lib)

configure_file(include/crab_llvm/config.h.cmake
               ${CMAKE_BINARY_DIR}/include/crab_llvm/config.h )
 

if (TopLevel)
  add_subdirectory(tools)
  add_subdirectory(py)
  add_subdirectory(tests)
endif()

# Propagate to the parent
if (NOT TopLevel)
   set (CRABLLVM_LIBS
       CrabLlvmAnalysis CrabLlvmInstrumentation
       ${CRAB_LIBS}
       PARENT_SCOPE)
    
   set (CRABLLVM_INCLUDE_DIRS
        ${CrabLlvm_SOURCE_DIR}/include
        ${CMAKE_BINARY_DIR}/include ## crab-llvm config.h
        ${CRAB_INCLUDE_DIRS}
        PARENT_SCOPE)    
   #message (STATUS "Crab include headers=${CRAB_INCLUDE_DIRS}")
endif ()  


if (NOT TopLevel)
  set (CRABLLVM_LIBS
      CrabLlvmAnalysis CrabLlvmInstrumentation
      ${CRAB_LIBS}
      PARENT_SCOPE)

  set (CRAB_INCLUDE_DIRS ${CRAB_INCLUDE_DIR})
  if (EXISTS ${LDD_INCLUDE_DIR})
     set (CRAB_INCLUDE_DIRS ${CRAB_INCLUDE_DIRS} ${LDD_INCLUDE_DIR})
  endif ()
  if (EXISTS ${APRON_INCLUDE_DIR})
     set (CRAB_INCLUDE_DIRS ${CRAB_INCLUDE_DIRS} ${APRON_INCLUDE_DIR})
  endif ()   
  
  set (CRABLLVM_INCLUDE_DIRS
      ${CrabLlvm_SOURCE_DIR}/include
      ${CRAB_INCLUDE_DIRS}
      PARENT_SCOPE)  
endif ()  


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
endif ()


## Install files

install(DIRECTORY ${CrabLlvm_SOURCE_DIR}/include/crab_llvm DESTINATION include
     FILES_MATCHING
     PATTERN "*.hpp"
     PATTERN "*.hh"
     PATTERN "*.h"
     PATTERN "CMakeFiles" EXCLUDE
     PATTERN ".svn" EXCLUDE
)

if (TopLevel)

   if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/ext/llvm/LICENSE.TXT)
       install (FILES ext/llvm/LICENSE.TXT
         DESTINATION .
         RENAME llvm_license.txt)
   endif()
   
   install (FILES README.md DESTINATION .)
   install (FILES LICENSE DESTINATION .)
   
endif ()     
