cmake_minimum_required(VERSION 3.3)

project(Clam)
set (Clam_VERSION_MAJOR 14)
set (Clam_VERSION_MINOR 0)
set (Clam_VERSION_PATCH 0)
set (Clam_VERSION_TWEAK "rc0")

if(CMAKE_VERSION VERSION_GREATER "3.13.0")
cmake_policy(SET CMP0074 NEW)
cmake_policy(SET CMP0077 NEW)
endif()

## LLVM 14 requires c++14
set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR )
  message (FATAL_ERROR
    "In-source builds are not allowed. Please clean your source tree and try again.")
endif()

# determine if this is top-level or embedded project
if (PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME)
  set(TopLevel TRUE)
else()
  set(TopLevel FALSE)
endif()

if (NOT CRAB_ROOT)
  set(CRAB_ROOT "" CACHE PATH "Root of Crab source tree")
endif()

if (NOT SEADSA_ROOT)
  set(SEADSA_ROOT "" CACHE PATH "Root of sea-dsa source tree")
endif()  

# Save old CMAKE_FIND_LIBRARY_SUFFIXES
set(_CLAM_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})

# Default is release with debug info
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()

if (TopLevel)
  if (NOT PACKAGE_VERSION)
     set(PACKAGE_VERSION
       "${Clam_VERSION_MAJOR}.${Clam_VERSION_MINOR}.${Clam_VERSION_PATCH}")
     if (DEFINED Clam_VERSION_TWEAK)
       set (PACKAGE_VERSION "${PACKAGE_VERSION}-${Clam_VERSION_TWEAK}")
     endif()
     set (Clam_VERSION_INFO ${PACKAGE_VERSION})
  endif()
endif ()

#------- Clam options -------#
# By default, we include all the source code if we are not an
# embedded project, but user can change that.
option(CLAM_INCLUDE_TRANSFORMS "Include clam pre-processing transformations" ON)
option(CLAM_INCLUDE_SEAOPT "Include llvm-seahorn as extra package" ON)
option(CLAM_INCLUDE_POST_TRANSFORMS "Include clam post-processing transformations" ON)
option(CLAM_INCLUDE_PYTHON "Include clam python scripts" ON)
option(CLAM_INCLUDE_TESTS "Include clam tests" ON)
option(CLAM_INCLUDE_TOOLS "Include clam tools" ON)

if (NOT TopLevel)
  set(CLAM_INCLUDE_TRANSFORMS OFF)
  set(CLAM_INCLUDE_SEAOPT OFF)
  set(CLAM_INCLUDE_POST_TRANSFORMS OFF)  
  set(CLAM_INCLUDE_PYTHON OFF)
  set(CLAM_INCLUDE_TESTS OFF)
  set(CLAM_INCLUDE_TOOLS OFF)
else()  
  if (CLAM_INCLUDE_TRANSFORMS)
    message(STATUS "Clam: including LLVM pre-processing transformations")
  else()
    message(STATUS "Clam: excluding LLVM pre-processing transformations")
  endif()
  if (CLAM_INCLUDE_POST_TRANSFORMS)
    message(STATUS "Clam: including LLVM post-processing transformations")
  else()
    message(STATUS "Clam: excluding LLVM post-processing transformations")
  endif()
  if (CLAM_INCLUDE_PYTHON)
    message(STATUS "Clam: including python scripts")
  else()
    message(STATUS "Clam: excluding python scripts")
  endif()
  if (CLAM_INCLUDE_TESTS)
    message(STATUS "Clam: including tests")
  else()
    message(STATUS "Clam: excluding tests")
  endif()
  if (CLAM_INCLUDE_TOOLS)
    message(STATUS "Clam: including binary tools (clam and clam-pp)")
  else()
    message(STATUS "Clam: excluding binary tools (clam and clam-pp)")
  endif()
  if (CLAM_INCLUDE_SEAOPT)
    message(STATUS "Clam: including llvm-seahorn")
  else()
    message(STATUS "Clam: excluding llvm-seahorn")
  endif()
endif()

option(CLAM_USE_COTIRE "Use cotire to reduce compilation time" OFF)
option(CLAM_BUILD_LIBS_SHARED "Build all Clam libraries dynamically" OFF)
option(CLAM_USE_DBM_BIGNUM "Use big numbers for DBM weights" OFF)
option(CLAM_USE_DBM_SAFEINT "Use safe integers for DBM weights" OFF)
option(CLAM_INCLUDE_ALL_DOMAINS "Include all default abstract domains" ON)

if (CLAM_USE_DBM_BIGNUM)
  message(STATUS "Crab: DBM-based domains will use big numbers for representing weights")  
  set(USE_DBM_BIGNUM TRUE)
else()
  set(USE_DBM_BIGNUM FALSE)
  if (CLAM_USE_DBM_SAFEINT)
    message(STATUS "Crab: DBM-based domains will use safe integers for representing weights")  
    set(USE_DBM_SAFEINT TRUE)
  else()
    set(USE_DBM_SAFEINT FALSE)
  endif()
endif()

if (CLAM_INCLUDE_ALL_DOMAINS)
  set(INCLUDE_ALL_DOMAINS TRUE)
else()
  set(INCLUDE_ALL_DOMAINS FALSE)
endif()
if (TopLevel)
  set(CLAM_IS_TOPLEVEL TRUE)
else()
  set(CLAM_IS_TOPLEVEL FALSE)
endif()


# Add path for custom modules
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

#------- Static/Dynamic libraries -------#
# Support preference of static libs by adjusting CMAKE_FIND_LIBRARY_SUFFIXES
if (NOT CLAM_BUILD_LIBS_SHARED)
   set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ${CMAKE_FIND_LIBRARY_SUFFIXES})
endif ()   

if (CLAM_BUILD_LIBS_SHARED)
  message (STATUS "Clam: library built dynamically except its external components "
                   "sea-dsa and llvm-seahorn")
  set(CLAM_LIBS_TYPE SHARED)
  set(CRAB_BUILD_LIBS_SHARED ON)
  # sea-dsa provides this flag but it doesn't use it 
  # set(BUILD_SEA_DSA_LIBS_SHARED ON)
else()
  message (STATUS "Clam: libraries are built statically")
  set(CLAM_LIBS_TYPE STATIC)
  set(CRAB_BUILD_LIBS_SHARED OFF)
  #set(BUILD_SEA_DSA_LIBS_SHARED OFF)
endif ()

# so that executables outside the build tree can find later shared
# libraries
# Even if CLAM_LIBS_TYPE=STATIC we could have a mix of shared and
# static libraries
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(CMAKE_MACOSX_RPATH TRUE)  
endif ()  
if (NOT CMAKE_INSTALL_RPATH)
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif ()

#------- Boost -------#
if (TopLevel)  
  set (CUSTOM_BOOST_ROOT "" CACHE PATH "Path to custom boost installation.")
  if (CUSTOM_BOOST_ROOT)
    set (BOOST_ROOT ${CUSTOM_BOOST_ROOT})
    set (Boost_NO_SYSTEM_PATHS "ON")
  endif()
  option (CLAM_STATIC_EXE "Static executable." OFF)
  if (NOT CLAM_BUILD_LIBS_SHARED)  
    set (Boost_USE_STATIC_LIBS ON)
  endif ()
  find_package (Boost 1.65 REQUIRED)
  if (Boost_FOUND)
    include_directories (${Boost_INCLUDE_DIRS})
    if(NOT LLVM_ENABLE_EH)
      add_definitions(-DBOOST_NO_EXCEPTIONS)
    endif()
  endif ()
endif ()

#------- Cotire -------#
if (CLAM_USE_COTIRE)
  # Required by cotire
  set (ENV{CCACHE_SLOPPINESS} "pch_defines,time_macros")
  include(cotire)
endif ()

#------- Download external dependencies -------#

include(ExternalProject)
set_property(DIRECTORY PROPERTY EP_STEP_TARGETS configure build test)
find_package(Git)
if (GIT_FOUND)
  include(seadsa-git)
  include(llvm-seahorn-git)
  include(crab-git)
  
  if (TopLevel)
    if (CLAM_INCLUDE_SEAOPT)
      add_custom_target (extra DEPENDS sea-dsa-git seahorn-llvm-git)
    else()
      add_custom_target (extra DEPENDS sea-dsa-git)
    endif()
  endif ()
  add_custom_target (crab ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_LIST_FILE}
                     DEPENDS crab-git)    
else ()
  if (TopLevel)
    message (STATUS "Clam: could not find git. Not adding 'extra' and 'crab' targets.")
  endif ()
endif ()


if (TopLevel)
  find_package(LLVM 14 CONFIG)
  if (NOT LLVM_FOUND)
    ExternalProject_Get_Property (llvm INSTALL_DIR)
    set (LLVM_ROOT ${INSTALL_DIR})
    set (LLVM_DIR ${LLVM_ROOT}/lib/cmake/llvm CACHE PATH
      "Forced location of LLVM cmake config" FORCE)
    message (WARNING "No llvm found. Install LLVM 14.")
    return()
  else()
    message(STATUS "Clam: found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Clam: using LLVMConfig.cmake in: ${LLVM_DIR}")
    
    # We incorporate the CMake features provided by LLVM:
    list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
    include(AddLLVM)
    include(HandleLLVMOptions)
    set(LLVM_RUNTIME_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin)
    set(LLVM_LIBRARY_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib)
    
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LLVM_CXXFLAGS}")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LLVM_LDFLAGS}")
    
    include_directories(${LLVM_INCLUDE_DIRS})
    link_directories(${LLVM_LIBRARY_DIRS})
    add_definitions(${LLVM_DEFINITIONS})
    
    if (NOT LLVM_BUILD_TYPE STREQUAL CMAKE_BUILD_TYPE)
      message(WARNING
	"LLVM_BUILD_TYPE and CMAKE_BUILD_TYPE differ:\n"
	"\tLLMV_BUILD_TYPE=${LLVM_BUILD_TYPE}\n"
	"\tCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}\n"
	"Runtime errors might occur.")
      if (LLVM_BUILD_TYPE MATCHES "Release|RelWithDebInfo" AND 
          CMAKE_BUILD_TYPE MATCHES "Release|RelWithDebInfo")
	message(STATUS "Assuming that mixing Release and RelWithDebInfo is allowed.")
      else()
	ExternalProject_Get_Property (llvm INSTALL_DIR)
	set (LLVM_ROOT ${INSTALL_DIR})
	set (LLVM_DIR ${LLVM_ROOT}/lib/cmake/llvm CACHE PATH
	  "Forced location of LLVM cmake config" FORCE)
	message (WARNING "Incompatible mix of LLVM_BUILD_TYPE and CMAKE_BUILD_TYPE. Run \n\tcmake --build . && cmake ${CMAKE_SOURCE_DIR}")
      return()
       endif()
    endif()
  endif()
 endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")

## ------ sea-dsa ------- ##
if (TopLevel)
  if (IS_DIRECTORY "${SEADSA_ROOT}" AND EXISTS ${SEADSA_ROOT}/CMakeLists.txt)
    ## sea-dsa source code already exists out-of-the tree     
    message(STATUS "Clam: using sea-dsa found in ${SEADSA_ROOT}")
    include_directories (BEFORE ${SEADSA_ROOT}/include)
    set(SEADSA_BINARY_DIR sea-dsa) ## needed if SEADSA_ROOT out of tree
    add_subdirectory (${SEADSA_ROOT} ${SEADSA_BINARY_DIR})
    set(SEA_DSA_LIBS SeaDsaAnalysis)
  elseif (IS_DIRECTORY ${SEADSA_SOURCE_DIR} AND EXISTS ${SEADSA_SOURCE_DIR}/CMakeLists.txt)
    ## sea-dsa source code already exists in-tree    
    include_directories (BEFORE ${SEADSA_SOURCE_DIR}/include)
    set(SEADSA_BINARY_DIR sea-dsa) 
    add_subdirectory (${SEADSA_SOURCE_DIR} ${SEADSA_BINARY_DIR})
    set(SEA_DSA_LIBS SeaDsaAnalysis)
  else()
    message (WARNING "Clam: sea-dsa is required but not found in ${SEADSA_SOURCE_DIR}. Run\n\tcmake --build . --target extra && cmake ${CMAKE_SOURCE_DIR}")
    return()
  endif()
endif()

## ------ llvm-seahorn ------- ##
if (IS_DIRECTORY ${SEAHORN_LLVM_SOURCE_DIR} AND
    EXISTS ${SEAHORN_LLVM_SOURCE_DIR}/lib/CMakeLists.txt)
  set (HAVE_LLVM_SEAHORN TRUE)
endif()

if (TopLevel)
  if (HAVE_LLVM_SEAHORN)
    include_directories(BEFORE llvm-seahorn/include)
    add_subdirectory(llvm-seahorn/lib)
    add_subdirectory(llvm-seahorn/tools)
    set (LLVM_SEAHORN_LIBS SeaInstCombine SeaLoops)
  else()  
    message(WARNING "Clam: no llvm-seahorn found in ${CMAKE_SOURCE_DIR}/llvm-seahorn\n"
      "The lack of this package might affect both performance and precision of Clam.")
  endif()
endif()  

## ------ Crab ------- ##
# Using old policy allows to set CRAB_BUILD_LIBS_SHARED in Crab.
set(CMAKE_POLICY_DEFAULT_CMP0077 OLD)
if (IS_DIRECTORY "${CRAB_ROOT}" AND EXISTS ${CRAB_ROOT}/CMakeLists.txt)
  ## Crab source code already exists out-of-the tree   
  message(STATUS "Clam: using Crab found in ${CRAB_ROOT}")
  set(CRAB_BINARY_DIR crab) ## needed if CRAB_ROOT out of tree.
  add_subdirectory(${CRAB_ROOT} ${CRAB_BINARY_DIR})
  include_directories (BEFORE ${CRAB_INCLUDE_DIRS})
elseif(IS_DIRECTORY ${CRAB_SOURCE_DIR} AND EXISTS ${CRAB_SOURCE_DIR}/CMakeLists.txt)
  ## Crab source code already exists in-tree  
  set(CRAB_BINARY_DIR crab)
  set(CRAB_ROOT ${CRAB_SOURCE_DIR})
  add_subdirectory(${CRAB_SOURCE_DIR} ${CRAB_BINARY_DIR})
  include_directories (BEFORE ${CRAB_INCLUDE_DIRS})
else()
  message (WARNING "Clam: no crab found. Run \n\tcmake --build . --target crab && cmake ${CMAKE_SOURCE_DIR}")
  return ()
endif()

#---------------------------------------#

include_directories(BEFORE ${Clam_SOURCE_DIR}/include)
include_directories (BEFORE ${CMAKE_BINARY_DIR}/include)
add_subdirectory(lib)
configure_file(include/clam/config.h.cmake
               ${CMAKE_BINARY_DIR}/include/clam/config.h) 
if (CLAM_INCLUDE_TOOLS)
  add_subdirectory(tools)
endif()
if (CLAM_INCLUDE_PYTHON)
  add_subdirectory(py)
  add_subdirectory(scripts)
endif()
if (CLAM_INCLUDE_TESTS)
  configure_file(${Clam_SOURCE_DIR}/tests/clam_config.py.cmake
                 ${Clam_SOURCE_DIR}/tests/clam_config.py) 
  add_subdirectory(tests)
endif()

# restore old CMAKE_FIND_LIBRARY_SUFFIXES
set(CMAKE_FIND_LIBRARY_SUFFIXES ${_CLAM_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES})     

# HACK: propagate cmake variables to the parent project
if (NOT TopLevel)
   set(XCLAM_LIBS
     ClamAnalysis)

   if (CLAM_INCLUDE_POST_TRANSFORMS)
     set(XCLAM_LIBS ${XCLAM_LIBS} ClamOptimizer)
   endif()
   
   if (CLAM_INCLUDE_TRANSFORMS)
     set(XCLAM_LIBS ${XCLAM_LIBS} LlvmPasses)
   endif()
  
   set(CLAM_LIBS
       ${XCLAM_LIBS}
       ${CRAB_LIBS}
       PARENT_SCOPE)
     
   set(CLAM_INCLUDE_DIRS
       ${Clam_SOURCE_DIR}/include
       ${CMAKE_BINARY_DIR}/include 
       ${CRAB_INCLUDE_DIRS}
       PARENT_SCOPE)    
endif ()  

#-------------- Install files --------------#
if (TopLevel)
  set (PACKAGE_NAME Clam)
  set (PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")
  
  # Configure CPack.
  include(TargetArch)
  target_architecture(CMAKE_TARGET_ARCH)
  #message ("-- arch: ${CMAKE_TARGET_ARCH}")
  set(CPACK_PACKAGE_INSTALL_DIRECTORY "Clam")
  set(CPACK_PACKAGE_VENDOR "Clam")
  set(CPACK_PACKAGE_VERSION_MAJOR ${Clam_VERSION_MAJOR})
  set(CPACK_PACKAGE_VERSION_MINOR ${Clam_VERSION_MINOR})
  set(CPACK_PACKAGE_VERSION_PATCH ${Clam_VERSION_PATCH})
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

install(DIRECTORY include/ DESTINATION include PATTERN "config.h.cmake" EXCLUDE)
install(FILES ${CMAKE_BINARY_DIR}/include/clam/config.h DESTINATION include/clam)
if (TopLevel)
  if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/ext/llvm/LICENSE.TXT)
    install(FILES ext/llvm/LICENSE.TXT
      DESTINATION .
      RENAME llvm_license.txt)
  endif()
  install(FILES README.md DESTINATION .)
  install(FILES LICENSE DESTINATION .)
endif ()     
