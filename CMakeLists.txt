project(Clam)

cmake_minimum_required(VERSION 3.3)

set (Clam_VERSION_MAJOR 0)
set (Clam_VERSION_MINOR 1)
set (Clam_VERSION_PATCH 0)
#set (Clam_VERSION_TWEAK "rc1")

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR )
  message (FATAL_ERROR
    "In-source builds are not allowed. Please clean your source tree and try again.")
endif()

enable_testing()
include (CTest)

# By default, we don't include clam transformations if we are an
# embedded project, but user can change that.
option(CLAM_INCLUDE_TRANSFORMS OFF)

# Save old CMAKE_FIND_LIBRARY_SUFFIXES
set(_CLAM_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})

# determine if this is top-level or embedded project
if (PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME)
  set(TopLevel TRUE)
  set(CLAM_INCLUDE_TRANSFORMS ON)  
else()
  set(TopLevel FALSE)
endif()

if (CLAM_INCLUDE_TRANSFORMS)
  message(STATUS "Including clam transformations.")
else()
  message(STATUS "Excluding clam transformations.")
endif()

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

#### clam options ######
option (CLAM_USE_COTIRE "Use cotire to reduce compilation time" OFF)
option (CLAM_BUILD_LIBS_SHARED "Build all Clam libraries dynamically" OFF)
option (CLAM_ALL_DOMAINS "Include all crab abstract domains" ON)
if (CLAM_ALL_DOMAINS)
  set (HAVE_ALL_DOMAINS TRUE)
else ()
  message(STATUS "Some abstract domains are disabled to reduce compilation time")
  set (HAVE_ALL_DOMAINS FALSE)
endif ()

option (CLAM_ENABLE_INTER "Include crab inter-procedural analysis" ON)
if (CLAM_ENABLE_INTER)
  set (HAVE_INTER TRUE)
else ()
  message(STATUS "Inter-procedural analysis is disabled to reduce compilation time")
  set (HAVE_INTER FALSE)
endif ()

## only applicable if CLAM_ENABLE_INTER is ON
option (CLAM_TOP_DOWN_INTER "Use new top-down inter-procedural analysis" ON)
if (CLAM_TOP_DOWN_INTER)
  message(STATUS "Compiling with new top-down inter-procedural analysis")
  set(TOP_DOWN_INTER_ANALYSIS TRUE)
else()
  message(STATUS "Compiling with bottom-up inter-procedural analysis")
  set(TOP_DOWN_INTER_ANALYSIS FALSE)
endif()  
#### end clam options ######

# Add path for custom modules
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Support preference of static libs by adjusting CMAKE_FIND_LIBRARY_SUFFIXES
if (NOT CLAM_BUILD_LIBS_SHARED)
   set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ${CMAKE_FIND_LIBRARY_SUFFIXES})
endif ()   

if (CLAM_BUILD_LIBS_SHARED)
  message (STATUS "Clam libraries are built dynamically but not llvm, sea-dsa, llvm-dsa, and llvm-seahorn")
  set(CLAM_LIBS_TYPE SHARED)
  ## To build crab dynamically
  set(CRAB_BUILD_LIBS_SHARED ON)
else()
  message (STATUS "Clam libraries are built statically")
  set(CLAM_LIBS_TYPE STATIC)
  set(CRAB_BUILD_LIBS_SHARED OFF)  
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
  find_package (Boost 1.55)
  if (Boost_FOUND)
    include_directories (${Boost_INCLUDE_DIRS})
  endif ()
endif ()

if (CLAM_USE_COTIRE)
  # Required by cotire
  set (ENV{CCACHE_SLOPPINESS} "pch_defines,time_macros")
  include(cotire)
endif ()

include(ExternalProject)
set_property(DIRECTORY PROPERTY EP_STEP_TARGETS configure build test)

find_package (Git)

if (GIT_FOUND)
  if (TopLevel)
    set (LLVM_DSA_REPO "https://github.com/seahorn/llvm-dsa"
      CACHE STRING "llvm-dsa repo")
    set (SEA_DSA_REPO "https://github.com/seahorn/sea-dsa"
      CACHE STRING "sea-dsa repo")
    set (SEAHORN_LLVM_REPO "https://github.com/seahorn/llvm-seahorn"
      CACHE STRING "seahorn-llvm repo")

    add_custom_target (llvm-dsa-git
      ${GIT_EXECUTABLE} clone ${LLVM_DSA_REPO} ${CMAKE_SOURCE_DIR}/llvm-dsa)
    add_custom_target (sea-dsa-git
      ${GIT_EXECUTABLE} clone ${SEA_DSA_REPO} ${CMAKE_SOURCE_DIR}/sea-dsa)
    add_custom_target (seahorn-llvm-git
      ${GIT_EXECUTABLE} clone ${SEAHORN_LLVM_REPO} ${CMAKE_SOURCE_DIR}/llvm-seahorn)

    add_custom_target (extra DEPENDS llvm-dsa-git sea-dsa-git seahorn-llvm-git)
  endif ()

  set (CRAB_REPO "https://github.com/seahorn/crab.git" CACHE STRING "crab repo")
  
  if (TopLevel)
    set (CRAB_ROOT "${CMAKE_SOURCE_DIR}/crab" CACHE STRING "crab root directory")
  else ()
    set (CRAB_ROOT "${CMAKE_SOURCE_DIR}/clam/crab" CACHE STRING "crab root directory" FORCE)
  endif ()
  
  add_custom_target (crab-git
     		     ${GIT_EXECUTABLE} clone ${CRAB_REPO} ${CRAB_ROOT})
  
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
 include_directories (BEFORE ${CRAB_INCLUDE_DIRS})
else()
  message (WARNING "No crab found. Run \n\tcmake --build . --target crab && cmake ${CMAKE_SOURCE_DIR}")
  return ()
endif()
#### end crab stuff ####

if (TopLevel)
  option(CLAM_REQUIRES_RTTI "Enable RTTI support" ON)
  option(CLAM_REQUIRES_EH "Enable Exceptions" ON)

  # if top-level, offer to build llvm
  ExternalProject_Add (llvm
    SVN_REPOSITORY http://llvm.org/svn/llvm-project/llvm/tags/RELEASE_500/final/      
    SOURCE_DIR ${CMAKE_SOURCE_DIR}/ext/llvm
    INSTALL_DIR ${CMAKE_BINARY_DIR}/run
    CMAKE_ARGS
    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
    -DLLVM_TARGETS_TO_BUILD:STRING=X86 -DWITH_POLY:BOOL=OFF
    -DLLVM_ENABLE_PEDANTIC=OFF 
    -DLLVM_ENABLE_PIC=ON -DLLVM_REQUIRES_RTTI:BOOL=${CLAM_REQUIRES_RTTI}
    -DLLVM_BUILD_LLVM_DYLIB:BOOL=${CLAM_BUILD_LIBS_SHARED}
    -DLLVM_INCLUDE_TESTS:BOOL=OFF
    -DLLVM_INCLUDE_GO_TESTS=OFF
    -DLLVM_INCLUDE_EXAMPLES=OFF
    -DLLVM_INCLUDE_DOCS=OFF    
    -DLLVM_BINDINGS_LIST=" "
    LOG_CONFIGURE 1
    LOG_BUILD 1
    LOG_INSTALL 1)

  find_package (LLVM 5.0 CONFIG NO_DEFAULT_PATH)
  if (NOT LLVM_FOUND)
    ExternalProject_Get_Property (llvm INSTALL_DIR)
    set (LLVM_ROOT ${INSTALL_DIR})
    set (LLVM_DIR ${LLVM_ROOT}/lib/cmake/llvm CACHE PATH
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

  set(LLVM_REQUIRES_RTTI ${CLAM_REQUIRES_RTTI})
  set(LLVM_REQUIRES_EH ${CLAM_REQUIRES_EH})
  
  include_directories(${LLVM_INCLUDE_DIRS})
  link_directories(${LLVM_LIBRARY_DIRS})
  add_definitions(${LLVM_DEFINITIONS})
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-redeclared-class-member")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sometimes-uninitialized")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-declarations")
if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedefs")  
  else ()
  if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedef")
     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-inconsistent-missing-override")
  endif ()
endif ()

if (IS_DIRECTORY ${CMAKE_SOURCE_DIR}/llvm-dsa AND
    EXISTS ${CMAKE_SOURCE_DIR}/llvm-dsa/lib/DSA/Steensgaard.cpp)
  set (HAVE_DSA TRUE)
endif()

if (IS_DIRECTORY ${CMAKE_SOURCE_DIR}/sea-dsa AND
    EXISTS ${CMAKE_SOURCE_DIR}/sea-dsa/src/DsaAnalysis.cc)
  set (HAVE_SEA_DSA TRUE)
else()
  message (WARNING "No SEA DSA found in ${CMAKE_SOURCE_DIR}/sea-dsa. Run\n\tcmake --build . --target extra && cmake ${CMAKE_SOURCE_DIR}")
  return()
endif()

if (IS_DIRECTORY ${CMAKE_SOURCE_DIR}/llvm-seahorn AND
    EXISTS ${CMAKE_SOURCE_DIR}/llvm-seahorn/lib/CMakeLists.txt)
  set (HAVE_LLVM_SEAHORN TRUE)
endif()

if (TopLevel)
  if (HAVE_DSA)
    include_directories (AFTER llvm-dsa/include)
    add_subdirectory (llvm-dsa/lib/AssistDS)
    add_subdirectory (llvm-dsa/lib/DSA)
    set (DSA_LIBS DSA AssistDS)
    set (HAVE_DSA TRUE)
  else()
    set (DSA_LIBS "")
  endif()

  if (HAVE_SEA_DSA)
    include_directories (BEFORE sea-dsa/include)
    add_subdirectory (${CMAKE_SOURCE_DIR}/sea-dsa)
    set (SEA_DSA_LIBS SeaDsaAnalysis)
    set (HAVE_SEA_DSA TRUE)    
  else()
    set (SEA_DSA_LIBS "")
  endif()
  
  if (HAVE_LLVM_SEAHORN)
    include_directories (BEFORE llvm-seahorn/include)
    add_subdirectory(llvm-seahorn/lib)
    add_subdirectory(llvm-seahorn/tools)
    set (HAVE_LLVM_SEAHORN TRUE)
    set (LLVM_SEAHORN_LIBS SeaInstCombine SeaLoops SeaInstNondet)
  endif()

  if ((NOT HAVE_DSA) OR (NOT HAVE_SEA_DSA) OR (NOT HAVE_LLVM_SEAHORN))
    set(EXTRA_MSG  "")
    if (NOT HAVE_DSA)
      set(EXTRA_MSG "${EXTRA_MSG}\n - No LLVM-DSA in ${CMAKE_SOURCE_DIR}/llvm-dsa.")
    endif()
    if (NOT HAVE_SEA_DSA)
      set(EXTRA_MSG "${EXTRA_MSG}\n - No SEA-DSA in ${CMAKE_SOURCE_DIR}/sea-dsa.")
    endif()
    if (NOT HAVE_LLVM_SEAHORN)
      set(EXTRA_MSG "${EXTRA_MSG}\n - No LLVM-SEAHORN in ${CMAKE_SOURCE_DIR}/llvm-seahorn.")
    endif()
    message(WARNING "The following packages cannot be found:${EXTRA_MSG}\nRun cmake --build . --target extra && cmake ${CMAKE_SOURCE_DIR}")
  endif()  
  
endif ()

include_directories(BEFORE ${Clam_SOURCE_DIR}/include)
include_directories (BEFORE ${CMAKE_BINARY_DIR}/include)
add_subdirectory(lib)

configure_file(include/clam/config.h.cmake
               ${CMAKE_BINARY_DIR}/include/clam/config.h) 

if (TopLevel)
  add_subdirectory(tools)
  add_subdirectory(py)
  add_subdirectory(tests)
endif()

# restore old CMAKE_FIND_LIBRARY_SUFFIXES
set(CMAKE_FIND_LIBRARY_SUFFIXES ${_CLAM_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES})     

# Propagate to the parent
if (NOT TopLevel)
   set(XCLAM_LIBS
       ClamAnalysis ClamInstrumentation)
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

if (TopLevel)
   set (PACKAGE_NAME Clam)
   set (PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")

   # Configure CPack.
   include(TargetArch)
   target_architecture(CMAKE_TARGET_ARCH)
   message ("-- arch: ${CMAKE_TARGET_ARCH}")
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


## Install files

install(DIRECTORY include/ DESTINATION include
        PATTERN "config.h.cmake" EXCLUDE)
install(FILES ${CMAKE_BINARY_DIR}/include/clam/config.h DESTINATION include/clam)

if (TopLevel)

   if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/ext/llvm/LICENSE.TXT)
       install (FILES ext/llvm/LICENSE.TXT
         DESTINATION .
         RENAME llvm_license.txt)
   endif()
   
   install (FILES README.md DESTINATION .)
   install (FILES LICENSE DESTINATION .)
   
endif ()     
