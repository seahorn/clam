# Find LDD
if (NOT LDD_FOUND)

  set(LDD_ROOT "" CACHE PATH "Root of LDD compiled source tree.")

  #set(CUDD_ROOT ${LDD_ROOT}/cudd-2.4.2)
  set(CUDD_ROOT ${LDD_ROOT})
  find_package (Cudd QUIET)

  find_path(LDD_INCLUDE_DIR NAMES  ldd.h tvpi.h PATHS ${LDD_ROOT}/include/ldd)
  #find_path(LDD_INCLUDE_DIR NAMES  ldd.h tvpi.h PATHS ${LDD_ROOT}/src/include)
  find_library(LDD_TVPI_LIBRARY   NAMES tvpi   PATHS  ${LDD_ROOT}/lib)
  #find_library(LDD_TVPI_LIBRARY   NAMES tvpi   PATHS  ${LDD_ROOT}/src/tvpi)
  find_library(LDD_LDD_LIBRARY   NAMES ldd   PATHS  ${LDD_ROOT}/lib)
  #find_library(LDD_LDD_LIBRARY   NAMES ldd   PATHS  ${LDD_ROOT}/src/ldd)

  set(LDD_LIBRARY ${LDD_LDD_LIBRARY} ${LDD_TVPI_LIBRARY})
  include (FindPackageHandleStandardArgs)
  find_package_handle_standard_args(Ldd
    REQUIRED_VARS LDD_INCLUDE_DIR LDD_LIBRARY CUDD_FOUND)
  if (LDD_FOUND)  
     set (LDD_CXXFLAGS "${CUDD_CXXFLAGS}")
     set(LDD_LIBRARY ${LDD_LIBRARY} ${CUDD_LIBRARY})
     set (LDD_INCLUDE_DIR ${LDD_INCLUDE_DIR} ${CUDD_INCLUDE_DIR})
     mark_as_advanced (LDD_INCLUDE_DIR LDD_TVPI_LIBRARY 
                       LDD_LDD_LIBRARY LDD_LIBRARY LDD_CXXFLAGS)
  endif () 

endif ()
