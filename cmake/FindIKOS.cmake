if (NOT IKOS_FOUND)
  set (IKOS_ROOT "" CACHE PATH "Search path for ikos-core")
  message (STATUS "Ikos root: ${IKOS_ROOT}")
  find_path(IKOS_INCLUDE_DIR NAMES ikos/cfg/Cfg.hpp
    PATHS ${IKOS_ROOT}/include  NO_DEFAULT_PATH)
  find_library(IKOS_DBM_LIB NAMES dbm PATHS ${IKOS_ROOT}/lib NO_DEFAULT_PATH)
  include (FindPackageHandleStandardArgs)
  
  find_package_handle_standard_args(IKOS REQUIRED_VARS IKOS_INCLUDE_DIR IKOS_DBM_LIB) 
  mark_as_advanced(IKOS_ROOT IKOS_INCLUDE_DIR IKOS_DBM_LIB)
  # start from 1 to make cmakedefine happy
  set (IKOS_MAJOR_VERSION 1)
  if (EXISTS ${IKOS_ROOT}/lib/dbm/dbm.cpp)
    set (IKOS_MINOR_VERSION 1)
  else()
    set (IKOS_MINOR_VERSION 2)
  endif()
endif()
