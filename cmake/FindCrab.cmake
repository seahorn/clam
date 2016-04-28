if (NOT CRAB_FOUND)
  set (CRAB_ROOT "" CACHE PATH "Search path for CRAB")
  message (STATUS "Given Crab root: ${CRAB_ROOT}")
  find_path(CRAB_INCLUDE_DIR NAMES crab/cfg/Cfg.hpp
    PATHS ${CRAB_ROOT}/include  NO_DEFAULT_PATH)
  #find_library(CRAB_DBM_LIB NAMES dbm PATHS ${CRAB_ROOT}/lib NO_DEFAULT_PATH)
  find_library(CRAB_TERM_LIB NAMES term PATHS ${CRAB_ROOT}/lib NO_DEFAULT_PATH)
  find_library(CRAB_DEBUG_LIB NAMES Debug PATHS ${CRAB_ROOT}/lib NO_DEFAULT_PATH)
  find_library(CRAB_STATS_LIB NAMES Stats PATHS ${CRAB_ROOT}/lib NO_DEFAULT_PATH)
  #message (STATUS "Crab libs: ${CRAB_TERM_LIB} ${CRAB_DEBUG_LIB} ${CRAB_STATS_LIB}")
  
  include (FindPackageHandleStandardArgs)
  find_package_handle_standard_args(CRAB REQUIRED_VARS CRAB_INCLUDE_DIR) 
  if (CRAB_FOUND)
    set (CRAB_LIBS 
         #${CRAB_DBM_LIB}
         ${CRAB_TERM_LIB}
         ${CRAB_DEBUG_LIB}
         ${CRAB_STATS_LIB}
         ${GMPXX_LIB}
         ${GMP_LIB}
         )

    mark_as_advanced(CRAB_ROOT CRAB_INCLUDE_DIR 
                     #CRAB_DBM_LIB
                     CRAB_TERM_LIB 
		 CRAB_DEBUG_LIB
		 CRAB_STATS_LIB)
    message (STATUS "Found Crab at ${CRAB_INCLUDE_DIR}")
    # start from 1 to make cmakedefine happy
    # set (CRAB_MAJOR_VERSION 1)
    # if (CRAB_DBM_LIB)
    #   set (CRAB_MINOR_VERSION 2)
    # else()
    #   set (CRAB_MINOR_VERSION 1)
    #   set (CRAB_DBM_LIB CACHE FILEPATH "" FORCE)
    #   set (CRAB_TERM_LIB CACHE FILEPATH "" FORCE)
    # endif()
  endif()
endif()
