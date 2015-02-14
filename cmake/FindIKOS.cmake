if (NOT IKOS_FOUND)
  find_path(IKOS_INCLUDE_DIR NAMES Cfg.hpp PATHS ${IKOS_ROOT}/include/ikos_cfg NO_DEFAULT_PATH)
  get_filename_component(IKOS_INCLUDE_DIR ${IKOS_INCLUDE_DIR} DIRECTORY)
  find_library(IKOS_DBM_LIB NAMES dbm PATHS ${IKOS_ROOT}/lib NO_DEFAULT_PATH)

  include (FindPackageHandleStandardArgs)
  find_package_handle_standard_args(IKOS REQUIRED_VARS IKOS_INCLUDE_DIR IKOS_DBM_LIB) 
  mark_as_advanced(IKOS_SEARCH_PATH IKOS_INCLUDE_DIR IKOS_DBM_LIB)
endif()