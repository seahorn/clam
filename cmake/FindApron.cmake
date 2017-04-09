#Find Apron library
if (NOT APRON_FOUND)

   set (APRON_ROOT "" CACHE PATH "Root of Apron install.")

   find_package (Gmp QUIET)
   find_package (Mpfr QUIET)

   find_path(APRON_INCLUDE_DIR NAMES ap_abstract0.h PATHS ${APRON_ROOT}/include)

   ### XXX: we grab static libraries
   
   find_library(Apron_Polka_Lib NAMES libpolkaMPQ.a PATHS ${APRON_ROOT}/lib)
   ## octD is faster than octMPQ
   find_library(Apron_Oct_Lib NAMES liboctD.a PATHS ${APRON_ROOT}/lib)
   # find_library(Apron_Oct_Lib NAMES octMPQ PATHS ${APRON_ROOT}/lib)
   find_library(Apron_Opt_Oct_Lib NAMES liboptoct.a PATHS ${APRON_ROOT}/lib)
   find_library(Apron_Opt_Oct_utils_Lib NAMES liblinkedlistapi.a PATHS ${APRON_ROOT}/lib)
   find_library(Apron_Apron_Lib NAMES libapron.a PATHS ${APRON_ROOT}/lib)
   find_library(Apron_Box_Lib NAMES libboxMPQ.a PATHS ${APRON_ROOT}/lib)
   find_library(Apron_Itv_Lib NAMES libitvMPQ.a PATHS ${APRON_ROOT}/lib)
   
   set(APRON_LIBRARY ${Apron_Polka_Lib} ${Apron_Oct_Lib} 
     ${Apron_Opt_Oct_Lib} ${Apron_Opt_Oct_utils_Lib}
     ${Apron_Apron_Lib} ${Apron_Box_Lib} ${Apron_Itv_Lib} )
   
   include (FindPackageHandleStandardArgs)
   find_package_handle_standard_args (Apron
     REQUIRED_VARS APRON_INCLUDE_DIR APRON_LIBRARY GMP_FOUND MPFR_FOUND)
   
   set (APRON_INCLUDE_DIR ${APRON_INCLUDE_DIR} ${MPFR_INC_DIR})
   set (APRON_LIBRARY ${APRON_LIBRARY} ${MPFR_LIB})
   
   mark_as_advanced(APRON_LIBRARY APRON_INCLUDE_DIR 
     Apron_Apron_Lib Apron_Box_Lib Apron_Itv_Lib Apron_Oct_Lib Apron_Polka_Lib )

endif ()
