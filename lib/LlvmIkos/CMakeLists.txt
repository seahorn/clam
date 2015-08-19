add_library (LlvmIkosAnalysis STATIC
  CfgBuilder.cc
  LlvmCIkos.cc
  LlvmIkos.cc
  )

set (IKOS_CORE_LIBS 
  ${IKOS_DBM_LIB}
  ${IKOS_TERM_LIB}
  ${GMPXX_LIB}
  ${GMP_LIB}
  )

target_link_libraries (LlvmIkosAnalysis ${IKOS_CORE_LIBS})
install(TARGETS LlvmIkosAnalysis ARCHIVE DESTINATION lib)

add_library (LlvmIkosInstrumentation STATIC
  InsertInvariants.cc
  )


