add_library (LlvmIkosAnalysis STATIC
  CfgBuilder.cc
  LlvmCIkos.cc
  LlvmIkos.cc
  )

target_link_libraries (LlvmIkosAnalysis ${IKOS_DBM_LIB} ${GMPXX_LIB} ${GMP_LIB})
install(TARGETS LlvmIkosAnalysis ARCHIVE DESTINATION lib)

add_library (LlvmIkosInstrumentation STATIC
  InsertInvariants.cc
  )


