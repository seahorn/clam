add_library (LlvmIkosPasses STATIC
  CfgBuilder.cc
  LlvmIkos.cc
  )

target_link_libraries (LlvmIkosPasses ${IKOS_DBM_LIB} ${GMPXX_LIB} ${GMP_LIB})
install(TARGETS LlvmIkosPasses ARCHIVE DESTINATION lib)


