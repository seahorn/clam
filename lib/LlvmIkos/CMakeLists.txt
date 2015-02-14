add_library (LlvmIkosPasses STATIC
  CfgBuilder.cc
  LlvmIkos.cc
  )

#target_link_libraries (LlvmIkosPasses ${IKOS_DBM_LIB} z)

target_link_libraries (LlvmIkosPasses dbm z)

install(TARGETS LlvmIkosPasses ARCHIVE DESTINATION lib)


