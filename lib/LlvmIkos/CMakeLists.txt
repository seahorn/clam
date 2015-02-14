add_library (LlvmIkosPasses SHARED
  CfgBuilder.cc
  LlvmIkos.cc
  )

target_link_libraries (LlvmIkosPasses dbm z)

install(TARGETS LlvmIkosPasses LIBRARY DESTINATION lib)


