add_llvm_library (LlvmIkosPasses
  CfgBuilder.cc
  LlvmIkos.cc
  )

target_link_libraries (LlvmIkosPasses dbm z)

