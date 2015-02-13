add_llvm_library (LlvmPasses
  IkosIndVarSimplify.cc
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  NameValues.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  )

target_link_libraries (LlvmPasses)

add_library (LlvmIkosPasses SHARED
  CfgBuilder.cc
  LlvmIkos.cc
  )

target_link_libraries (LlvmIkosPasses dbm z)

install(TARGETS LlvmIkosPasses LIBRARY DESTINATION lib)

