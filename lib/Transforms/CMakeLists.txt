add_library(LlvmPasses STATIC
  CrabIndVarSimplify.cc
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  NameValues.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  )

install(TARGETS LlvmPasses ARCHIVE DESTINATION lib)
