add_library(LlvmPasses STATIC
  IkosIndVarSimplify.cc
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  NameValues.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  )

target_link_libraries (LlvmPasses)


