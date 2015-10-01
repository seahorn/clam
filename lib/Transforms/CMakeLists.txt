add_library(LlvmPasses STATIC
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  NameValues.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  SimplifyAssume.cc
  )

install(TARGETS LlvmPasses ARCHIVE DESTINATION lib)
