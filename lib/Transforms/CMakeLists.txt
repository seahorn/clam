add_library(LlvmPasses STATIC
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  NameValues.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  SimplifyAssume.cc
  DevirtFunctions.cc
  )

install(TARGETS LlvmPasses ARCHIVE DESTINATION lib)
