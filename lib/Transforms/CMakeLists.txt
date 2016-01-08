add_library(LlvmPasses ${CRABLLVM_LIBS_TYPE}
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  SimplifyAssume.cc
  DevirtFunctions.cc
  )

install(TARGETS LlvmPasses 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
