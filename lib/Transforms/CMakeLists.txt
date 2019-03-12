add_library(LlvmPasses ${CRABLLVM_LIBS_TYPE}
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  LowerUnsignedICmp.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  SimplifyAssume.cc
  DevirtFunctions.cc
  DevirtFunctionsPass.cc
  ExternalizeAddressTakenFunctions.cc
  PromoteAssume.cc  
  PromoteMalloc.cc
  )

if (HAVE_DSA)
  add_definitions(-Wno-undefined-var-template)
  target_link_libraries(LlvmPasses ${DSA_LIBS})
endif()

install(TARGETS LlvmPasses 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
