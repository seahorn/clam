add_llvm_library(LlvmPasses ${CLAM_LIBS_TYPE}
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  LowerUnsignedICmp.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  DevirtFunctions.cc
  DevirtFunctionsPass.cc
  ExternalizeAddressTakenFunctions.cc
  PromoteAssume.cc  
  PromoteMalloc.cc
  Scalarizer.cc
  )

llvm_map_components_to_libnames(LLVM_LIBS
  support core irreader bitwriter instrumentation ipo scalaropts codegen objcarcopts)

target_link_libraries(LlvmPasses
  ## Needed if dynamic linking
  ${LLVM_LIBS})

if (HAVE_DSA)
  add_definitions(-Wno-undefined-var-template)
  target_link_libraries(LlvmPasses ${DSA_LIBS})
endif()

target_link_libraries(LlvmPasses ${SEA_DSA_LIBS})

install(TARGETS LlvmPasses 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
