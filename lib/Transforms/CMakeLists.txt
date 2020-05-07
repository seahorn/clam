add_llvm_library(LlvmPasses ${CLAM_LIBS_TYPE} DISABLE_LLVM_LINK_LLVM_DYLIB
  LowerCstExpr.cc
  LowerSelect.cc
  LowerUnsignedICmp.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  DevirtFunctions.cc
  DevirtFunctionsPass.cc
  ExternalizeAddressTakenFunctions.cc
  PromoteAssume.cc  
  PromoteMalloc.cc
  # LLVM 8.0 has already available this pass.
  #Scalarizer.cc
  )

llvm_map_components_to_libnames(LLVM_LIBS
  support core irreader bitwriter instrumentation ipo scalaropts codegen objcarcopts)

target_link_libraries(LlvmPasses
  ## Needed if dynamic linking
  ${LLVM_LIBS})

target_link_libraries(LlvmPasses ${SEA_DSA_LIBS})

install(TARGETS LlvmPasses 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
