add_llvm_library(LlvmPasses ${CLAM_LIBS_TYPE} DISABLE_LLVM_LINK_LLVM_DYLIB
  InsertEntryPoint.cc
  LowerCstExpr.cc
  LowerSelect.cc
  LowerUnsignedICmp.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  DevirtFunctions.cc
  DevirtFunctionsPass.cc
  ExternalizeFunctions.cc  
  ExternalizeAddressTakenFunctions.cc
  PromoteAssume.cc  
  PromoteMalloc.cc
  NondetInit.cc
  LoopPeeler.cc
  PropertyInstrumentation/MemoryCheck.cc
  PropertyInstrumentation/NullCheck.cc
  PropertyInstrumentation/UseAfterFreeCheck.cc  
  )

llvm_map_components_to_libnames(LLVM_LIBS
  support core irreader bitwriter instrumentation ipo scalaropts codegen objcarcopts)

target_link_libraries(LlvmPasses
  ${SEA_DSA_LIBS}
  ## Needed if dynamic linking
  ${LLVM_LIBS})

install(TARGETS LlvmPasses 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
