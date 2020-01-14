add_llvm_library (ClamAnalysis ${CLAM_LIBS_TYPE}
  CfgBuilder.cc
  CfgBuilderLit.cc
  CfgBuilderUtils.cc
  CfgBuilderShadowMem.cc  
  Clam.cc
  LlvmDsaHeapAbstraction.cc
  SeaDsaHeapAbstraction.cc
  SeaDsaHeapAbstractionUtils.cc
  SeaDsaHeapAbstractionDsaToRegion.cc
  NameValues.cc
  crab/path_analyzer.cc    
  )

llvm_map_components_to_libnames(LLVM_LIBS
  support core irreader bitwriter instrumentation ipo scalaropts codegen objcarcopts)

target_link_libraries (ClamAnalysis
  ${CRAB_LIBS}
  ## Needed if dynamic linking
  ${LLVM_LIBS}
  ${SEA_DSA_LIBS})
if (HAVE_DSA)
  ## Needed if dynamic linking
  add_definitions(-Wno-undefined-var-template)
  target_link_libraries(ClamAnalysis ${DSA_LIBS})
endif()

if (USE_COTIRE)
  cotire(ClamAnalysis)
endif ()

install(TARGETS ClamAnalysis 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

add_library (ClamInstrumentation ${CLAM_LIBS_TYPE}
  InsertInvariants.cc
  )

target_link_libraries(ClamInstrumentation
  ${CRAB_LIBS}
  ## Needed if dynamic linking
  ClamAnalysis
  ${LLVM_LIBS})

if (USE_COTIRE)
  cotire(ClamInstrumentation)  
endif ()

install(TARGETS ClamInstrumentation 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)


