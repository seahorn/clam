add_llvm_library (CrabLlvmAnalysis ${CRABLLVM_LIBS_TYPE}
  CfgBuilder.cc
  CrabLlvm.cc
  LlvmDsaHeapAbstraction.cc
  SeaDsaHeapAbstraction.cc
  NameValues.cc
  crab/path_analyzer.cc    
  )

llvm_map_components_to_libnames(LLVM_LIBS
  support core irreader bitwriter instrumentation ipo scalaropts codegen objcarcopts)

target_link_libraries (CrabLlvmAnalysis
  ${CRAB_LIBS}
  ## Needed if dynamic linking
  ${LLVM_LIBS}
  ${SEA_DSA_LIBS})
if (HAVE_DSA)
  ## Needed if dynamic linking
  add_definitions(-Wno-undefined-var-template)
  target_link_libraries(CrabLlvmAnalysis ${DSA_LIBS})
endif()

if (USE_COTIRE)
  cotire(CrabLlvmAnalysis)
endif ()

install(TARGETS CrabLlvmAnalysis 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

add_library (CrabLlvmInstrumentation ${CRABLLVM_LIBS_TYPE}
  InsertInvariants.cc
  )

target_link_libraries(CrabLlvmInstrumentation
  ${CRAB_LIBS}
  ## Needed if dynamic linking
  CrabLlvmAnalysis
  ${LLVM_LIBS})

if (USE_COTIRE)
  cotire(CrabLlvmInstrumentation)  
endif ()

install(TARGETS CrabLlvmInstrumentation 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)


