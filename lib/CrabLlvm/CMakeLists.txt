add_library (CrabLlvmAnalysis ${CRABLLVM_LIBS_TYPE}
  CfgBuilder.cc
  CrabLlvm.cc
  LlvmDsaHeapAbstraction.cc
  SeaDsaHeapAbstraction.cc
  NameValues.cc
  crab/path_analyzer.cc    
  )

target_link_libraries (CrabLlvmAnalysis ${CRAB_LIBS})
if (USE_COTIRE)
  cotire(CrabLlvmAnalysis)
endif ()

install(TARGETS CrabLlvmAnalysis 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

add_library (CrabLlvmInstrumentation ${CRABLLVM_LIBS_TYPE}
  InsertInvariants.cc
  )

target_link_libraries (CrabLlvmInstrumentation ${CRAB_LIBS})
if (USE_COTIRE)
  cotire(CrabLlvmInstrumentation)  
endif ()

install(TARGETS CrabLlvmInstrumentation 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)


