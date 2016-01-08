set (CRAB_LIBS 
  ${CRAB_DBM_LIB}
  ${CRAB_TERM_LIB}
  ${GMPXX_LIB}
  ${GMP_LIB}
  )

if (USE_LDD)
  set (CRAB_LIBS ${CRAB_LIBS} ${LDD_LIBRARY}) 
endif ()

add_library (CrabLlvmAnalysis ${CRABLLVM_LIBS_TYPE}  
  CfgBuilder.cc
  CrabLlvm.cc
  ConCrabLlvm.cc
  NameValues.cc
  )
  
target_link_libraries (CrabLlvmAnalysis ${CRAB_LIBS})

install(TARGETS CrabLlvmAnalysis 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

add_library (CrabLlvmInstrumentation ${CRABLLVM_LIBS_TYPE}
  InsertInvariants.cc
  )

install(TARGETS CrabLlvmInstrumentation 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

# Needed if shared libraries
install(FILES ${CRAB_DBM_LIB} DESTINATION lib)
install(FILES ${CRAB_TERM_LIB} DESTINATION lib)
