set (CRAB_LIBS 
  ${CRAB_DBM_LIB}
  ${CRAB_TERM_LIB}
  ${GMPXX_LIB}
  ${GMP_LIB}
  )

if (USE_LDD)
  set (CRAB_LIBS ${CRAB_LIBS} ${LDD_LIBRARY}) 
endif ()

if (USE_APRON)
  set (CRAB_LIBS ${CRAB_LIBS} ${APRON_LIBS}) 
endif ()

add_library (CrabLlvmAnalysis STATIC
  CfgBuilder.cc
  CrabLlvm.cc
  ConCrabLlvm.cc
  NameValues.cc
  )

target_link_libraries (CrabLlvmAnalysis ${CRAB_LIBS})
install(TARGETS CrabLlvmAnalysis ARCHIVE DESTINATION lib)

add_library (CrabLlvmInstrumentation STATIC
  InsertInvariants.cc
  )
install(TARGETS CrabLlvmInstrumentation ARCHIVE DESTINATION lib)

