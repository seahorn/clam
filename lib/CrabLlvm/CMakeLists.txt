if (USE_LDD)
  set (CRAB_LIBS ${CRAB_LIBS} ${LDD_LIBRARY}) 
endif ()

if (USE_APRON)
  set (CRAB_LIBS ${CRAB_LIBS} ${APRON_LIBS}) 
endif ()

add_library (CrabLlvmAnalysis ${CRABLLVM_LIBS_TYPE}  
  CfgBuilder.cc
  CrabLlvm.cc
  NameValues.cc
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

# Needed if shared libraries
if (BUILD_CRABLLVM_LIBS_SHARED)
  install(FILES ${CRAB_LIBS} DESTINATION lib)
  if (USE_APRON)
    install(FILES ${APRON_LIBS} DESTINATION lib)
  endif ()
  if (USE_LDD)
    install(FILES ${LDD_LIBRARY} DESTINATION lib)
  endif ()
endif ()

