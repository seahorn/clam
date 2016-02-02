add_library(LlvmAnalysisPasses ${CRABLLVM_LIBS_TYPE}
  Profiler.cc
  )

install(TARGETS LlvmAnalysisPasses 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
