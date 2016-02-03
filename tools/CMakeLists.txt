add_definitions(-D__STDC_CONSTANT_MACROS)
add_definitions(-D__STDC_LIMIT_MACROS)

set (USED_LIBS 
  ${Boost_SYSTEM_LIBRARY}
  ${GMPXX_LIB}
  ${GMP_LIB}
  ${RT_LIB})

set(LLVM_LINK_COMPONENTS 
  irreader 
  bitwriter 
  ipo 
  scalaropts 
  instrumentation 
  core 
  codegen 
  objcarcopts)

add_executable(crabllvm-pp crabllvm-pp.cc)
target_link_libraries (crabllvm-pp LlvmPasses ${USED_LIBS} ${LLVM_SEAHORN_LIBS})
llvm_config (crabllvm-pp ${LLVM_LINK_COMPONENTS})
install(TARGETS crabllvm-pp RUNTIME DESTINATION bin)

add_executable(crabllvm crabllvm.cc)
target_link_libraries (crabllvm
  CrabLlvmAnalysis 
  CrabLlvmInstrumentation 
  LlvmPasses 
  ${USED_LIBS}
  ${LLVM_SEAHORN_LIBS})
if (HAVE_DSA)
  target_link_libraries (crabllvm ${DSA_LIBS})
endif ()
llvm_config (crabllvm ${LLVM_LINK_COMPONENTS})
install(TARGETS crabllvm RUNTIME DESTINATION bin)

add_executable(llvm-profiler llvm-profiler.cc)
target_link_libraries (llvm-profiler LlvmAnalysisPasses)
llvm_config (llvm-profiler ${LLVM_LINK_COMPONENTS})
install(TARGETS llvm-profiler RUNTIME DESTINATION bin)

add_executable(llvm-lint llvm-lint.cc)
target_link_libraries (llvm-lint LlvmAnalysisPasses)
llvm_config (llvm-lint ${LLVM_LINK_COMPONENTS})
install(TARGETS llvm-lint RUNTIME DESTINATION bin)

if (CRABLLVM_STATIC_EXE)
  set (CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties (crabllvm PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties (crabllvm PROPERTIES LINK_SEARCH_END_STATIC ON)

  set (CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties (crabllvm-pp PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties (crabllvm-pp PROPERTIES LINK_SEARCH_END_STATIC ON)

  set (CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties (llvm-profiler PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties (llvm-profiler PROPERTIES LINK_SEARCH_END_STATIC ON)

  set (CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties (llvm-lint PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties (llvm-lint PROPERTIES LINK_SEARCH_END_STATIC ON)
endif()