add_definitions(-D__STDC_CONSTANT_MACROS)
add_definitions(-D__STDC_LIMIT_MACROS)

set(LLVM_LINK_COMPONENTS 
  irreader 
  bitwriter 
  ipo 
  scalaropts 
  instrumentation 
  core 
  codegen 
  objcarcopts)

add_executable(clam-pp clam-pp.cc)
target_link_libraries (clam-pp
  LlvmPasses
  ${LLVM_SEAHORN_LIBS}
 )
llvm_config (clam-pp ${LLVM_LINK_COMPONENTS})
install(TARGETS clam-pp RUNTIME DESTINATION bin)

add_executable(clam clam.cc)
target_link_libraries (clam
  ClamAnalysis 
  ClamInstrumentation 
  LlvmPasses
  ${LLVM_SEAHORN_LIBS}
  ${DSA_LIBS}
  ${SEA_DSA_LIBS}
)
llvm_config (clam ${LLVM_LINK_COMPONENTS})
install(TARGETS clam RUNTIME DESTINATION bin)

if (CLAM_STATIC_EXE)
  set (CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties (clam PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties (clam PROPERTIES LINK_SEARCH_END_STATIC ON)

  set (CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties (clam-pp PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties (clam-pp PROPERTIES LINK_SEARCH_END_STATIC ON)
endif()
