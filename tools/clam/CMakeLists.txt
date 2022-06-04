add_definitions(-D__STDC_CONSTANT_MACROS)
add_definitions(-D__STDC_LIMIT_MACROS)

if (NOT CLAM_BUILD_LIBS_SHARED)  
    llvm_map_components_to_libnames(LLVM_LIBS 
      irreader bitwriter ipo scalaropts instrumentation transformutils core codegen objcarcopts)
endif()    

add_llvm_executable(clam
  #DISABLE_LLVM_LINK_LLVM_DYLIB
  clam.cc)

target_link_libraries(clam PRIVATE
  ClamAnalysis
  ClamOptimizer
  LlvmPasses

  ${LLVM_LIBS}
  ${LLVM_SEAHORN_LIBS}
  ${SEA_DSA_LIBS}
  )

install(TARGETS clam RUNTIME DESTINATION bin)

if (CLAM_STATIC_EXE)
  set(CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties(clam PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties(clam PROPERTIES LINK_SEARCH_END_STATIC ON)
endif()
