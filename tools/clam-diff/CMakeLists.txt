add_definitions(-D__STDC_CONSTANT_MACROS)
add_definitions(-D__STDC_LIMIT_MACROS)

set(LLVM_LINK_COMPONENTS 
  irreader 
  bitwriter 
  ipo 
  scalaropts 
  instrumentation
  transformutils
  core 
  codegen 
  objcarcopts)


add_llvm_executable(clam-diff DISABLE_LLVM_LINK_LLVM_DYLIB clam-diff.cc)
target_link_libraries(clam-diff PRIVATE
  ClamAnalysis
)

llvm_config(clam-diff ${LLVM_LINK_COMPONENTS})
install(TARGETS clam-diff RUNTIME DESTINATION bin)

if (CLAM_STATIC_EXE)
  set(CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties(clam-diff PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties(clam-diff PROPERTIES LINK_SEARCH_END_STATIC ON)
endif()
