add_definitions(-D__STDC_CONSTANT_MACROS)
add_definitions(-D__STDC_LIMIT_MACROS)

set (USED_LIBS 
  ${Boost_SYSTEM_LIBRARY}
  ${GMPXX_LIB}
  ${GMP_LIB}
  ${RT_LIB})

set(LLVM_LINK_COMPONENTS irreader bitwriter ipo scalaropts instrumentation core 
  codegen objcarcopts)

add_executable(llvmpp llvmpp.cc)
target_link_libraries (llvmpp LlvmPasses ${USED_LIBS})
llvm_config (llvmpp ${LLVM_LINK_COMPONENTS})
install(TARGETS llvmpp RUNTIME DESTINATION bin)

add_executable(llvmikos llvmikos.cc)
target_link_libraries (llvmikos LlvmIkosPasses LlvmPasses ${IKOS_DBM_LIB} ${USED_LIBS})
llvm_config (llvmikos ${LLVM_LINK_COMPONENTS})
install(TARGETS llvmikos RUNTIME DESTINATION bin)

if (LLVMIKOS_STATIC_EXE)
  set (CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++")
  set_target_properties (llvmpp PROPERTIES LINK_SEARCH_START_STATIC ON)
  set_target_properties (llvmpp PROPERTIES LINK_SEARCH_END_STATIC ON)
endif()