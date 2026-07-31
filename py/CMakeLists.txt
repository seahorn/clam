find_program(PYTHON python3 REQUIRED)

if (PYTHON)
  # Record the LLVM toolchain location (from find_package(LLVM)) so the
  # installed clam.py can prefer a matching clang/opt/llvm-dis.
  configure_file(clam_config.py.in ${CMAKE_CURRENT_BINARY_DIR}/clam_config.py @ONLY)
  install(PROGRAMS clam.py DESTINATION bin)
  install(PROGRAMS clam-yaml.py DESTINATION bin)
  install(FILES stats.py DESTINATION bin)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/clam_config.py DESTINATION bin)
endif()
