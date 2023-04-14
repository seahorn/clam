find_program(PYTHON python3 REQUIRED)

if (PYTHON)
  install(PROGRAMS read_results.py DESTINATION bin)
  install(PROGRAMS debug_assertion.py DESTINATION bin)
endif()
