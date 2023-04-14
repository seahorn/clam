find_program(PYTHON python3 REQUIRED)

if (PYTHON)
  install(PROGRAMS clam.py DESTINATION bin)
  install(PROGRAMS clam-yaml.py DESTINATION bin)  
  install(FILES stats.py DESTINATION bin)
endif()
