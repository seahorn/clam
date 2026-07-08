if(TopLevel)
  set(CRAB_SOURCE_DIR "${CMAKE_SOURCE_DIR}/crab" CACHE STRING "crab source directory")
else()
  set(CRAB_SOURCE_DIR "${CMAKE_SOURCE_DIR}/clam/crab" CACHE STRING "crab source directory" FORCE)
endif()
set(CRAB_REPO "https://github.com/seahorn/crab.git" CACHE STRING "crab repo")
set(CRAB_BRANCH "dev" CACHE STRING "crab branch")
add_custom_target(crab-git
  ${GIT_EXECUTABLE} clone -b ${CRAB_BRANCH} ${CRAB_REPO} ${CRAB_SOURCE_DIR})
