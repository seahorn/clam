set(SEADSA_SOURCE_DIR "${CMAKE_SOURCE_DIR}/sea-dsa" CACHE STRING "seadsa source directory")
if (TopLevel)
  set(SEA_DSA_REPO "https://github.com/seahorn/sea-dsa" CACHE STRING "sea-dsa repo")
  add_custom_target(sea-dsa-git
    ${GIT_EXECUTABLE} clone -b dev14 ${SEA_DSA_REPO} ${SEADSA_SOURCE_DIR})
endif()
