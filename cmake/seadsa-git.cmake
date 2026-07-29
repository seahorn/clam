set(SEADSA_SOURCE_DIR "${CMAKE_SOURCE_DIR}/sea-dsa" CACHE STRING "seadsa source directory")
if (TopLevel)
  set(SEA_DSA_REPO "https://github.com/seahorn/sea-dsa" CACHE STRING "sea-dsa repo")
  add_custom_target(sea-dsa-git
    # sea-dsa tracks the LLVM release on branches named dev<major>.
    ${GIT_EXECUTABLE} clone -b dev${CLAM_LLVM_VERSION} ${SEA_DSA_REPO} ${SEADSA_SOURCE_DIR})
endif()
