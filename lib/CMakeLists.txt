add_subdirectory (CrabLlvm)
if (TopLevel OR CrabTransforms)
  add_subdirectory(Transforms)
endif ()

