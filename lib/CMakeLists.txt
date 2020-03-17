add_subdirectory(Clam)
if (TopLevel OR CLAM_INCLUDE_TRANSFORMS)
  add_subdirectory(Transforms)
endif ()

