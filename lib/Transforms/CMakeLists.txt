add_library(LlvmPasses STATIC
  CrabIndVarSimplify.cc
  LowerCstExpr.cc
  LowerGvInitializers.cc
  LowerSelect.cc
  NameValues.cc
  RemoveUnreachableBlocksPass.cc
  MarkInternalInline.cc
  )

if (HAVE_DSA)
  target_link_libraries (LlvmPasses ${DSA_LIBS})
else ()
  target_link_libraries (LlvmPasses)
endif ()
