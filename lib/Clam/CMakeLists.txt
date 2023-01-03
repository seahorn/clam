add_llvm_library(ClamAnalysis ${CLAM_LIBS_TYPE} DISABLE_LLVM_LINK_LLVM_DYLIB
  CfgBuilder.cc
  CfgBuilderEmitters.cc  
  CfgBuilderLit.cc
  CfgBuilderMemRegions.cc
  CfgBuilderUtils.cc
  Clam.cc
  ClamQueryCache.cc
  NameValues.cc
  RegisterAnalysis.cc
  SeaDsaHeapAbstraction.cc
  SeaDsaHeapAbstractionUtils.cc
  SeaDsaToRegion.cc
  CrabDomainParser.cc
  crab/path_analysis/path_analyzer.cc
  crab/output/crabir/cfg_printer.cc
  crab/output/json/write_json.cc
  crab/output/json/read_json.cc
  crab/domains/intervals.cc
  crab/domains/boxes.cc
  crab/domains/dis_intervals.cc
  crab/domains/oct.cc
  crab/domains/pk.cc
  crab/domains/pk_pplite.cc  
  crab/domains/ric.cc
  crab/domains/sign_constant.cc
  crab/domains/split_dbm.cc
  crab/domains/split_oct.cc
  crab/domains/terms_dis_intervals.cc
  crab/domains/terms_intervals.cc
  crab/domains/terms_zones.cc
  crab/domains/wrapped_intervals.cc
  Support/BoostException.cc
  Support/CFGPrinter.cc
  Properties/MemoryCheckUtils.cc
  Properties/NullCheck.cc
  Properties/UafCheck.cc
  Properties/BndCheck.cc
  )

llvm_map_components_to_libnames(LLVM_LIBS
  support core irreader bitwriter instrumentation ipo scalaropts codegen objcarcopts)

target_link_libraries (ClamAnalysis
  PRIVATE
  ${CRAB_LIBS}
  ## Needed if dynamic linking
  ${LLVM_LIBS}
  ${SEA_DSA_LIBS})

if (USE_COTIRE)
  cotire(ClamAnalysis)
endif ()

install(TARGETS ClamAnalysis 
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

if (CLAM_INCLUDE_POST_TRANSFORMS)
  add_llvm_library (ClamOptimizer ${CLAM_LIBS_TYPE} DISABLE_LLVM_LINK_LLVM_DYLIB
    Optimizer/Optimizer.cc
    )
  
  target_link_libraries(ClamOptimizer
    PRIVATE 
    ${CRAB_LIBS}
    ## Needed if dynamic linking
    ClamAnalysis
    ${LLVM_LIBS})
  
  if (USE_COTIRE)
    cotire(ClamOptimizer)  
  endif ()
  
  install(TARGETS ClamOptimizer 
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib)
endif()  


