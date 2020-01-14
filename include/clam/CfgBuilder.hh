#pragma once

/* 
 * Translate a LLVM function to a Crab control-flow graph.
 */

#include "clam/crab/crab_cfg.hh"
#include "clam/CfgBuilderParams.hh"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"

#include <memory>

// forward declarations
namespace llvm {
  class DataLayout;
  class TargetLibraryInfo;
  class BasicBlock;
  class Function;
  class Twine;
  class raw_ostream;
}

namespace clam {
  class HeapAbstraction;
  class CfgBuilderImpl;
}

namespace sea_dsa {
  class ShadowMem;
}

namespace clam {

/** 
    Build a Crab CFG from LLVM Function
**/
class CrabBuilderManager;
  
class CfgBuilder {
  
  friend class CrabBuilderManager;
  
  std::unique_ptr<CfgBuilderImpl> m_impl;
  
  CfgBuilder(const llvm::Function& func, CrabBuilderManager& man);
  
  void build_cfg();
  
public:
  
  CfgBuilder(const CfgBuilder& o) = delete;
  
  CfgBuilder& operator=(const CfgBuilder& o) = delete;
  
  ~CfgBuilder();
  
  // return crab control flow graph
  cfg_t& get_cfg();
  
  // map a llvm basic block to a crab basic block label
  basic_block_label_t get_crab_basic_block(const llvm::BasicBlock *bb) const;

  // map a llvm edge to a crab basic block label.
  // return nullptr if the edge is not translated to a crab basic block.
  const basic_block_label_t*
  get_crab_basic_block(const llvm::BasicBlock *src,
		       const llvm::BasicBlock *dst) const;
  
  // Most crab statements have back pointers to LLVM operands so it
  // is always possible to find the corresponding LLVM
  // instruction. Array crab operations are an exceptions.
  // 
  // This method maps an **array** crab statement to its
  // corresponding llvm instruction. Return null if the the array
  // instruction is not mapped to a LLVM instruction.
  const llvm::Instruction* get_instruction(const statement_t& s) const;
  
}; // end class CfgBuilder
  
  
/**
 * A manager that keeps all the crab CFG builders.
 * A builder contains the crab CFG plus some extra information about
 * the translation.
 **/
class CrabBuilderManager {
public:
  using CfgBuilderPtr = std::shared_ptr<clam::CfgBuilder>;

  // This constructor will use HeapAbstraction to translate LLVM
  // memory instructions to Crab arrays.
  CrabBuilderManager(CrabBuilderParams params,
		     const llvm::TargetLibraryInfo &tli,
		     std::unique_ptr<HeapAbstraction> mem);

  // This constructor will use ShadowMem to translate LLVM memory
  // instructions to Crab arrays.
  CrabBuilderManager(CrabBuilderParams params,
		     const llvm::TargetLibraryInfo &tli,
		     sea_dsa::ShadowMem &sm);
  
  ~CrabBuilderManager();
  
  CrabBuilderManager(const CrabBuilderManager& o) = delete;
  
  CrabBuilderManager& operator=(const CrabBuilderManager& o) = delete;
  
  CfgBuilderPtr mk_cfg_builder(const llvm::Function& func); 
  
  bool has_cfg(const llvm::Function &f) const;
  
  cfg_t& get_cfg(const llvm::Function &f) const;
  
  CfgBuilderPtr get_cfg_builder(const llvm::Function &f) const;
  
  variable_factory_t& get_var_factory();
  
  const CrabBuilderParams& get_cfg_builder_params() const;
  
  const llvm::TargetLibraryInfo& get_tli() const ;
  
  HeapAbstraction& get_heap_abstraction();

  const sea_dsa::ShadowMem* get_shadow_mem() const;  
  
  sea_dsa::ShadowMem* get_shadow_mem();

private:
  
  // User-definable parameters for building the Crab CFGs
  CrabBuilderParams m_params;
  // Map LLVM function to Crab CfgBuilder
  llvm::DenseMap<const llvm::Function*, CfgBuilderPtr> m_cfg_builder_map;
  // Used for the translation from bitcode to Crab CFG
  const llvm::TargetLibraryInfo &m_tli;
  // All CFGs supervised by this manager are created using the same
  // variable factory.
  variable_factory_t m_vfac;
  // Whole-program heap analysis
  std::unique_ptr<HeapAbstraction> m_mem;
  // Shadow memory (it can be null if not available)
  sea_dsa::ShadowMem *m_sm;
};
  
} // end namespace clam
