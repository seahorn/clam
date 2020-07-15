#pragma once

/*
 * Translate a LLVM function to a Crab CFG.
 */

#include "clam/CfgBuilderParams.hh"
#include "clam/crab/crab_lang.hh"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"

#include "crab/analysis/dataflow/liveness.hpp"

#include <memory>

// forward declarations
namespace llvm {
class DataLayout;
class TargetLibraryInfo;
class TargetLibraryInfoWrapperPass;
class BasicBlock;
class Function;
class Twine;
class raw_ostream;
} // namespace llvm

namespace clam {
class HeapAbstraction;
class CfgBuilderImpl;
} // namespace clam

namespace seadsa {
class ShadowMem;
}

namespace clam {

/**
    Build a Crab CFG from LLVM Function
**/
class CrabBuilderManager;
class IntraClam_Impl;
class InterClam_Impl;

class CfgBuilder {
public:
  using liveness_t = crab::analyzer::live_and_dead_analysis<cfg_ref_t>;
  using varset = typename liveness_t::varset_domain_t;

private:
  friend class CrabBuilderManager;
  friend class IntraClamImpl;
  friend class InterClamImpl;

  // the actual cfg builder
  std::unique_ptr<CfgBuilderImpl> m_impl;
  // live and dead symbols
  std::unique_ptr<liveness_t> m_ls;

  CfgBuilder(const llvm::Function &func, CrabBuilderManager &man);

  void buildCfg();

  // return live symbols for the whole cfg. Return nullptr if
  // compute_live_symbols has not been called.
  // Only IntraClam_Impl and InterClam_Impl should call this method.
  const liveness_t *getLiveSymbols() const;

public:
  CfgBuilder(const CfgBuilder &o) = delete;

  CfgBuilder &operator=(const CfgBuilder &o) = delete;

  ~CfgBuilder();

  // return crab control flow graph
  cfg_t &getCfg();

  // compute live symbols per block by running standard liveness
  // analysis
  void computeLiveSymbols();

  // return live symbols at the end of block bb. Return None if
  // compute_live_symbols has not been called.
  llvm::Optional<varset> getLiveSymbols(const llvm::BasicBlock *bb) const;

  // map a llvm basic block to a crab basic block label
  basic_block_label_t getCrabBasicBlock(const llvm::BasicBlock *bb) const;

  // map a llvm edge to a crab basic block label.
  // return nullptr if the edge is not translated to a crab basic block.
  const basic_block_label_t *
  getCrabBasicBlock(const llvm::BasicBlock *src,
                    const llvm::BasicBlock *dst) const;

  // Most crab statements have back pointers to LLVM operands so it
  // is always possible to find the corresponding LLVM
  // instruction. Array crab operations are an exceptions.
  //
  // This method maps an **array** crab statement to its
  // corresponding llvm instruction. Return null if the the array
  // instruction is not mapped to a LLVM instruction.
  const llvm::Instruction *getInstruction(const statement_t &s) const;

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
                     llvm::TargetLibraryInfoWrapperPass &tli,
                     std::unique_ptr<HeapAbstraction> mem);

  // This constructor will use ShadowMem to translate LLVM memory
  // instructions to Crab arrays.
  CrabBuilderManager(CrabBuilderParams params,
                     llvm::TargetLibraryInfoWrapperPass &tli,
                     seadsa::ShadowMem &sm);

  ~CrabBuilderManager();

  CrabBuilderManager(const CrabBuilderManager &o) = delete;

  CrabBuilderManager &operator=(const CrabBuilderManager &o) = delete;

  CfgBuilderPtr mkCfgBuilder(const llvm::Function &func);

  bool hasCfg(const llvm::Function &f) const;

  cfg_t &getCfg(const llvm::Function &f) const;

  CfgBuilderPtr getCfgBuilder(const llvm::Function &f) const;

  variable_factory_t &getVarFactory();

  const CrabBuilderParams &getCfgBuilderParams() const;

  const llvm::TargetLibraryInfo &getTLI(const llvm::Function &) const;
  llvm::TargetLibraryInfoWrapperPass &getTLIWrapper() const;

  HeapAbstraction &getHeapAbstraction();

  const seadsa::ShadowMem *getShadowMem() const;

  seadsa::ShadowMem *getShadowMem();

private:
  // User-definable parameters for building the Crab CFGs
  CrabBuilderParams m_params;
  // Map LLVM function to Crab CfgBuilder
  llvm::DenseMap<const llvm::Function *, CfgBuilderPtr> m_cfg_builder_map;
  // Used for the translation from bitcode to Crab CFG
  llvm::TargetLibraryInfoWrapperPass &m_tli;
  // All CFGs created by this manager are created using the same
  // variable factory.
  variable_factory_t m_vfac;

  /// TODOX: hide details whether we use HeabAbstraction or ShadowMem.

  // Whole-program heap analysis
  std::unique_ptr<HeapAbstraction> m_mem;
  // Shadow memory (it can be null if not available)
  seadsa::ShadowMem *m_sm;
};

} // end namespace clam
