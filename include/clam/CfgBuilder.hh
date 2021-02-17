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

namespace clam {

/**
    Build a Crab CFG from LLVM Function
**/
class CrabBuilderManagerImpl;
class IntraClam_Impl;
class InterClam_Impl;

class CfgBuilder {
public:
  using liveness_t = crab::analyzer::live_and_dead_analysis<cfg_ref_t>;
  using varset = typename liveness_t::varset_domain_t;

private:
  friend class CrabBuilderManagerImpl;

  // the actual cfg builder
  std::unique_ptr<CfgBuilderImpl> m_impl;
  // live and dead symbols
  std::unique_ptr<liveness_t> m_ls;

  CfgBuilder(const llvm::Function &func, CrabBuilderManagerImpl &man);

  void addFunctionDeclaration(void);

  void buildCfg(void);

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
  // Low-level API: return live symbols for the whole cfg. Return
  // nullptr if compute_live_symbols has not been called.
  const liveness_t *getLiveSymbols() const;

  // map a llvm basic block to a crab basic block label
  basic_block_label_t getCrabBasicBlock(const llvm::BasicBlock *bb) const;

  // map a llvm edge to a crab basic block label.
  // return nullptr if the edge is not translated to a crab basic block.
  const basic_block_label_t *
  getCrabBasicBlock(const llvm::BasicBlock *src,
                    const llvm::BasicBlock *dst) const;

  llvm::Optional<var_t> getCrabVariable(const llvm::Value *v);
  
  // DEPRECATED
  //
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
using CfgBuilderPtr = std::shared_ptr<clam::CfgBuilder>;

class CrabBuilderManager {
public:
  CrabBuilderManager(CrabBuilderParams params,
                     llvm::TargetLibraryInfoWrapperPass &tli,
                     std::unique_ptr<HeapAbstraction> mem);

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
private:
  std::unique_ptr<CrabBuilderManagerImpl> m_impl;
};

} // end namespace clam
