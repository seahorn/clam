#pragma once

/*
 * Translate a LLVM function to a Crab CFG.
 */

#include "clam/CfgBuilderParams.hh"
#include "clam/CrabIREmitter.hh"
#include "clam/crab/crab_lang.hh"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Optional.h"

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
private:
  friend class CrabBuilderManagerImpl;

  // the actual cfg builder
  std::unique_ptr<CfgBuilderImpl> m_impl;

  CfgBuilder(const llvm::Function &func, CrabBuilderManagerImpl &man);

  void addFunctionDeclaration(void);

  void buildCfg(void);

public:
  CfgBuilder(const CfgBuilder &o) = delete;

  CfgBuilder &operator=(const CfgBuilder &o) = delete;

  ~CfgBuilder();

  // return crab control flow graph
  cfg_t &getCfg();

  // compute live symbols per block by running liveness analysis
  void computeLiveSymbols();
  // return live symbols at the end of block bb. Return None if
  // compute_live_symbols has not been called.
  llvm::Optional<varset_t> getLiveSymbols(const llvm::BasicBlock *bb) const;
  // return live LLVM symbols at the end of block bb. The returned
  // value will be empty whether no live symbols found or
  // compute_live_symbols has not been called. Note also that not all
  // Crab symbols are easily translated back to LLVM symbols. Thus,
  // some crab ghost variables can be ignored.
  llvm::DenseSet<const llvm::Value*>
  getLiveLLVMSymbols(const llvm::BasicBlock *bb) const;  
  // Low-level API: return live symbols for the whole cfg. Return
  // nullptr if compute_live_symbols has not been called.
  const liveness_t *getLiveSymbols() const;

  // heap abstraction for whole program
  HeapAbstraction &getHeapAbstraction();
  const HeapAbstraction &getHeapAbstraction() const;
  
  /***** Begin API to translate LLVM entities to Crab ones *****/  
  // map a llvm basic block to a crab basic block label
  basic_block_label_t getCrabBasicBlock(const llvm::BasicBlock *bb) const;
  // map a llvm edge to a crab basic block label.
  // return nullptr if the edge is not translated to a crab basic block.
  const basic_block_label_t *
  getCrabBasicBlock(const llvm::BasicBlock *src,
                    const llvm::BasicBlock *dst) const;
  // map a llvm Value to a Crab variable if possible
  llvm::Optional<var_t> getCrabVariable(const llvm::Value &v);
  // map the memory region to which v points to into a Crab region
  // variable if possible.
  llvm::Optional<var_t> getCrabRegionVariable(const llvm::Function &f, const llvm::Value &v);  
  /***** End API to translate LLVM entities to Crab ones *****/

  /***** Begin API to translate Crab entities to LLVM ones *****/    
  // Most crab operands have back pointers to LLVM operands so it
  // is often possible to find the corresponding LLVM
  // instruction for a given crab statement. Memory operations might not be the case.
  //
  // This method maps a memory/array crab statement to its
  // corresponding llvm instruction. Return null if the crab statement
  // cannot be mapped to a LLVM instruction.
  const llvm::Instruction *getInstruction(const statement_t &s) const;
  /***** End API to translate Crab entities to LLVM ones *****/      

}; // end class CfgBuilder

/**
 * A manager that keeps all the crab CFG builders.
 * A builder contains the crab CFG plus some extra information about
 * the translation.
 **/
class CrabBuilderManager {
public:
  CrabBuilderManager(CrabBuilderParams params,
                     llvm::TargetLibraryInfoWrapperPass &tli,
                     std::unique_ptr<HeapAbstraction> mem);

  CrabBuilderManager(CrabBuilderParams params,
                     llvm::TargetLibraryInfoWrapperPass &tli,
                     std::unique_ptr<HeapAbstraction> mem,
		     CrabIREmitterVec &&propertyEmitters);
  
  ~CrabBuilderManager();

  CrabBuilderManager(const CrabBuilderManager &o) = delete;

  CrabBuilderManager &operator=(const CrabBuilderManager &o) = delete;

  CfgBuilder &mkCfgBuilder(const llvm::Function &func);

  bool hasCfg(const llvm::Function &f) const;

  // pre: hasCfg(f) returns true
  cfg_t &getCfg(const llvm::Function &f);  
  const cfg_t &getCfg(const llvm::Function &f) const;

  // return null if mkCfgBuilder was not called on f
  CfgBuilder *getCfgBuilder(const llvm::Function &f);
  const CfgBuilder *getCfgBuilder(const llvm::Function &f) const;  

  variable_factory_t &getVarFactory();

  const CrabBuilderParams &getCfgBuilderParams() const;

  const llvm::TargetLibraryInfo &getTLI(const llvm::Function &) const;
  llvm::TargetLibraryInfoWrapperPass &getTLIWrapper() const;

  HeapAbstraction &getHeapAbstraction();
  const HeapAbstraction &getHeapAbstraction() const;    
private:
  std::unique_ptr<CrabBuilderManagerImpl> m_impl;
};

} // end namespace clam
