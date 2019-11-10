#pragma once
/* 
 * Translate a LLVM function to a Crab control-flow graph.
 */

#include "clam/crab/crab_cfg.hh"
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

namespace clam {

  /** User-definable parameters to build a Crab CFG **/
  struct CrabBuilderParams {
    // Level of abstraction of the CFG
    crab::cfg::tracked_precision precision_level;
    // Perform dead code elimination, cfg simplifications, etc
    bool simplify;
    bool interprocedural;
    // Lower singleton aliases (e.g., globals) to scalar ones
    bool lower_singleton_aliases;
    // Ignore translation of LLVM pointer instructions to Crab
    // pointer instructions.
    // 
    // Memory instructions will be translated to Crab array
    // instructions if tracked precision >= ARR.
    bool ignore_ptr;
    // Remove useless havoc operations 
    bool include_useless_havoc;
    // Initialization of arrays for weak Crab array domains (e.g., smashing)
    bool initialize_arrays;
    // More aggressive initialization of arrays.
    // 
    // Initialization of allocation sites originated from calloc or
    // memset instructions may be unsound if it can be executed by
    // more than one execution.
    bool aggressive_initialize_arrays;
    // Translate bignums (> 64), otherwise operations with big numbers
    // are havoced.
    bool enable_bignums;
    //// --- printing options
    // print the cfg after it has been built
    bool print_cfg;
    
    CrabBuilderParams():
        precision_level(crab::cfg::NUM)
      , simplify(false)
      , interprocedural(true)
      , lower_singleton_aliases(false)
      , ignore_ptr(false)
      , include_useless_havoc(true)
      , initialize_arrays(true)
      , aggressive_initialize_arrays(false)
      , enable_bignums(false)
      , print_cfg(false) {}

    CrabBuilderParams(crab::cfg::tracked_precision _precision_level,
		      bool _simplify, bool _interprocedural, bool _lower_singleton_aliases,
		      bool _ignore_ptr, bool _include_useless_havoc, bool _initialize_arrays,
		      bool _aggressive_initialize_arrays, bool _enable_bignums,
		      bool _print_cfg):
      precision_level(_precision_level)
      , simplify(_simplify)
      , interprocedural(_interprocedural)
      , lower_singleton_aliases(_lower_singleton_aliases)
      , ignore_ptr(_ignore_ptr)
      , include_useless_havoc(_include_useless_havoc)
      , initialize_arrays(_initialize_arrays)
      , aggressive_initialize_arrays(_aggressive_initialize_arrays)
      , enable_bignums(_enable_bignums)
      , print_cfg(_print_cfg) {}

    bool track_pointers() const {
      return precision_level >= crab::cfg::PTR && !ignore_ptr;      
    }
    
    bool enabled_array_initialization() const {
      return precision_level == crab::cfg::ARR && initialize_arrays;
    }

    bool enabled_aggressive_array_initialization() const {
      return (precision_level == crab::cfg::ARR && initialize_arrays &&
	      aggressive_initialize_arrays);
    }

    /* Represent only booleans and integers */
    void set_num_precision() {
      precision_level = crab::cfg::NUM;
    }

    /* Represent booleans, integers, and pointers */
    void set_pointer_precision() {
      precision_level = crab::cfg::PTR;
      ignore_ptr = false;            
    }
    
    /* Represent booleans, integers, and arrays of those types */
    void set_array_precision() {
      precision_level = crab::cfg::ARR;
      ignore_ptr = true;      
      initialize_arrays = true;
    }
    
    void write(llvm::raw_ostream &o) const;
  };

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

    CrabBuilderManager(CrabBuilderParams params,
		       const llvm::TargetLibraryInfo *tli,
		       std::unique_ptr<HeapAbstraction> mem);
    
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
    
  private:
    
    // User-definable parameters for building the Crab CFGs
    CrabBuilderParams m_params;
    // Map LLVM function to Crab CfgBuilder
    llvm::DenseMap<const llvm::Function*, CfgBuilderPtr> m_cfg_builder_map;
    // Used for the translation from bitcode to Crab CFG
    const llvm::TargetLibraryInfo *m_tli;
    // All CFGs supervised by this manager are created using the same
    // variable factory.
    variable_factory_t m_vfac;
    // Whole-program heap analysis 
    std::unique_ptr<HeapAbstraction> m_mem;

  };
  
} // end namespace clam
