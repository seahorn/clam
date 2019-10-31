#pragma once
/* 
 * Translate a LLVM function to a Crab control-flow graph.
 */

#include "clam/crab/crab_cfg.hh"
#include "llvm/ADT/DenseMap.h"

#include <memory>
#include <unordered_map>
#include <boost/functional/hash_fwd.hpp> // for hash_combine

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
    
    bool ignore_pointers() const {
      return precision_level >= crab::cfg::PTR && !ignore_ptr;
    }

    bool do_initialize_arrays() const {
      return precision_level == crab::cfg::ARR && initialize_arrays;
    }

    bool do_aggressive_initialize_arrays() const {
      return (precision_level == crab::cfg::ARR && initialize_arrays &&
	      aggressive_initialize_arrays);
    }
    
    // for precise array domains such array graph or array expansion
    // domain
    void set_array_precision() {
      precision_level = crab::cfg::ARR;
      lower_singleton_aliases = true;
      initialize_arrays = true;
    }

    // for weak array domains such as array smashing
    void set_array_precision_without_offsets() {
      set_array_precision();
      ignore_ptr = true;
    }

    void write(llvm::raw_ostream &o) const;
  };

  // forward declarations
  class crabLit;
  class crabLitFactoryImpl;
  
  typedef std::shared_ptr<crabLit> crab_lit_ref_t;
  
  /** 
      Factory to create crab literals: typed variable or number.
  **/
  class crabLitFactory {
  public:
    
    crabLitFactory(llvm_variable_factory &vfac, const CrabBuilderParams& params);

    ~crabLitFactory();
    
    llvm_variable_factory& get_vfac();

    crab::cfg::tracked_precision get_track() const;
    
    const CrabBuilderParams& get_cfg_builder_params() const;

    /** convert a Value to a crabLit **/
    crab_lit_ref_t getLit(const llvm::Value &v);

    /** make typed variables **/
    var_t mkIntVar(unsigned bitwidth);
    
    var_t mkBoolVar();
    
    var_t mkPtrVar();

    var_t mkIntArrayVar(unsigned bitwidth);
    
    var_t mkBoolArrayVar();
    
    var_t mkPtrArrayVar();
    
    template<typename Region> var_t mkArrayVar(Region r);
    
    template<typename Region> var_t mkArraySingletonVar(Region r);    

    /** direct accessors to crabLit subclasses **/
    bool isBoolTrue(const crab_lit_ref_t ref) const;
    
    bool isBoolFalse(const crab_lit_ref_t ref) const;

    bool isPtrNull(const crab_lit_ref_t ref) const;

    lin_exp_t getExp(const crab_lit_ref_t ref) const;
    
    number_t getIntCst(const crab_lit_ref_t ref) const;    
          
  private:
    crabLitFactoryImpl* m_impl;
  };

  struct pair_hash {
    template<typename T1, typename T2>
    std::size_t operator()(const std::pair<T1,T2> &p) const {
      std::size_t seed = 0;
      boost::hash_combine(seed, p.first);
      boost::hash_combine(seed, p.second);	
      return seed;
    }
  };

  /** 
     Build a Crab CFG from LLVM Function
  **/
  class CfgBuilder {
   public:
    
    CfgBuilder(const llvm::Function& func, 
	       llvm_variable_factory &vfac, HeapAbstraction &mem, 
	       const llvm::TargetLibraryInfo *tli,
	       const CrabBuilderParams &params);
    
    ~CfgBuilder();

    CfgBuilder(const CfgBuilder& o) = delete;
    
    CfgBuilder& operator=(const CfgBuilder& o) = delete;

    // main method
    void build_cfg();
    
    // return crab control flow graph
    cfg_t& get_cfg();

    // map a llvm basic block to a crab basic block label
    basic_block_label_t get_crab_basic_block(const llvm::BasicBlock *bb) const;

    // map a llvm edge to a crab basic block label.
    // return nullptr if the edge is not translated to a crab basic block.
    const basic_block_label_t*
    get_crab_basic_block(const llvm::BasicBlock *src,
			 const llvm::BasicBlock *dst) const;

    HeapAbstraction& get_heap_abstraction() {
      return m_mem;
    }

    const llvm::TargetLibraryInfo& get_tli() const {
      return *m_tli;
    }

    const llvm::DataLayout& get_dl() const {
      return *m_dl;
    }

    const CrabBuilderParams& get_cfg_builder_params() const {
      return m_params;
    }

    // Most crab statements have back pointers to LLVM operands so it
    // is always possible to find the corresponding LLVM
    // instruction. Array crab operations are an exceptions.
    // 
    // This method maps an **array** crab statement to its
    // corresponding llvm instruction. Return null if the the array
    // instruction is not mapped to a LLVM instruction.
    const llvm::Instruction* get_instruction(const statement_t& s) const;
    
   private:

    // map from a llvm basic block to a crab basic block id
    using node_to_crab_block_map_t = std::unordered_map<const llvm::BasicBlock*,
						      basic_block_label_t>;

    // map from a llvm edge to a crab basic block id
    using edge_to_crab_block_map_t = std::unordered_map<std::pair<const llvm::BasicBlock*,
								  const llvm::BasicBlock*>,
							basic_block_label_t, pair_hash>;
    
    // keep track whether the crab CFG has been built
    bool m_is_cfg_built;
    // The function should be const because it's never modified.
    llvm::Function &m_func;
    crabLitFactory m_lfac; 
    HeapAbstraction &m_mem;
    // the crab CFG
    std::unique_ptr<cfg_t> m_cfg;
    // generate unique identifiers for crab basic block ids
    unsigned int m_id;
    // map llvm CFG basic blocks to crab basic block ids
    node_to_crab_block_map_t m_node_to_crab_map;
    // map llvm CFG edges to crab basic block ids
    edge_to_crab_block_map_t m_edge_to_crab_map;
    // map Crab statement to its corresponding LLVM instruction
    // 
    // In most of the crab statements, their operands have back
    // pointers to their corresponding LLVM values. However, this is
    // not the case for array instructions. For those case, we keep
    // explicitly the reverse mapping.
    llvm::DenseMap<const statement_t*, const llvm::Instruction*> m_rev_map;
    // information about LLVM pointers
    const llvm::DataLayout* m_dl;
    const llvm::TargetLibraryInfo *m_tli;
    // cfg builder parameters
    const CrabBuilderParams& m_params;
    
    /// Helpers for build_cfg
    
    // Given a llvm basic block return its corresponding crab basic block    
    basic_block_t* lookup(const llvm::BasicBlock &bb) const;

    void add_block(const llvm::BasicBlock &bb);
    
    void add_edge(const llvm::BasicBlock &src, const llvm::BasicBlock &target);

    basic_block_t* exec_edge(const llvm::BasicBlock &src,
			     const llvm::BasicBlock &target); 

    void add_block_in_between(basic_block_t &src, basic_block_t &dst,
			      basic_block_t &between);
    
    basic_block_label_t make_crab_basic_block_label(const llvm::BasicBlock* bb);
    
    basic_block_label_t make_crab_basic_block_label(const llvm::BasicBlock* src,
						    const llvm::BasicBlock* dst);

  }; // end class CfgBuilder

  /**
   * A manager that keeps all the crab CFG builders.
   * A builder contains the crab CFG plus some extra information about
   * the translation.
   **/
  class CrabBuilderManager {
  public:
    using CfgBuilderPtr = std::shared_ptr<clam::CfgBuilder>;

    CrabBuilderManager(CrabBuilderParams params);
    
    ~CrabBuilderManager();
    
    CrabBuilderManager(const CrabBuilderManager& o) = delete;
    
    CrabBuilderManager& operator=(const CrabBuilderManager& o) = delete;
    
    bool has_cfg(const llvm::Function &f) const;
    
    void add(const llvm::Function &f, CfgBuilderPtr cfg_builder);
    
    cfg_t& get_cfg(const llvm::Function &f) const;
    
    const CfgBuilderPtr get_cfg_builder(const llvm::Function &f) const;
    
    variable_factory_t& get_var_factory();

    const CrabBuilderParams& get_cfg_builder_params() const;
    
  private:    
    // user-definable parameters for building the Crab CFGs
    CrabBuilderParams m_params;
    // map LLVM function to Crab CfgBuilder
    llvm::DenseMap<const llvm::Function*, CfgBuilderPtr> m_cfg_builder_map;
    // All CFGs supervised by this manager are created using the same
    // variable factory.
    variable_factory_t m_vfac;
  };
  
} // end namespace clam
