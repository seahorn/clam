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
}

namespace clam {
  class HeapAbstraction;
}

namespace clam {

  // forward declarations
  class crabLit;
  class crabLitFactoryImpl;
  
  typedef std::shared_ptr<crabLit> crab_lit_ref_t;
  
  /** 
      Factory to create crab literals: typed variable or number.
  **/
  class crabLitFactory {
  public:
    
    crabLitFactory(llvm_variable_factory &vfac, crab::cfg::tracked_precision tracklev);

    ~crabLitFactory();
    
    llvm_variable_factory& get_vfac();
    
    crab::cfg::tracked_precision get_track() const;

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
    
    CfgBuilder(llvm::Function& func, 
	       llvm_variable_factory &vfac, HeapAbstraction &mem, 
	       crab::cfg::tracked_precision tracklev, 
	       const llvm::TargetLibraryInfo *tli,
	       bool isInterProc = true);
    
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
    // information about LLVM pointers
    const llvm::DataLayout* m_dl;
    const llvm::TargetLibraryInfo *m_tli;
    // whether the translation is inter-procedural
    bool m_is_inter_proc;    

    /// Helpers for build_cfg
    
    // Given a llvm basic block return its corresponding crab basic block    
    basic_block_t* lookup(const llvm::BasicBlock &bb) const;

    void add_block(llvm::BasicBlock &bb);
    
    void add_edge(llvm::BasicBlock &src, const llvm::BasicBlock &target);

    basic_block_t* exec_edge(llvm::BasicBlock &src,
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
    
    CrabBuilderManager();
    
    ~CrabBuilderManager();
    
    CrabBuilderManager(const CrabBuilderManager& o) = delete;
    
    CrabBuilderManager& operator=(const CrabBuilderManager& o) = delete;
    
    bool has_cfg(const llvm::Function &f) const;
    
    void add(const llvm::Function &f, CfgBuilderPtr cfg_builder);
    
    cfg_t& get_cfg(const llvm::Function &f) const;
    
    const CfgBuilderPtr get_cfg_builder(const llvm::Function &f) const;    
    
  private:
    llvm::DenseMap<const llvm::Function*, CfgBuilderPtr> m_cfg_builder_map;
  };
  
} // end namespace clam
