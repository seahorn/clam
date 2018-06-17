#ifndef __CFG_BUILDER_HH_
#define __CFG_BUILDER_HH_

/* 
 * Translate a LLVM function to a Crab control-flow graph.
 */

#include <boost/optional.hpp>
#include <boost/noncopyable.hpp>
#include "crab_llvm/crab_cfg.hh"

// forward declarations
namespace llvm {
  class DataLayout;
  class TargetLibraryInfo;
  class BasicBlock;
  class Function;
  class Twine;
}

namespace crab_llvm {
  class HeapAbstraction;
}

namespace crab_llvm {

  // forward declarations
  class crabLit;
  class crabLitFactoryImpl;
  
  typedef boost::shared_ptr<crabLit> crab_lit_ref_t;
  
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
  class CfgBuilder: public boost::noncopyable {
   public:
    
    CfgBuilder(llvm::Function& func, 
	       llvm_variable_factory &vfac, HeapAbstraction &mem, 
	       crab::cfg::tracked_precision tracklev, bool isInterProc,
	       const llvm::TargetLibraryInfo *tli);
    
    ~CfgBuilder();

    // The caller owns the pointer and it will be in charge of freeing it.
    cfg_t* get_cfg();

    // expose internal details
    typedef boost::unordered_map<std::pair<const llvm::BasicBlock*,
					   const llvm::BasicBlock*>,
			       basic_block_label_t, pair_hash> edge_to_bb_map_t;

    // expose internal details.
    // return by value because it is intended to survive CfgBuilder.
    edge_to_bb_map_t getEdgeToBBMap() const
    { return m_edge_bb_map; }
    
   private:

    typedef boost::unordered_map<basic_block_label_t, 
				 basic_block_t&> llvm_bb_map_t;

    typedef boost::optional<basic_block_t&> opt_basic_block_t;
    
    bool m_is_cfg_built;
    llvm::Function &m_func;
    crabLitFactory m_lfac; 
    HeapAbstraction &m_mem;
    unsigned m_id;
    cfg_t *m_cfg;
    // map llvm basic blocks to crab basic blocks
    llvm_bb_map_t m_bb_map;
    // map edge to crab basic block
    edge_to_bb_map_t m_edge_bb_map;
    bool m_is_inter_proc;
    const llvm::DataLayout* m_dl;
    const llvm::TargetLibraryInfo *m_tli;
    
    void build_cfg();

    void add_block(llvm::BasicBlock &BB);

    opt_basic_block_t lookup(const llvm::BasicBlock &);
    
    void add_edge(llvm::BasicBlock &Src, const llvm::BasicBlock &Target);

    opt_basic_block_t exec_br(llvm::BasicBlock &Src,
			      const llvm::BasicBlock &Target); 

    void add_block_in_between(basic_block_t &src, basic_block_t &dst,
			      basic_block_t &between);

  }; // end class CfgBuilder

} // end namespace crab_llvm

#endif 
