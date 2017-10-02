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
  
  class CfgBuilder: public boost::noncopyable {
   public:

    typedef boost::optional<basic_block_t&> opt_basic_block_t;

   private:
    
    typedef boost::unordered_map<basic_block_label_t, 
				 basic_block_t&> llvm_bb_map_t;

    bool m_is_cfg_built;
    llvm::Function &m_func;
    llvm_variable_factory &m_vfac;
    HeapAbstraction &m_mem;
    crab::cfg::tracked_precision m_tracklev;
    unsigned m_id;
    cfg_ptr_t m_cfg;
    llvm_bb_map_t m_bb_map;
    bool m_is_inter_proc;
    const llvm::DataLayout* m_dl;
    const llvm::TargetLibraryInfo *m_tli;

   public:
    
    CfgBuilder(llvm::Function& func, 
	       llvm_variable_factory &vfac,
	       HeapAbstraction &mem, 
	       crab::cfg::tracked_precision tracklev,
	       bool isInterProc,
	       const llvm::TargetLibraryInfo *tli);
    
    ~CfgBuilder();

    cfg_ptr_t getCfg();
    
   private:
    
    void build_cfg();

    opt_basic_block_t lookup(const llvm::BasicBlock &);
    
    void add_block(llvm::BasicBlock &BB);

    void add_edge(llvm::BasicBlock &Src, const llvm::BasicBlock &Target);

    opt_basic_block_t execBr(llvm::BasicBlock &Src, const llvm::BasicBlock &Target); 

    void add_block_in_between (basic_block_t &src, basic_block_t &dst,  
			       basic_block_t &between);

  }; // end class CfgBuilder

} // end namespace crab_llvm

#endif 
