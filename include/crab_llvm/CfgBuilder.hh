#ifndef __CFG_BUILDER_HH_
#define __CFG_BUILDER_HH_

/* 
 * Translate a LLVM function to a Crab CFG.
 *
 * WARNING: the translation is, in general, an abstraction of the
 * concrete semantics of the input program. 
 */

#include <boost/optional.hpp>
#include <boost/noncopyable.hpp>

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"

#include "crab_llvm/SymEval.hh"
#include "crab_llvm/crab_cfg.hh"

// forward declaration
namespace llvm {
  class DataLayout;
  class TargetLibraryInfo;
}

namespace crab_llvm {
  
  class MemAnalysis;

  typedef SymEval<VariableFactory, z_lin_exp_t> sym_eval_t;

  class CfgBuilder: public boost::noncopyable {
   public:

    typedef boost::optional<basic_block_t&> opt_basic_block_t;

   private:
    
    typedef boost::unordered_map<basic_block_label_t, 
				 basic_block_t&> llvm_bb_map_t;

    bool m_is_cfg_built;
    Function& m_func;
    sym_eval_t m_sev;
    unsigned m_id;
    cfg_ptr_t m_cfg;
    llvm_bb_map_t m_bb_map;
    tracked_precision m_tracklev;
    bool m_is_inter_proc;
    const DataLayout* m_dl;
    const TargetLibraryInfo* m_tli;
    // Placeholder blocks added *temporary* to the LLVM bitecode for
    // translating Branch instructions into Crab assume statements
    vector<llvm::BasicBlock*> m_fake_assume_blocks;

   public:
    
    CfgBuilder (Function& func, 
                VariableFactory& vfac, MemAnalysis& mem, 
                tracked_precision tracklev, bool isInterProc,
                const TargetLibraryInfo* tli);
    
    ~CfgBuilder ();

    cfg_ptr_t getCfg ();
    
    // for ConcCrabLlvm
    sym_eval_t getSymEval () { return m_sev; }

   private:
    
    const llvm::BasicBlock* createFakeBlock (LLVMContext &ctx, 
                                             const Twine &name,
                                             Function *parent);

    string create_bb_name(string prefix = "") {
      if (prefix == "") prefix = string("__@bb_");
      ++m_id;
      string id_str = std::to_string(m_id);
      return prefix + id_str;
    }

    void build_cfg();

    opt_basic_block_t lookup(const llvm::BasicBlock &);
    
    void add_block(llvm::BasicBlock &BB);

    void add_edge(llvm::BasicBlock &Src, const llvm::BasicBlock &Target);

    opt_basic_block_t execBr(llvm::BasicBlock &Src, 
                             const llvm::BasicBlock &Target); 

    basic_block_t& add_block_in_between (basic_block_t &src, 
                                         basic_block_t &dst, 
                                         const llvm::BasicBlock* B) ;

  }; // end class CfgBuilder

} // end namespace crab_llvm

#endif 
