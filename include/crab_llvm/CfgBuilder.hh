#ifndef __CFG_BUILDER_HH_
#define __CFG_BUILDER_HH_

/* 
 * Translate a LLVM function to a CFG language understood by
 * crab.
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
#include "llvm/IR/DataLayout.h"

#include "crab_llvm/SymEval.hh"

#include "crab/cfg/Cfg.hpp"
#include "crab/cfg/VarFactory.hpp"
#include "crab/common/bignums.hpp"

namespace crab { namespace cfg { 
   namespace var_factory_impl {
      namespace indexed_string_impl {
         // To print variable names
         template<> inline std::string get_str(const llvm::Value *v) 
         {return v->getName().str();}
      } 
   }
}}

namespace crab {
  namespace cfg_impl {

     using namespace cfg;

     // To print basic block labels
     template<> inline std::string get_label_str(const llvm::BasicBlock *B) 
     { return B->getName (); }
  
     // Variable factory from llvm::Value's
     class LlvmVariableFactory : public boost::noncopyable  {
       typedef var_factory_impl::VariableFactory< const llvm::Value* > LlvmVariableFactory_t;
       std::unique_ptr< LlvmVariableFactory_t > m_factory; 
       
      public: 
       
       typedef LlvmVariableFactory_t::variable_t varname_t;
       typedef LlvmVariableFactory_t::const_var_range const_var_range;
       
       LlvmVariableFactory(): m_factory (new LlvmVariableFactory_t()){ }

       const_var_range get_shadow_vars () const {
         return m_factory->get_shadow_vars ();
       }
       
       // to generate fresh varname_t without having a Value
       varname_t get () { 
         return m_factory->get ();
       }
       
       // to generate varname_t without having a Value
       varname_t get (int k) {
         return m_factory->get (k);
       }
       
       varname_t operator[](const llvm::Value &v) {
         const llvm::Value *V = &v;
         return (*m_factory)[V];			      
       }
     }; 
  
     typedef LlvmVariableFactory VariableFactory;
     typedef typename VariableFactory::varname_t varname_t;
     // CFG
     typedef ikos::variable< ikos::z_number, varname_t > z_var;
     typedef const llvm::BasicBlock* basic_block_label_t;
     typedef Cfg<basic_block_label_t, varname_t> cfg_t;
     typedef Cfg_Ref<cfg_t> cfg_ref_t;
     typedef cfg_t::basic_block_t basic_block_t;
     typedef typename cfg_t::basic_block_t::z_lin_exp_t z_lin_exp_t;
     typedef typename cfg_t::basic_block_t::z_lin_cst_t z_lin_cst_t;
     typedef ikos::linear_constraint_system<ikos::z_number, varname_t> z_lin_cst_sys_t;

  } // end namespace cfg_impl
} // end namespace crab

namespace
{
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        const crab::cfg_impl::cfg_t& cfg) {
    std::ostringstream s;
    s << cfg;
    o << s.str ();
    return o;
  }

  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::cfg_impl::cfg_ref_t cfg) {
    std::ostringstream s;
    s << cfg;
    o << s.str ();
    return o;
  }

}

namespace crab_llvm
{
  using namespace std;
  using namespace crab::cfg_impl;
  using namespace llvm;

  class MemAnalysis;

  typedef SymEval<VariableFactory, z_lin_exp_t> sym_eval_t;

  class CfgBuilder: public boost::noncopyable {
   public:

    typedef boost::optional<basic_block_t&> opt_basic_block_t;
    typedef boost::shared_ptr<cfg_t> cfg_ptr_t;

   private:
    
    typedef boost::unordered_map< basic_block_label_t, 
                                  basic_block_t& > llvm_bb_map_t;

    bool m_is_cfg_built;
    Function& m_func;
    sym_eval_t m_sev;
    unsigned m_id;
    cfg_ptr_t m_cfg;
    llvm_bb_map_t m_bb_map;
    TrackedPrecision m_tracklev;
    bool m_is_inter_proc;
    const DataLayout* m_dl;
    // Placeholder blocks added *temporary* to the LLVM bitecode for
    // translating Branch instructions into Crab assume statements
    vector<llvm::BasicBlock*> m_fake_assume_blocks;

   public:
    
    CfgBuilder (Function& func, 
                VariableFactory& vfac, MemAnalysis& mem, 
                TrackedPrecision tracklev, bool isInterProc)
        : m_is_cfg_built (false),                          
          m_func (func), 
          m_sev (vfac, mem, tracklev),
          m_id (0),
          m_cfg (boost::make_shared<cfg_t>(&m_func.getEntryBlock (), tracklev)),
          m_tracklev (tracklev),
          m_is_inter_proc (isInterProc),
          m_dl (func.getParent ()->getDataLayout ()) { }
    
    ~CfgBuilder () { 
      for (llvm::BasicBlock* B: m_fake_assume_blocks) {
        delete B; // B->eraseFromParent ();
      }
    }

    cfg_ptr_t getCfg () { 
      if (!m_is_cfg_built) {
        build_cfg ();
        m_is_cfg_built = true;
      }
      return m_cfg;
    }

    // for ConcCrabLlvm
    sym_eval_t getSymEval () { return m_sev; }

   private:
    
    const llvm::BasicBlock* createFakeBlock (LLVMContext &ctx, 
                                             const Twine &name,
                                             Function *parent);

    string create_bb_name(string prefix = "") {
      if (prefix == "") prefix = string("_crab_bb_");
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
