#ifndef __CFG_BUILDER_HH_
#define __CFG_BUILDER_HH_

/* 
 * Translate a LLVM function to a CFG language understood by
 * crab.
 *
 * WARNING: the translation is, in general, an abstraction of the
 * concrete semantics of the input program. This is a key feature to
 * make more scalable analyses.
 *
 * There are different ways of abstraction during the translation. One
 * example is that users can choose reasoning between llvm registers,
 * pointers or memory contents. This means of course that the user
 * needs to know then which analyses will run so that he/she can
 * ensure that the CFG language contains all the details needed by the
 * analyses but hopefully not more. Another example of abstraction is
 * to ignore certain instructions (e.g., floating point operations)
 * for which user may know that his/her abstract domain cannot reason
 * about them.
 */

#include <boost/optional.hpp>
#include <boost/noncopyable.hpp>

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/DataLayout.h"

#include "crab_llvm/MemAnalysis.hh"

#include <crab/cfg/Cfg.hpp>
#include <crab/cfg/VarFactory.hpp>
#include <crab/common/bignums.hpp>

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
     template<> inline std::string get_label_str(llvm::BasicBlock *B) 
     { return B->getName (); }
  
     // Variable factory from llvm::Value's
     class LlvmVariableFactory : public boost::noncopyable  
     {
       typedef var_factory_impl::VariableFactory< const llvm::Value* > LlvmVariableFactory_t;
       std::unique_ptr< LlvmVariableFactory_t > m_factory; 
       
      public: 
       
       typedef LlvmVariableFactory_t::variable_t varname_t;
       typedef LlvmVariableFactory_t::const_var_range const_var_range;
       
       LlvmVariableFactory(): m_factory (new LlvmVariableFactory_t()){ }

       const_var_range get_shadow_vars () const 
       {
         return m_factory->get_shadow_vars ();
       }
       
       // to generate fresh varname_t without having a Value
       varname_t get ()  
       {
         return m_factory->get ();
       }
       
       // to generate varname_t without having a Value
       varname_t get (int k)  
       {
         return m_factory->get (k);
       }
       
       varname_t operator[](const llvm::Value &v)
       {
         const llvm::Value *V = &v;
         return (*m_factory)[V];			      
       }
     }; 
  
     typedef LlvmVariableFactory VariableFactory;
     typedef typename VariableFactory::varname_t varname_t;
     // CFG
     typedef ikos::variable< ikos::z_number, varname_t > z_var;
     typedef llvm::BasicBlock* basic_block_label_t;
     typedef Cfg< basic_block_label_t, varname_t> cfg_t;
     typedef cfg_t::basic_block_t basic_block_t;
     typedef typename cfg_t::basic_block_t::z_lin_exp_t z_lin_exp_t;
     typedef typename cfg_t::basic_block_t::z_lin_cst_t z_lin_cst_t;
     typedef ikos::linear_constraint_system<ikos::z_number, varname_t> z_lin_cst_sys_t;
  } // end namespace cfg_impl
} // end namespace crab

namespace
{
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, crab::cfg_impl::cfg_t cfg)
  {
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

  class CfgBuilder: public boost::noncopyable
  {
    
   public:

    typedef boost::optional<basic_block_t&> opt_basic_block_t;

   private:
    
    typedef boost::unordered_map< basic_block_label_t, 
                                  basic_block_t& > llvm_bb_map_t;
    Function&         m_func;
    VariableFactory&  m_vfac;
    unsigned          m_id;
    cfg_t             m_cfg;
    llvm_bb_map_t     m_bb_map;
    MemAnalysis*      m_mem;
    bool              m_is_inter_proc;
    const DataLayout* m_dl;
    vector<llvm::BasicBlock*> m_extra_blks;

   public:
    
    CfgBuilder (Function &func, VariableFactory &vfac, MemAnalysis* mem, 
                bool isInterProc);
        
    ~CfgBuilder ();

    cfg_t & makeCfg () { 
      build_cfg ();
      return m_cfg; 
    }

   private:
    
    string create_bb_name(string prefix = "")
    {
      if (prefix == "") prefix = string("_bb_");
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
                                         basic_block_label_t bb_id) ;

  }; // end class CfgBuilder

} // end namespace crab_llvm

#endif 
