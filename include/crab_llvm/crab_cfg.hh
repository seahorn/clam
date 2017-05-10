#ifndef __CRAB_CFG_LANGUAGE_HH_
#define __CRAB_CFG_LANGUAGE_HH_

/* 
 * Instantiation of the Crab CFG language with llvm::Value* as
 * variables and llvm::BasicBlock* as basic block labels.
 */


#include "llvm/IR/Value.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"

#include "crab/cfg/cfg.hpp"
#include "crab/cfg/var_factory.hpp"
#include "crab/common/bignums.hpp"

namespace crab {
  namespace cfg { 
    namespace var_factory_impl {
      namespace indexed_string_impl {
        // To print variable names
        template<> inline std::string get_str(const llvm::Value *v) 
        {return v->getName().str();}
      } 
    }
  }
  namespace cfg_impl {
     // To print basic block labels
     template<> inline std::string get_label_str(const llvm::BasicBlock *B) 
     { return B->getName (); }
  }
}

namespace crab_llvm {
  
     // Variable factory from llvm::Value's
     class LlvmVariableFactory : public boost::noncopyable  {
       typedef crab::cfg::var_factory_impl::variable_factory<const llvm::Value*> LlvmVariableFactory_t;
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
     typedef crab::cfg::Cfg<basic_block_label_t, varname_t> cfg_t;
     typedef boost::shared_ptr<cfg_t> cfg_ptr_t;
     typedef crab::cfg::cfg_ref<cfg_t> cfg_ref_t;
     typedef cfg_t::basic_block_t basic_block_t;
     typedef typename cfg_t::basic_block_t::z_lin_exp_t z_lin_exp_t;
     typedef typename cfg_t::basic_block_t::z_lin_cst_t z_lin_cst_t;
     typedef ikos::linear_constraint_system<ikos::z_number, varname_t> z_lin_cst_sys_t;

} // end namespace crab_llvm

namespace {
  inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o, 
				       const crab_llvm::cfg_t& cfg) {
    crab::crab_string_os s;
    s << cfg;
    o << s.str ();
    return o;
  }

  inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o, 
				       crab_llvm::cfg_ref_t cfg) {
    crab::crab_string_os s;
    s << cfg;
    o << s.str ();
    return o;
  }
}
#endif 
