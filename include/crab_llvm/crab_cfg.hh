#ifndef __CRAB_CFG_LANGUAGE_HH_
#define __CRAB_CFG_LANGUAGE_HH_

/* 
 * Definition of the Crab CFG language with llvm::Value* as variables
 * and llvm::BasicBlock* as basic block labels.
 */


#include "llvm/IR/Value.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"

#include "crab/cfg/cfg.hpp"
#include "crab/cfg/var_factory.hpp"
#include "crab/common/bignums.hpp"

#include <boost/functional/hash.hpp>

namespace crab_llvm {

  // This wrapper is needed because we can have crab blocks which do
  // not correspond to llvm blocks.
  class llvm_basic_block_wrapper {
    
  public:

    // the new block represents that the control is at b
    llvm_basic_block_wrapper(const llvm::BasicBlock *b)
      : m_bb(b), m_edge(nullptr, nullptr), m_name(b->getName()) {
      assert(b->hasName());
    }

    // the new block represents that the control goes from src to dst
    llvm_basic_block_wrapper(const llvm::BasicBlock *src, const llvm::BasicBlock *dst, std::string name)
      : m_bb(nullptr), m_edge(src, dst), m_name(name){}

    llvm_basic_block_wrapper()
      : m_bb(nullptr), m_edge(nullptr, nullptr), m_name("") {}

    std::string get_name() const { return m_name; }

    bool is_edge() const {
      return !m_bb && (m_edge.first && m_edge.second);
    }
    
    const llvm::BasicBlock* get_basic_block() const {
      return m_bb;
    }
    
    const std::pair<const llvm::BasicBlock*, const llvm::BasicBlock*>& get_edge() const {
      return m_edge;
    }

    bool operator==(const llvm_basic_block_wrapper &other) const
    { return m_name == other.m_name; }
    
    bool operator!=(const llvm_basic_block_wrapper &other) const
    { return !(this->operator==(other)); }
    
    bool operator<(const llvm_basic_block_wrapper &other) const
    { return m_name < other.m_name; }

    std::size_t index() const {
      boost::hash<std::string> hasher;
      return hasher(m_name);
    }

  private:
    
    const llvm::BasicBlock *m_bb;
    std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *> m_edge;
    std::string m_name;
  };
  
  inline llvm::raw_ostream& operator<<(llvm::raw_ostream &o,
				       const llvm_basic_block_wrapper& b) {
    o << b.get_name();
    return o;
  }

  inline crab::crab_os& operator<<(crab::crab_os &o,
				   const llvm_basic_block_wrapper& b) {
    o << b.get_name();
    return o;
  }
  
  inline std::size_t hash_value (const llvm_basic_block_wrapper &b)
  { return b.index(); }
}

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
     template<> inline std::string get_label_str(crab_llvm::llvm_basic_block_wrapper b) 
     { return b.get_name (); }
    
  }
}

namespace crab_llvm {
       
     // Variable factory from llvm::Value's
     class llvm_variable_factory :
      public crab::cfg::var_factory_impl::
             variable_factory<const llvm::Value*> {
       typedef crab::cfg::var_factory_impl::
               variable_factory<const llvm::Value*> variable_factory_t;
      public: 
       
       typedef variable_factory_t::varname_t varname_t;
       typedef variable_factory_t::const_var_range const_var_range;
       
       llvm_variable_factory(): variable_factory_t() {}
     };
  
     typedef llvm_variable_factory variable_factory_t;
     typedef typename variable_factory_t::varname_t varname_t;
  
     // CFG over integers
     typedef ikos::z_number number_t;
     typedef ikos::variable<number_t, varname_t> var_t;
     typedef ikos::variable_ref<number_t, varname_t> var_ref_t;  
     typedef llvm_basic_block_wrapper basic_block_label_t;
     typedef crab::cfg::cfg<basic_block_label_t,varname_t,number_t> cfg_t;
     typedef boost::shared_ptr<cfg_t> cfg_ptr_t;
     typedef crab::cfg::cfg_ref<cfg_t> cfg_ref_t;
     typedef cfg_t::basic_block_t basic_block_t;
     typedef typename cfg_t::basic_block_t::lin_exp_t lin_exp_t;
     typedef typename cfg_t::basic_block_t::lin_cst_t lin_cst_t;
     typedef ikos::linear_constraint_system<number_t, varname_t> lin_cst_sys_t;
     typedef ikos::disjunctive_linear_constraint_system<number_t, varname_t> disj_lin_cst_sys_t;
     typedef crab::pointer_constraint<var_t> ptr_cst_t;

} // end namespace crab_llvm

namespace crab {
  namespace cfg {
    // Convenient wrapper to relate crab statement with its parent
    // FIXME: crab should provide this.
    class statement_wrapper {
    public:  
      typedef typename crab_llvm::cfg_ref_t::statement_t statement_t;
      typedef typename crab_llvm::cfg_ref_t::basic_block_label_t basic_block_label_t;  
      
      statement_wrapper(statement_t* s, basic_block_label_t bb)
	: m_s(s), m_parent(bb) {}
      
      friend crab::crab_os& operator<<(crab::crab_os& o, statement_wrapper& s){
      	o << *(s.m_s);
      	return o;
      }
      
      statement_t* m_s;
      basic_block_label_t m_parent;
    };
  }
}

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
