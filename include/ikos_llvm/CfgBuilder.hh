#ifndef __CFG_BUILDER_HH_
#define __CFG_BUILDER_HH_

/* Build a CFG from a LLVM function */

#include <boost/optional.hpp>
#include <boost/noncopyable.hpp>

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"

//#include "avy/AvyDebug.h"

#include <ikos/cfg/Cfg.hpp>
#include <ikos/cfg/VarFactory.hpp>
#include <ikos/bignums.hpp>

#include "ikos_llvm/MemAnalysis.hh"

namespace cfg
{
  namespace var_factory_impl
  {
    namespace indexed_string_impl 
    {
      // To print variable names
      template<> inline std::string get_str(const llvm::Value *v) 
      {return v->getName().str();}
    } 
  }
}

namespace cfg_impl
{
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
  typedef cfg_t::BasicBlock_t basic_block_t;
  typedef typename cfg_t::BasicBlock_t::ZLinearExpression ZLinExp;
  typedef typename cfg_t::BasicBlock_t::ZLinearConstraint ZLinCst;
  typedef ikos::linear_constraint_system<ikos::z_number, varname_t> ZLinCstSystem;
  // for compatibility with seahorn
  typedef ZLinCst ZLinearConstraint;
  typedef ZLinCstSystem ZLinearConstraintSystem;
} // end namespace cfg_impl

namespace{
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, cfg_impl::cfg_t cfg)
  {
    std::ostringstream s;
    s << cfg;
    o << s.str ();
    return o;
  }
}

namespace llvm_ikos
{
  using namespace std;
  using namespace cfg_impl;
  using namespace llvm;

  class CfgBuilder: public boost::noncopyable
  {
    
    friend class SymExecSelectVisitor;

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

   public:
    
    CfgBuilder (Function &func, VariableFactory &vfac, MemAnalysis* mem, 
                bool isInterProc);
    
    cfg_t & operator()()
    { 
      make_cfg ();
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

    void make_cfg();

    opt_basic_block_t lookup(const llvm::BasicBlock &);
    
    void add_block(llvm::BasicBlock &BB);

    void add_edge(llvm::BasicBlock &Src, const llvm::BasicBlock &Target);

    opt_basic_block_t execBr(llvm::BasicBlock &Src, 
                             const llvm::BasicBlock &Target); 

    basic_block_t& add_block_in_between (basic_block_t &src, 
                                         basic_block_t &dst, 
                                         basic_block_label_t bb_id) ;

  }; // end class CfgBuilder

} // end namespace llvm_ikos

#endif 
