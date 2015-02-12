#ifndef __LLVM_IKOS_HPP_
#define __LLVM_IKOS_HPP_

/* 
 * Compute invariants for each basic block using a numerical abstract
 * domain.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"
#include "boost/optional.hpp"
#include "include/CfgBuilder.hh"

namespace llvm_ikos
{

  using namespace llvm;
  using namespace cfg_impl;

  class LlvmIkos : public llvm::ModulePass
  {
    typedef llvm::DenseMap< const llvm::BasicBlock *, ZLinearConstraintSystem > invariants_map_t;

    bool             m_is_enabled;
    invariants_map_t m_inv_map;

    ZLinearConstraint mkTRUE() const 
    {
      return ZLinearConstraint ( ZLinearExpression (1) == ZLinearExpression (1));
    }

    ZLinearConstraint mkFALSE() const 
    {
      return ZLinearConstraint ( ZLinearExpression (1) == ZLinearExpression (0));
    }

   public:

    typedef invariants_map_t::iterator       iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    LlvmIkos();

    ~LlvmIkos (){ m_inv_map.clear(); }

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU){ }

    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    iterator       begin ()       { return m_inv_map.begin(); } 
    iterator       end ()         { return m_inv_map.end();   }
    const_iterator begin () const { return m_inv_map.begin(); }
    const_iterator end ()   const { return m_inv_map.end();   }

    ZLinearConstraintSystem operator[] (const llvm::BasicBlock *BB) const
    {
      const_iterator it = m_inv_map.find (BB);
      if (it == m_inv_map.end())
      { 
        ZLinearConstraintSystem tt;
        tt += mkTRUE ();
        return tt;
      }
      else return it->second;        
    }

    bool IsEnabled() const 
    { return m_is_enabled; }

    void dump (llvm::Module &M) const
    {
      for (auto &F : M)
      {
        if (F.isDeclaration () || F.empty ()) continue;

        errs () << "\nFunction " << F.getName () << "\n";
        for (auto &B : F)
        {
          const llvm::BasicBlock * BB = &B;
          errs () << "\t" << BB->getName () << ": ";
          auto inv = this->operator[] (BB);
          std::cout  << inv << "\n";
        }
      }
    }

   private:

    template<typename AbsDomain> 
    bool runOnCfg (cfg_t& cfg, llvm::Function &F, VariableFactory &vfac);

  };
}

#endif
