#ifndef __LLVM_IKOS_HPP_
#define __LLVM_IKOS_HPP_

/* 
 * Infer invariants using Ikos.
 */

#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"

#include "boost/optional.hpp"

#include "ikos/CfgBuilder.hh"

namespace llvm_ikos
{enum IkosDomain { INTERVALS, CONGRUENCES, INTERVALS_CONGRUENCES, ZONES, OCTAGONS};}

namespace llvm_ikos
{

  using namespace llvm;
  using namespace cfg_impl;

  class LlvmIkos : public llvm::ModulePass
  {
    typedef llvm::DenseMap< const llvm::BasicBlock *, ZLinearConstraintSystem > invariants_map_t;

    invariants_map_t m_inv_map;
    IkosDomain       m_absdom;
    bool             m_runlive;

   public:

    typedef invariants_map_t::iterator       iterator;
    typedef invariants_map_t::const_iterator const_iterator;

    static char ID;        
    
    LlvmIkos (IkosDomain absdom = INTERVALS, bool runLive = false):
        llvm::ModulePass (ID), m_absdom (absdom), m_runlive(runLive)  
    { }

    ~LlvmIkos ()
    { m_inv_map.clear(); }

    virtual void getAnalysisUsage (llvm::AnalysisUsage &AU) const 
    {AU.setPreservesAll ();}

    virtual void releaseMemory () {m_inv_map.clear ();}
    
    virtual bool runOnModule (llvm::Module& M);

    virtual bool runOnFunction (llvm::Function &F);

    iterator       begin ()       { return m_inv_map.begin(); } 
    iterator       end ()         { return m_inv_map.end();   }
    const_iterator begin () const { return m_inv_map.begin(); }
    const_iterator end ()   const { return m_inv_map.end();   }

    ZLinearConstraintSystem operator[] (const llvm::BasicBlock *BB) const
    {
      const_iterator it = m_inv_map.find (BB);
      assert (it != m_inv_map.end ());
      return it->second;
    }

    void dump (llvm::Module &M) const;

   private:

    ZLinearConstraint mkTRUE() const 
    { return ZLinearConstraint ( ZLinearExpression (1) == ZLinearExpression (1)); }

    ZLinearConstraint mkFALSE() const 
    { return ZLinearConstraint ( ZLinearExpression (1) == ZLinearExpression (0)); }

    template<typename AbsDomain> 
    bool runOnCfg (cfg_t& cfg, llvm::Function &F, VariableFactory &vfac);

  };

} // end namespace llvm_ikos

#endif
