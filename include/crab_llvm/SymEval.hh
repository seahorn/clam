#ifndef __SYM_EVAL_HH__
#define __SYM_EVAL_HH__

#include "llvm/IR/Instruction.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/APInt.h"

#include <crab/cfg/Cfg.hpp>

namespace crab_llvm
{

  using namespace llvm;
  using namespace std;
  using namespace crab::cfg;

  //! Map llvm values to integer linear expressions
  template<typename VariableFactory, typename ZLinExp>
  struct SymEval {

    VariableFactory& m_vfac;
    TrackedPrecision m_track_lvl;

    typedef typename VariableFactory::varname_t varname_t;

    SymEval (VariableFactory &vfac, TrackedPrecision track_lvl): 
        m_vfac (vfac), m_track_lvl (track_lvl)
    { }

    bool isTracked (const llvm::Value &v) {
      
      // -- ignore any shadow variable created by seahorn
      if (v.getName().startswith ("shadow.mem")) 
        return false;

      // -- a pointer
      if (v.getType ()->isPointerTy ()) 
        return (m_track_lvl >= PTR); 

      // -- always track integer registers
      return v.getType ()->isIntegerTy ();
    }

    varname_t symVar (const Value &v) {
      assert (isTracked (v));
      return m_vfac [v];
    }

    varname_t symVar (int v) {
      return m_vfac.get (v);
    }
  
    bool isVar (ZLinExp e) {
      auto v = e.get_variable ();
      bool res = (v ? true: false);
      return res;
    }

    boost::optional<ZLinExp> lookup (const Value &v) {

      if (isa<const UndefValue> (&v))
        return boost::optional<ZLinExp> ();

      if (isa<ConstantPointerNull> (&v))
        return boost::optional<ZLinExp> (0);
      
      if (const ConstantInt *c = dyn_cast<const ConstantInt> (&v)) {
        if (c->getType ()->isIntegerTy(1)) {
          return ZLinExp (c->getZExtValue ());
        }
        else if (c->getValue ().getMinSignedBits () <= 64) {
          return ZLinExp (c->getSExtValue ());
        }
        else {
          errs () << "Warning: " 
                  <<  toMpz (c->getValue ()).get_str ()  
                  << " does not fit in int64_t.\n";
        }
      }

      if (isTracked(v) && !isa<ConstantExpr> (v))
        return ZLinExp (symVar (v));
      
      return boost::optional<ZLinExp>();
    }
        
    /** Converts v to mpz_class. Assumes that v is signed */
    inline mpz_class toMpz (const APInt &v)
    {
      // Based on:
      // https://llvm.org/svn/llvm-project/polly/trunk/lib/Support/GICHelper.cpp
      // return v.getSExtValue ();
      
      APInt abs;
      abs = v.isNegative () ? v.abs () : v;
      
      const uint64_t *rawdata = abs.getRawData ();
      unsigned numWords = abs.getNumWords ();
      
      // TODO: Check if this is true for all platforms.
      mpz_class res;
      mpz_import(res.get_mpz_t (), numWords, 1, 
                 sizeof (uint64_t), 0, 0, rawdata);
    
      return v.isNegative () ? mpz_class(-res) : res;
    }
  };
}

#endif
