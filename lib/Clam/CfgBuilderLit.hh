#pragma once

/* A wrapper object for a LLVM variable or constant */

#include "llvm/ADT/Optional.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

#include "clam/CfgBuilderParams.hh"
#include "clam/HeapAbstraction.hh"
#include "clam/crab/crab_lang.hh"

#include <unordered_map>

namespace clam {

// Convenient wrapper for a LLVM variable or constant
class crabLit {
public:
  enum lit_class_t {
    CRAB_LITERAL_BOOL,
    CRAB_LITERAL_INT,
    CRAB_LITERAL_REF,
  };

  crabLit(lit_class_t lit_class) : m_lit_class(lit_class) {}

  virtual ~crabLit() {}

  bool isBool() const { return m_lit_class == CRAB_LITERAL_BOOL; }

  bool isInt() const { return m_lit_class == CRAB_LITERAL_INT; }

  bool isRef() const { return m_lit_class == CRAB_LITERAL_REF; }

  virtual bool isVar() const = 0;

  virtual var_t getVar() const = 0;

  virtual void write(crab::crab_os &out) const = 0;

private:
  lit_class_t m_lit_class;
};

inline crab::crab_os &operator<<(crab::crab_os &out, const crabLit &l) {
  l.write(out);
  return out;
}

/** A Boolean literal is either a variable or constants true and false. **/
class crabBoolLit : public crabLit {
  friend class crabLitFactoryImpl;

  bool m_cst; // only considered if !m_var.hasValue()
  llvm::Optional<var_t> m_var;

  crabBoolLit(bool cst) : crabLit(CRAB_LITERAL_BOOL), m_cst(cst) {}

  crabBoolLit(var_t var) : crabLit(CRAB_LITERAL_BOOL), m_cst(false), m_var(var) {}

public:
  bool isVar() const override { return (m_var.hasValue()); }

  var_t getVar() const override {
    assert(isVar());
    return m_var.getValue();
  }

  bool isConst() const { return !isVar(); }

  bool isTrue() const {
    if (!isConst())
      return false;
    return m_cst;
  }

  bool isFalse() const {
    if (!isConst())
      return false;
    return !m_cst;
  }

  var_or_cst_t getTypedConst() const {
    assert(isConst());

    if (isTrue()) {
      return var_or_cst_t::make_bool_true();
    } else {
      return var_or_cst_t::make_bool_false();
    }
  }

  void write(crab::crab_os &out) const override {
    if (isVar()) {
      out << getVar();
    } else if (isTrue()) {
      out << "true";
    } else {
      out << "false";
    }
  }
};

/** A reference literal is either a variable or constant null.**/
class crabRefLit : public crabLit {
  friend class crabLitFactoryImpl;

  // if !m_lit.hasValue() then the literal represents null
  llvm::Optional<var_t> m_lit;

  crabRefLit() : crabLit(CRAB_LITERAL_REF) {} // null
  crabRefLit(var_t v) : crabLit(CRAB_LITERAL_REF), m_lit(v) {}

public:
  bool isVar() const override { return m_lit.hasValue(); }

  var_t getVar() const override {
    assert(isVar());
    return m_lit.getValue();
  }

  bool isNull() const { return !isVar(); }

  var_or_cst_t getTypedConst() const {
    assert(isNull());
    return var_or_cst_t::make_reference_null();
  }

  void write(crab::crab_os &out) const override {
    if (isVar()) {
      out << getVar();
    } else {
      out << "NULL";
    }
  }
};

/** A numerical literal is either a variable or constant number.**/
class crabIntLit : public crabLit {
  friend class crabLitFactoryImpl;

  number_t m_num; // only considered if !m_var.hasValue()
  llvm::Optional<var_t> m_var;
  unsigned m_bitwidth;

  // If z_number != number_t we assume that number_t has a
  // constructor for z_number.
  explicit crabIntLit(ikos::z_number n, unsigned bitwidth)
      : crabLit(CRAB_LITERAL_INT), m_num(n), m_bitwidth(bitwidth) {}

  explicit crabIntLit(var_t v)
      : crabLit(CRAB_LITERAL_INT), m_var(v),
        m_bitwidth(v.get_type().get_integer_bitwidth()) {}

public:
  bool isVar() const override { return m_var.hasValue(); }

  var_t getVar() const override {
    assert(isVar());
    return m_var.getValue();
  }

  bool isInt() const { return !isVar(); }

  var_or_cst_t getTypedConst() const {
    assert(isInt());
    return var_or_cst_t(getInt(),
                        crab::variable_type(crab::INT_TYPE, getBitwidth()));
  }

  unsigned getBitwidth() const { return m_bitwidth; }

  number_t getInt() const {
    assert(isInt());
    return m_num;
  }

  lin_exp_t getExp() const {
    if (isInt())
      return lin_exp_t(getInt());
    else {
      assert(isVar());
      return lin_exp_t(getVar());
    }
  }

  void write(crab::crab_os &out) const override {
    if (isVar()) {
      out << getVar();
    } else {
      out << getInt();
    }
  }
};

typedef std::shared_ptr<crabLit> crab_lit_ref_t;

class crabLitFactoryImpl;

/**
 *  Factory to create crab literals: typed variable or number.  This
 *  factory also maps Heap Analysis' regions to crab typed variables.
 **/
class crabLitFactory {
public:
  crabLitFactory(variable_factory_t &vfac, const CrabBuilderParams &params);

  ~crabLitFactory();

  variable_factory_t &getVFac();

  CrabBuilderPrecision getTrack() const;

  const CrabBuilderParams &getCfgBuilderParams() const;

  /** convert a Value to a crabLit. By default all integer constants
      are interpreted as signed unless stated otherwise. **/
  crab_lit_ref_t getLit(const llvm::Value &v, bool IntCstAsSigned = true);

  /** make fresh typed variables.
   ** Each call returns a new fresh variable
   **/
  var_t mkIntVar(unsigned bitwidth);
  var_t mkBoolVar();
  var_t mkRefVar();
  llvm::Optional<var_t> mkVar(const llvm::Value &v);
  var_t mkArrayVar(RegionInfo rgnInfo);
  var_t mkRegionVar(RegionInfo rgnInfo);

  /** This is the inverse of mkVar.
   **  Return null if no Value associated with v 
   ***/
  const llvm::Value* getLLVMVar(const var_t &v) const;
  
  /** make typed variables associated with regions.
   ** Multiple calls with same parameters return the same variable.
   **/
  var_t mkArrayVar(Region rgn);
  var_t mkRegionVar(Region rgn);
  var_t mkScalarVar(Region rgn);

  /** direct accessors to crabLit subclasses **/
  bool isBoolTrue(const crab_lit_ref_t ref) const;
  bool isBoolFalse(const crab_lit_ref_t ref) const;
  bool isRefNull(const crab_lit_ref_t ref) const;
  /** error if the literal is not an integer **/
  lin_exp_t getExp(const crab_lit_ref_t ref) const;
  /** error if the literal is not a constant **/
  var_or_cst_t getTypedConst(const crab_lit_ref_t ref) const;
  /** error if the literal is not an integer constant **/
  number_t getIntCst(const crab_lit_ref_t ref) const;

private:
  crabLitFactoryImpl *m_impl;
};

} // end namespace clam
