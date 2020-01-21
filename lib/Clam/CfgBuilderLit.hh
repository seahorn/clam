#pragma once

/* A wrapper object for a LLVM variable or constant */

#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

#include "clam/CfgBuilderParams.hh"
#include "clam/crab/crab_cfg.hh"
#include "clam/HeapAbstraction.hh"

#include <unordered_map>

namespace clam {

// Convenient wrapper for a LLVM variable or constant
class crabLit {
public:
  enum lit_class_t {
    CRAB_LITERAL_BOOL,
    CRAB_LITERAL_INT,
    CRAB_LITERAL_PTR,
  };

  crabLit(lit_class_t lit_class) : m_lit_class(lit_class) {}

  virtual ~crabLit() {}

  bool isBool() const { return m_lit_class == CRAB_LITERAL_BOOL; }

  bool isInt() const { return m_lit_class == CRAB_LITERAL_INT; }

  bool isPtr() const { return m_lit_class == CRAB_LITERAL_PTR; }

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

  bool m_cst; // only considered if m_var.is_null()
  var_ref_t m_var;

  crabBoolLit(bool cst) : crabLit(CRAB_LITERAL_BOOL), m_cst(cst) {}

  crabBoolLit(var_t var) : crabLit(CRAB_LITERAL_BOOL), m_var(var) {}

public:
  bool isVar() const override { return (!m_var.is_null()); }

  var_t getVar() const override {
    assert(isVar());
    return m_var.get();
  }

  bool isConst() const { return (m_var.is_null()); }

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

/** A pointer literal is either a variable or constant null.**/
class crabPtrLit : public crabLit {
  friend class crabLitFactoryImpl;

  var_ref_t m_lit; // if m_lit.is_null() then the literal represents null

  crabPtrLit() : crabLit(CRAB_LITERAL_PTR) {} // null
  crabPtrLit(var_t v) : crabLit(CRAB_LITERAL_PTR), m_lit(v) {}

public:
  bool isVar() const override { return !m_lit.is_null(); }

  var_t getVar() const override {
    assert(isVar());
    return m_lit.get();
  }

  bool isNull() const { return m_lit.is_null(); }

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

  number_t m_num; // only considered if m_var.is_null();
  var_ref_t m_var;

  // If z_number != number_t we assume that number_t has a
  // constructor for z_number.
  explicit crabIntLit(ikos::z_number n) : crabLit(CRAB_LITERAL_INT), m_num(n) {}

  explicit crabIntLit(var_t v) : crabLit(CRAB_LITERAL_INT), m_var(v) {}

public:
  bool isVar() const override { return !m_var.is_null(); }

  var_t getVar() const override {
    assert(isVar());
    return m_var.get();
  }

  bool isInt() const { return m_var.is_null(); }

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

/* Implementation of a factory to create literals */
class crabLitFactoryImpl {
public:
  
  crabLitFactoryImpl(llvm_variable_factory &vfac,
                     const CrabBuilderParams &params);

  llvm_variable_factory &get_vfac() { return m_vfac; }

  const CrabBuilderParams &get_cfg_builder_params() const { return m_params; }

  // Translate v into a crab literal based on v's type
  crab_lit_ref_t getLit(const llvm::Value &v);

  // Create a fresh integer array variable
  var_t mkIntArrayVar(unsigned bitwidth);

  // Create a fresh boolean array variable
  var_t mkBoolArrayVar();

  // Create a fresh pointer array variable
  var_t mkPtrArrayVar();

  // Create an array variable associated with region r.
  var_t mkArrayVar(Region r, const llvm::Value *name);

  // Create an scalar variable associated with region r.
  var_t mkArraySingletonVar(Region r, const llvm::Value *name);

  // Create a fresh integer variable of bitwidth bits
  var_t mkIntVar(unsigned bitwidth);

  // Create a fresh boolean variable
  var_t mkBoolVar();

  // Create a fresh pointer variable
  var_t mkPtrVar();

  // Create a fresh variable from a Value
  llvm::Optional<var_t> mkVar(const llvm::Value &v);

  // Common accessors to crab_lit_ref_t subclasses.
  bool isBoolTrue(const crab_lit_ref_t ref) const;

  bool isBoolFalse(const crab_lit_ref_t ref) const;

  bool isPtrNull(const crab_lit_ref_t ref) const;

  number_t getIntCst(const crab_lit_ref_t ref) const;

  lin_exp_t getExp(const crab_lit_ref_t ref) const;

private:
  typedef std::unordered_map<const llvm::Value *, crab_lit_ref_t> lit_cache_t;
  typedef typename lit_cache_t::value_type binding_t;

  llvm_variable_factory &m_vfac;
  const CrabBuilderParams &m_params;
  lit_cache_t m_lit_cache;
  
  llvm::Optional<crabBoolLit> getBoolLit(const llvm::Value &v);
  llvm::Optional<crabIntLit> getIntLit(const llvm::Value &v);
  llvm::Optional<crabPtrLit> getPtrLit(const llvm::Value &v);
};

/**
 *  Factory to create crab literals: typed variable or number.
 **/
class crabLitFactory {
public:
  
  crabLitFactory(llvm_variable_factory &vfac, const CrabBuilderParams &params);

  ~crabLitFactory();

  llvm_variable_factory &get_vfac();

  crab::cfg::tracked_precision get_track() const;

  const CrabBuilderParams &get_cfg_builder_params() const;

  /** convert a Value to a crabLit **/
  crab_lit_ref_t getLit(const llvm::Value &v);

  /** make typed variables **/
  var_t mkIntVar(unsigned bitwidth);

  var_t mkBoolVar();

  var_t mkPtrVar();

  llvm::Optional<var_t> mkVar(const llvm::Value &v);

  var_t mkIntArrayVar(unsigned bitwidth);

  var_t mkBoolArrayVar();

  var_t mkPtrArrayVar();

  var_t mkArrayVar(Region r, const llvm::Value *name = nullptr);

  var_t mkArraySingletonVar(Region r, const llvm::Value *name = nullptr);
  
  /** direct accessors to crabLit subclasses **/
  bool isBoolTrue(const crab_lit_ref_t ref) const;

  bool isBoolFalse(const crab_lit_ref_t ref) const;

  bool isPtrNull(const crab_lit_ref_t ref) const;

  lin_exp_t getExp(const crab_lit_ref_t ref) const;

  number_t getIntCst(const crab_lit_ref_t ref) const;

private:
  crabLitFactoryImpl *m_impl;
};

} // end namespace clam
