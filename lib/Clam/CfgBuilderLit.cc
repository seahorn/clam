#include "CfgBuilderLit.hh"
#include "CfgBuilderUtils.hh"

#include "clam/Support/Debug.hh"
#include "clam/crab/crab_cfg.hh"

#include "llvm/IR/Constants.h"

namespace clam {

using namespace llvm;
using namespace crab;

crabLitFactoryImpl::crabLitFactoryImpl(llvm_variable_factory &vfac,
                                       const CrabBuilderParams &params)
    : m_vfac(vfac), m_params(params) {}

crab_lit_ref_t crabLitFactoryImpl::getLit(const Value &v) {
  auto it = m_lit_cache.find(&v);
  if (it != m_lit_cache.end()) {
    return it->second;
  }
  const Type &t = *v.getType();
  // Note that getBoolLit, getPtrLit and getIntLit are not aware of
  // which types are tracked or not. They only use type information
  // and not the track level.
  if (isBool(&t)) {
    Optional<crabBoolLit> lit = getBoolLit(v);
    if (lit.hasValue()) {
      crab_lit_ref_t ref = std::static_pointer_cast<crabLit>(
          std::make_shared<crabBoolLit>(lit.getValue()));
      m_lit_cache.insert(binding_t(&v, ref));
      return ref;
    }
  } else if (isInteger(&t)) {
    Optional<crabIntLit> lit = getIntLit(v);
    if (lit.hasValue()) {
      crab_lit_ref_t ref = std::static_pointer_cast<crabLit>(
          std::make_shared<crabIntLit>(lit.getValue()));
      m_lit_cache.insert(binding_t(&v, ref));
      return ref;
    }
  } else if (t.isPointerTy()) {
    Optional<crabPtrLit> lit = getPtrLit(v);
    if (lit.hasValue()) {
      crab_lit_ref_t ref = std::static_pointer_cast<crabLit>(
          std::make_shared<crabPtrLit>(lit.getValue()));
      m_lit_cache.insert(binding_t(&v, ref));
      return ref;
    }
  }
  return nullptr;
}

var_t crabLitFactoryImpl::mkArrayVar(Region mem_region, const Value *name) {
  crab::variable_type type = crab::UNK_TYPE;
  unsigned bitwidth = 0; /* unknown */
  auto info = mem_region.getRegionInfo();
  switch (info.get_type()) {
  case INT_REGION:
    type = ARR_INT_TYPE;
    bitwidth = info.get_bitwidth();
    break;
  case BOOL_REGION:
    type = ARR_BOOL_TYPE;
    bitwidth = 1;
    break;
  case PTR_REGION:
    type = ARR_PTR_TYPE;
    break;
  default:
    CLAM_ERROR("unsupported region type");
  }  
  auto varname = (name ? m_vfac[name] : m_vfac.get(mem_region.get_id()));  
  return var_t(varname, type, bitwidth);
}

var_t crabLitFactoryImpl::mkArraySingletonVar(Region mem_region,
					      const Value *name) {
  crab::variable_type type = crab::UNK_TYPE;
  unsigned bitwidth = 0; /* unknown */
  if (const Value *v = mem_region.getSingleton()) {
    Type *ty = cast<PointerType>(v->getType())->getElementType();
    bitwidth = ty->getIntegerBitWidth();
    if (mem_region.getRegionInfo().get_type() == INT_REGION && bitwidth <= 1) {
      CLAM_ERROR("Integer region must have bitwidth > 1");
    }
    // If the singleton contains a pointer then getIntegerBitWidth()
    // returns zero which means for us "unknown" bitwidth so we are
    // good.
  } else {
    CLAM_ERROR("Memory region does not belong to a global singleton");
  }
  switch (mem_region.getRegionInfo().get_type()) {
  case INT_REGION:
    type = INT_TYPE;
    break;
  case BOOL_REGION:
    type = BOOL_TYPE;
    break;
  case PTR_REGION:
    type = PTR_TYPE;
    break;
  default:
    CLAM_ERROR("unsupported region type");
  }
  auto varname = (name ? m_vfac[name] : m_vfac.get(mem_region.get_id()));
  return var_t(varname, type, bitwidth);
}

var_t crabLitFactoryImpl::mkIntArrayVar(unsigned bitwidth) {
  return var_t(m_vfac.get(), ARR_INT_TYPE, bitwidth);
}

var_t crabLitFactoryImpl::mkBoolArrayVar() {
  return var_t(m_vfac.get(), ARR_BOOL_TYPE, 1);
}

var_t crabLitFactoryImpl::mkPtrArrayVar() {
  return var_t(m_vfac.get(), ARR_PTR_TYPE);
}

var_t crabLitFactoryImpl::mkIntVar(unsigned bitwidth) {
  return var_t(m_vfac.get(), INT_TYPE, bitwidth);
}

var_t crabLitFactoryImpl::mkBoolVar() {
  return var_t(m_vfac.get(), BOOL_TYPE, 1);
}

var_t crabLitFactoryImpl::mkPtrVar() { return var_t(m_vfac.get(), PTR_TYPE); }

Optional<var_t> crabLitFactoryImpl::mkVar(const Value &v) {
  if (isBool(v)) {
    return mkBoolVar();
  } else if (isInteger(v)) {
    unsigned bitwidth = v.getType()->getIntegerBitWidth();
    return mkIntVar(bitwidth);
  } else if (isPointer(v, m_params)) {
    return mkPtrVar();
  }
  return None;
}

bool crabLitFactoryImpl::isBoolTrue(const crab_lit_ref_t ref) const {
  if (!ref || !ref->isBool())
    CLAM_ERROR("Literal is not a Boolean");
  auto lit = std::static_pointer_cast<const crabBoolLit>(ref);
  return lit->isTrue();
}

bool crabLitFactoryImpl::isBoolFalse(const crab_lit_ref_t ref) const {
  if (!ref || !ref->isBool())
    CLAM_ERROR("Literal is not a Boolean");
  auto lit = std::static_pointer_cast<const crabBoolLit>(ref);
  return lit->isFalse();
}

bool crabLitFactoryImpl::isPtrNull(const crab_lit_ref_t ref) const {
  if (!ref || !ref->isPtr())
    CLAM_ERROR("Literal is not a pointer");
  auto lit = std::static_pointer_cast<const crabPtrLit>(ref);
  return lit->isNull();
}

lin_exp_t crabLitFactoryImpl::getExp(const crab_lit_ref_t ref) const {
  if (!ref || !ref->isInt())
    CLAM_ERROR("Literal is not an integer");
  auto lit = std::static_pointer_cast<const crabIntLit>(ref);
  return lit->getExp();
}

number_t crabLitFactoryImpl::getIntCst(const crab_lit_ref_t ref) const {
  if (!ref || !ref->isInt())
    CLAM_ERROR("Literal is not an integer");
  auto lit = std::static_pointer_cast<const crabIntLit>(ref);
  return lit->getInt();
}

Optional<crabBoolLit> crabLitFactoryImpl::getBoolLit(const Value &v) {
  if (isBool(v)) {
    if (const ConstantInt *c = dyn_cast<const ConstantInt>(&v)) {
      // -- constant boolean
      bool is_bignum;
      ikos::z_number n = getIntConstant(c, m_params, is_bignum);
      if (!is_bignum) {
        return crabBoolLit(n > 0 ? true : false);
      }
    } else if (!isa<ConstantExpr>(v)) {
      // -- boolean variable
      if (isa<UndefValue>(v)) {
	// Create a fresh variable: this treats an undef value as a
	// nondeterministic value.
	return crabBoolLit(var_t(m_vfac.get(), BOOL_TYPE, 1));
      } else {
	return crabBoolLit(var_t(m_vfac[&v], BOOL_TYPE, 1));
      }
    }
  }
  return None;
}

Optional<crabPtrLit> crabLitFactoryImpl::getPtrLit(const Value &v) {
  if (isa<ConstantPointerNull>(&v)) {
    // -- constant null
    return crabPtrLit();
  } else if (v.getType()->isPointerTy() && !isa<ConstantExpr>(v)) {
    // -- pointer variable
      if (isa<UndefValue>(v)) {
	// Create a fresh variable: this treats an undef value as a
	// nondeterministic value.
	return crabPtrLit(var_t(m_vfac.get(), PTR_TYPE));
      } else {
	return crabPtrLit(var_t(m_vfac[&v], PTR_TYPE));
      }
  }
  return None;
}

Optional<crabIntLit> crabLitFactoryImpl::getIntLit(const Value &v) {
  if (isInteger(v)) {
    if (const ConstantInt *c = dyn_cast<const ConstantInt>(&v)) {
      // -- constant integer
      bool is_bignum;
      ikos::z_number n = getIntConstant(c, m_params, is_bignum);
      if (!is_bignum) {
        return crabIntLit(n);
      }
    } else if (!isa<ConstantExpr>(v)) {
      // -- integer variable
      unsigned bitwidth = v.getType()->getIntegerBitWidth();
      if (isa<UndefValue>(v)) {
	// Create a fresh variable: this treats an undef value as a
	// nondeterministic value.
	return crabIntLit(var_t(m_vfac.get(), INT_TYPE, bitwidth));
      } else {
	return crabIntLit(var_t(m_vfac[&v], INT_TYPE, bitwidth));
      }
    }
  }
  return None;
}

crabLitFactory::crabLitFactory(llvm_variable_factory &vfac,
                               const CrabBuilderParams &params)
    : m_impl(new crabLitFactoryImpl(vfac, params)) {}

crabLitFactory::~crabLitFactory() { delete m_impl; }

llvm_variable_factory &crabLitFactory::get_vfac() { return m_impl->get_vfac(); }

crab::cfg::tracked_precision crabLitFactory::get_track() const {
  return get_cfg_builder_params().precision_level;
}

const CrabBuilderParams &crabLitFactory::get_cfg_builder_params() const {
  return m_impl->get_cfg_builder_params();
}

crab_lit_ref_t crabLitFactory::getLit(const Value &v) {
  return m_impl->getLit(v);
}

var_t crabLitFactory::mkIntArrayVar(unsigned bitwidth) {
  return m_impl->mkIntArrayVar(bitwidth);
}

var_t crabLitFactory::mkBoolArrayVar() { return m_impl->mkBoolArrayVar(); }

var_t crabLitFactory::mkPtrArrayVar() { return m_impl->mkPtrArrayVar(); }

var_t crabLitFactory::mkArrayVar(Region r, const Value *name) {
  return m_impl->mkArrayVar(r, name);
}

var_t crabLitFactory::mkArraySingletonVar(Region r, const Value *name) {
  return m_impl->mkArraySingletonVar(r, name);
}

var_t crabLitFactory::mkIntVar(unsigned bitwidth) {
  return m_impl->mkIntVar(bitwidth);
}

var_t crabLitFactory::mkBoolVar() { return m_impl->mkBoolVar(); }

var_t crabLitFactory::mkPtrVar() { return m_impl->mkPtrVar(); }

Optional<var_t> crabLitFactory::mkVar(const Value &v) {
  return m_impl->mkVar(v);
}

bool crabLitFactory::isBoolTrue(const crab_lit_ref_t ref) const {
  return m_impl->isBoolTrue(ref);
}

bool crabLitFactory::isBoolFalse(const crab_lit_ref_t ref) const {
  return m_impl->isBoolFalse(ref);
}

bool crabLitFactory::isPtrNull(const crab_lit_ref_t ref) const {
  return m_impl->isPtrNull(ref);
}

lin_exp_t crabLitFactory::getExp(const crab_lit_ref_t ref) const {
  return m_impl->getExp(ref);
}

number_t crabLitFactory::getIntCst(const crab_lit_ref_t ref) const {
  return m_impl->getIntCst(ref);
}

} // end namespace clam
