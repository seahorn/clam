#include "CfgBuilderLit.hh"
#include "CfgBuilderUtils.hh"

#include "clam/Support/Debug.hh"
#include "clam/crab/crab_lang.hh"

#include "llvm/IR/Constants.h"

namespace clam {

using namespace llvm;
using namespace crab;

/* Implementation of a factory to create literals */
class crabLitFactoryImpl {
public:
  crabLitFactoryImpl(variable_factory_t &vfac,
                     const CrabBuilderParams &params);

  variable_factory_t &getVFac() { return m_vfac; }

  const CrabBuilderParams &getCfgBuilderParams() const { return m_params; }

  // Translate v into a crab literal based on v's type
  crab_lit_ref_t getLit(const llvm::Value &v, bool IntCstAsSigned);

  // Create a fresh integer variable of bitwidth bits
  var_t mkIntVar(unsigned bitwidth);

  // Create a fresh boolean variable
  var_t mkBoolVar();

  // Create a fresh reference variable
  var_t mkRefVar();

  // Create a fresh variable from a Value
  llvm::Optional<var_t> mkVar(const llvm::Value &v);

  // Inverse of mkVar. Return null if no Value found.
  const Value* getLLVMVar(const var_t &v) const;

  // Create a fresh array variable
  var_t mkArrayVar(RegionInfo rgnInfo);

  // Create a fresh region variable
  var_t mkRegionVar(RegionInfo rgnInfo);

  // Create an array variable associated with region rgn.
  var_t mkArrayVar(Region rgn);

  // Create a region variable associated with region rgn.
  var_t mkRegionVar(Region rgn);

  // Create an scalar variable associated with region rgn.
  var_t mkScalarVar(Region rgn);

  // Unsafe common accessors to crab_lit_ref_t subclasses.
  bool isBoolTrue(const crab_lit_ref_t ref) const;

  bool isBoolFalse(const crab_lit_ref_t ref) const;

  bool isRefNull(const crab_lit_ref_t ref) const;

  var_or_cst_t getTypedConst(const crab_lit_ref_t ref) const;

  number_t getIntCst(const crab_lit_ref_t ref) const;

  lin_exp_t getExp(const crab_lit_ref_t ref) const;

private:
  using lit_cache_t = std::unordered_map<const llvm::Value *, crab_lit_ref_t>;
  // We need this ordering for caching regions because the same
  // region's id can appear with two different types so we need to
  // treat them as different variables.
  // 
  // FIXME/REVISIT: not sure why this is possible but it happens.
  struct RegionCompare {
    // lexicographical ordering of region id and type
    bool operator()(const Region &rgn1, const Region &rgn2) const{
      return (rgn1.getId() == rgn2.getId() ?
	      rgn1.getRegionInfo() < rgn2.getRegionInfo() :
	      rgn1.getId() < rgn2.getId());
    }
  };
  using rgn_cache_t = std::map<Region, var_t, RegionCompare>;

  variable_factory_t &m_vfac;
  const CrabBuilderParams &m_params;
  lit_cache_t m_lit_cache;
  rgn_cache_t m_rgn_cache;

  llvm::Optional<crabBoolLit> getBoolLit(const llvm::Value &v);
  llvm::Optional<crabIntLit> getIntLit(const llvm::Value &v, bool IntCstAsSigned);
  llvm::Optional<crabRefLit> getRefLit(const llvm::Value &v);

  crab::variable_type regionTypeToCrabType(RegionInfo rgnInfo);
};

crabLitFactoryImpl::crabLitFactoryImpl(variable_factory_t &vfac,
                                       const CrabBuilderParams &params)
    : m_vfac(vfac), m_params(params) {}

crab_lit_ref_t crabLitFactoryImpl::getLit(const Value &v, bool IntCstAsSigned) {
  auto it = m_lit_cache.find(&v);
  if (it != m_lit_cache.end()) {
    return it->second;
  }
  const Type &t = *v.getType();
  // Note that getBoolLit, getRefLit and getIntLit are not aware of
  // which types are tracked or not. They only use type information
  // and not the track level.
  if (isBool(&t)) {
    Optional<crabBoolLit> lit = getBoolLit(v);
    if (lit.hasValue()) {
      crab_lit_ref_t ref = std::static_pointer_cast<crabLit>(
          std::make_shared<crabBoolLit>(lit.getValue()));
      m_lit_cache.insert({&v, ref});
      return ref;
    }
  } else if (isInteger(&t)) {
    Optional<crabIntLit> lit = getIntLit(v, IntCstAsSigned);
    if (lit.hasValue()) {
      crab_lit_ref_t ref = std::static_pointer_cast<crabLit>(
          std::make_shared<crabIntLit>(lit.getValue()));
      m_lit_cache.insert({&v, ref});
      return ref;
    }
  } else if (t.isPointerTy()) {
    Optional<crabRefLit> lit = getRefLit(v);
    if (lit.hasValue()) {
      crab_lit_ref_t ref = std::static_pointer_cast<crabLit>(
          std::make_shared<crabRefLit>(lit.getValue()));
      m_lit_cache.insert({&v, ref});
      return ref;
    }
  }
  return nullptr;
}

var_t crabLitFactoryImpl::mkArrayVar(Region rgn) {
  if (!m_params.trackOnlySingletonMemory()) {
    CLAM_ERROR("literal factory should not create a crab array variable");
  }

  auto it = m_rgn_cache.find(rgn);
  if (it != m_rgn_cache.end()) {
    return it->second;
  }

  crab::variable_type type(crab::UNK_TYPE);
  auto info = rgn.getRegionInfo();
  switch (info.getType().first) {
  case region_type_t::INT_REGION:
    type = crab::variable_type(ARR_INT_TYPE);
    break;
  case region_type_t::BOOL_REGION:
    type = crab::variable_type(ARR_BOOL_TYPE);
    break;
  default:
    CLAM_ERROR("unsupported region type " << rgn);
  }

  var_t res(m_vfac.get(), type);
  m_rgn_cache.insert({rgn, res});
  return res;
}

crab::variable_type
crabLitFactoryImpl::regionTypeToCrabType(RegionInfo rgnInfo) {
  crab::variable_type type(crab::UNK_TYPE);
  switch (rgnInfo.getType().first) {
  case region_type_t::INT_REGION:
    type = crab::variable_type(REG_INT_TYPE, rgnInfo.getType().second);
    break;
  case region_type_t::BOOL_REGION:
    type = crab::variable_type(REG_BOOL_TYPE, 1);
    break;
  case region_type_t::PTR_REGION:
    type = crab::variable_type(REG_REF_TYPE, 32);
    break;
  case region_type_t::UNTYPED_REGION:
    type = crab::variable_type(REG_UNKNOWN_TYPE);
    break;
  }
  return type;
}

var_t crabLitFactoryImpl::mkRegionVar(Region rgn) {
  if (!m_params.trackMemory()) {
    CLAM_ERROR("literal factory should not create a crab region variable");
  }

  auto it = m_rgn_cache.find(rgn);
  if (it != m_rgn_cache.end()) {
    return it->second;
  }
  
  crab::variable_type type = regionTypeToCrabType(rgn.getRegionInfo());
  var_t res(m_vfac.get(), type);
  m_rgn_cache.insert({rgn, res});
  return res;
}

var_t crabLitFactoryImpl::mkScalarVar(Region rgn) {

  auto it = m_rgn_cache.find(rgn);
  if (it != m_rgn_cache.end()) {
    return it->second;
  }
  
  unsigned bitwidth = 0;
  if (const Value *v = rgn.getSingleton()) {
    Type *ty = cast<PointerType>(v->getType())->getPointerElementType();
    bitwidth = ty->getIntegerBitWidth();
    if (rgn.getRegionInfo().getType().first == region_type_t::INT_REGION &&
        bitwidth <= 1) {
      CLAM_ERROR("Integer region must have bitwidth > 1");
    }
    // If the singleton contains a pointer then getIntegerBitWidth()
    // returns zero which means for us "unknown" bitwidth so we are
    // good.
  } else {
    CLAM_ERROR("Memory region does not belong to a global singleton");
  }

  crab::variable_type type(crab::UNK_TYPE);
  switch (rgn.getRegionInfo().getType().first) {
  case region_type_t::INT_REGION:
    type = crab::variable_type(INT_TYPE, bitwidth);
    break;
  case region_type_t::BOOL_REGION:
    type = crab::variable_type(BOOL_TYPE, 1);
    break;
  default:
    CLAM_ERROR("unsupported region type" << rgn);
  }

  var_t res(m_vfac.get(), type);
  m_rgn_cache.insert({rgn, res});
  return res;
}

var_t crabLitFactoryImpl::mkIntVar(unsigned bitwidth) {
  return var_t(m_vfac.get(), crab::variable_type(INT_TYPE, bitwidth));
}

var_t crabLitFactoryImpl::mkBoolVar() {
  return var_t(m_vfac.get(), crab::variable_type(BOOL_TYPE, 1));
}

var_t crabLitFactoryImpl::mkRefVar() {
  return var_t(m_vfac.get(), crab::variable_type(REF_TYPE, 32));
}

var_t crabLitFactoryImpl::mkArrayVar(RegionInfo rgnInfo) {
  if (!m_params.trackOnlySingletonMemory()) {
    CLAM_ERROR("literal factory should not create a crab array variable");
  }

  crab::variable_type type(crab::UNK_TYPE);
  switch (rgnInfo.getType().first) {
  case region_type_t::INT_REGION:
    type = crab::variable_type(ARR_INT_TYPE);
    break;
  case region_type_t::BOOL_REGION:
    type = crab::variable_type(ARR_BOOL_TYPE);
    break;
  default:
    CLAM_ERROR("unsupported region type for making an array variable "
               << rgnInfo);
  }
  return var_t(m_vfac.get(), type);
}

var_t crabLitFactoryImpl::mkRegionVar(RegionInfo rgnInfo) {
  if (!m_params.trackMemory()) {
    CLAM_ERROR("literal factory should not create a crab region variable");
  }

  crab::variable_type type = regionTypeToCrabType(rgnInfo);
  return var_t(m_vfac.get(), type);
}

Optional<var_t> crabLitFactoryImpl::mkVar(const Value &v) {
  if (isBool(v)) {
    return mkBoolVar();
  } else if (isInteger(v)) {
    unsigned bitwidth = v.getType()->getIntegerBitWidth();
    return mkIntVar(bitwidth);
  } else if (isReference(v, m_params)) {
    return mkRefVar();
  }
  return None;
}

const Value* crabLitFactoryImpl::getLLVMVar(const var_t &v) const {
  const Value *res = nullptr;
  if (v.name().get()) {
    res = dyn_cast<const Value>(*(v.name().get()));
  }
  return res;
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

bool crabLitFactoryImpl::isRefNull(const crab_lit_ref_t ref) const {
  if (!ref || !ref->isRef())
    CLAM_ERROR("Literal is not a pointer");
  auto lit = std::static_pointer_cast<const crabRefLit>(ref);
  return lit->isNull();
}

var_or_cst_t crabLitFactoryImpl::getTypedConst(const crab_lit_ref_t ref) const {
  if (ref->isBool()) {
    auto b = std::static_pointer_cast<const crabBoolLit>(ref);
    if (b->isConst()) {
      return b->getTypedConst();
    } else {
      CLAM_ERROR("Called getTypedConst on a non-constant literal");
    }
  } else if (ref->isInt()) {
    auto i = std::static_pointer_cast<const crabIntLit>(ref);
    if (i->isInt()) {
      return i->getTypedConst();
    } else {
      CLAM_ERROR("Called getTypedConst on a non-constant literal");
    }
  } else if (ref->isRef()) {
    auto r = std::static_pointer_cast<const crabRefLit>(ref);
    if (r->isNull()) {
      return r->getTypedConst();
    } else {
      CLAM_ERROR("called getTypedConst on a non-constant literal");
    }
  } else { /*unreachable*/
    CLAM_ERROR("called getTypedConst on an unexpected literal");
  }
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
      bool isTooBig;
      ikos::z_number n = getIntConstant(c, m_params, true/*IntCstAsSigned*/, isTooBig);
      if (!isTooBig) {
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

Optional<crabRefLit> crabLitFactoryImpl::getRefLit(const Value &v) {
  if (isa<ConstantPointerNull>(&v)) {
    // -- constant null
    return crabRefLit();
  } else if (v.getType()->isPointerTy() && !isa<ConstantExpr>(v)) {
    // -- pointer variable
    if (isa<UndefValue>(v)) {
      // Create a fresh variable: this treats an undef value as a
      // nondeterministic value.
      return crabRefLit(var_t(m_vfac.get(), REF_TYPE));
    } else {
      return crabRefLit(var_t(m_vfac[&v], REF_TYPE));
    }
  }
  return None;
}

Optional<crabIntLit> crabLitFactoryImpl::getIntLit(const Value &v, bool IntCstAsSigned) {
  if (isInteger(v)) {
    if (const ConstantInt *c = dyn_cast<const ConstantInt>(&v)) {
      // -- constant integer
      unsigned bitwidth = c->getType()->getIntegerBitWidth();
      bool isTooBig;
      ikos::z_number n = getIntConstant(c, m_params, IntCstAsSigned, isTooBig);
      if (!isTooBig) {
        return crabIntLit(n, bitwidth);
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

crabLitFactory::crabLitFactory(variable_factory_t &vfac,
                               const CrabBuilderParams &params)
    : m_impl(new crabLitFactoryImpl(vfac, params)) {}

crabLitFactory::~crabLitFactory() { delete m_impl; }

variable_factory_t &crabLitFactory::getVFac() { return m_impl->getVFac(); }

CrabBuilderPrecision crabLitFactory::getTrack() const {
  return getCfgBuilderParams().precision_level;
}

const CrabBuilderParams &crabLitFactory::getCfgBuilderParams() const {
  return m_impl->getCfgBuilderParams();
}

crab_lit_ref_t crabLitFactory::getLit(const Value &v, bool IntCstAsSigned) {
  return m_impl->getLit(v, IntCstAsSigned);
}

var_t crabLitFactory::mkIntVar(unsigned bitwidth) {
  return m_impl->mkIntVar(bitwidth);
}

var_t crabLitFactory::mkBoolVar() { return m_impl->mkBoolVar(); }

var_t crabLitFactory::mkRefVar() { return m_impl->mkRefVar(); }

var_t crabLitFactory::mkArrayVar(RegionInfo rgnInfo) {
  return m_impl->mkArrayVar(rgnInfo);
}

var_t crabLitFactory::mkRegionVar(RegionInfo rgnInfo) {
  return m_impl->mkRegionVar(rgnInfo);
}

Optional<var_t> crabLitFactory::mkVar(const Value &v) {
  return m_impl->mkVar(v);
}

const Value* crabLitFactory::getLLVMVar(const var_t &v) const {
  return m_impl->getLLVMVar(v);
}
  
var_t crabLitFactory::mkArrayVar(Region rgn) { return m_impl->mkArrayVar(rgn); }

var_t crabLitFactory::mkRegionVar(Region rgn) {
  return m_impl->mkRegionVar(rgn);
}

var_t crabLitFactory::mkScalarVar(Region rgn) {
  return m_impl->mkScalarVar(rgn);
}

bool crabLitFactory::isBoolTrue(const crab_lit_ref_t ref) const {
  return m_impl->isBoolTrue(ref);
}

bool crabLitFactory::isBoolFalse(const crab_lit_ref_t ref) const {
  return m_impl->isBoolFalse(ref);
}

bool crabLitFactory::isRefNull(const crab_lit_ref_t ref) const {
  return m_impl->isRefNull(ref);
}

var_or_cst_t crabLitFactory::getTypedConst(const crab_lit_ref_t ref) const {
  return m_impl->getTypedConst(ref);
}

lin_exp_t crabLitFactory::getExp(const crab_lit_ref_t ref) const {
  return m_impl->getExp(ref);
}

number_t crabLitFactory::getIntCst(const crab_lit_ref_t ref) const {
  return m_impl->getIntCst(ref);
}

} // end namespace clam
