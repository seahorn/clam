/*
 * Translate a LLVM function to a CFG language understood by Crab.
 *
 * Crab supports operations over boolean, integers and
 * pointers. Moreover, Crab supports unidimensional arrays. Arrays are
 * interpreted as sequence of consecutive bytes which are disjoint
 * from each other.
 *
 * The translation of LLVM integer operations (tracked precision = NUM)
 * is pretty straightforward.  LLVM branches are translated to Crab
 * assume and goto statements. The translation also removes phi nodes.
 *
 * If tracked precision = SINGLETON_MEM then only pointer operations
 * over singleton memory objects are translated to Crab array
 * operations. This translation requires the Heap analysis used to
 * partition statically memory into disjoint regions. Then, each
 * memory region's _field_ (i.e., Burstall's memory model) is mapped
 * to a Crab array and LLVM load/store are translated to array
 * read/write statements. A memory object is considered a singleton if
 * it represents a single allocated memory region. This is usually the
 * case of global and stack variables.
 *
 * If tracked precision = MEM then all LLVM pointer operations are
 * translated to Crab reference operations. This translation is almost
 * one-to-one but it also requires the use of the Heap
 * Analysis. Sometimes, the LLVM instruction can be safely ignored if
 * the Heap Analysis is too imprecise.
 *
 * The translation of function calls is also straightforward except
 * that all functions are _purified_ if tracked precision != NUM. That
 * is, the translation ensures that functions have no side-effects.
 *
 * Known limitations of the translation:
 *
 * - Ignore floating point instructions.
 * - Ignore inttoptr/ptrtoint instructions.
 * - Almost ignore memset/memmove/memcpy.
 * - Only if tracked precision = SINGLETON_MEM: if a `LoadInst`'s lhs
 *   (`StoreInst` value operand) is a pointer then the translation
 *   ignores safely the LLVM instruction and hence, it won't add the
 *   corresponding Crab array statement `array_load` (`array_store`).
 */

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include "CfgBuilderLit.hh"
#include "CfgBuilderMemRegions.hh"
#include "CfgBuilderUtils.hh"

#include "clam/CfgBuilder.hh"
#include "clam/DummyHeapAbstraction.hh"
#include "clam/HeapAbstraction.hh"       
#include "clam/SeaDsaHeapAbstraction.hh" 
#include "clam/Support/CFG.hh"
#include "clam/Support/Debug.hh"
#include "crab/support/debug.hpp"
#include "crab/support/stats.hpp"
#include "crab/transforms/dce.hpp"

#include <algorithm>
#include <boost/functional/hash_fwd.hpp> // for hash_combine
#include <unordered_map>

using namespace llvm;
using namespace crab;
using namespace ikos;
using namespace crab::cfg;

namespace {

using namespace clam;

bool checkAllDefinitionsHaveNames(const Function &F) {
  // for (auto &Arg : F.args()) {
  //   if (!Arg.hasName()) {
  // 	return false;
  //   }
  // }

  for (const BasicBlock &BB : F) {
    if (!BB.hasName()) {
      return false;
    }
    for (const Instruction &I : BB) {
      if (!I.hasName() && !(I.getType()->isVoidTy())) {
        return false;
      }
    }
  }
  return true;
}

void havoc(var_t v, basic_block_t &bb, bool include_useless_havoc) {
  if (include_useless_havoc) {
    bb.havoc(v);
  }
}

// %x = icmp geq %y, 10  ---> bool_assign(%x, y >= 0)
void cmpInstToCrabBool(CmpInst &I, crabLitFactory &lfac, basic_block_t &bb) {
  // The type of I is a boolean or vector of booleans
  normalizeCmpInst(I);

  const Value &v0 = *I.getOperand(0);
  const Value &v1 = *I.getOperand(1);

  crab_lit_ref_t ref = lfac.getLit(I);
  if (!ref || !(ref->isBool()) || !(ref->isVar())) {
    // It could be here if the type of I is a vector of booleans.
    // We prefer to raise an error.
    CLAM_ERROR("lhs of CmpInst should be a Boolean");
  }
  var_t lhs = ref->getVar();

  crab_lit_ref_t ref0 = lfac.getLit(v0);
  if (!ref0 || !(ref0->isInt())) {
    havoc(lhs, bb, lfac.getCfgBuilderParams().include_useless_havoc);
    return;
  }

  crab_lit_ref_t ref1 = lfac.getLit(v1);
  if (!ref1 || !(ref1->isInt())) {
    havoc(lhs, bb, lfac.getCfgBuilderParams().include_useless_havoc);
    return;
  }

  lin_exp_t op0 = lfac.getExp(ref0);
  lin_exp_t op1 = lfac.getExp(ref1);

  assert(isBool(I));
  switch (I.getPredicate()) {
  case CmpInst::ICMP_EQ: {
    lin_cst_t cst(op0 == op1);
    bb.bool_assign(lhs, cst);
    break;
  }
  case CmpInst::ICMP_NE: {
    lin_cst_t cst(op0 != op1);
    bb.bool_assign(lhs, cst);
    break;
  }
  case CmpInst::ICMP_ULT:
  case CmpInst::ICMP_SLT: {
    lin_cst_t cst(op0 <= op1 - number_t(1));
    if (I.getPredicate() == CmpInst::ICMP_ULT) {
      cst.set_unsigned();
    }
    bb.bool_assign(lhs, cst);
    break;
  }
  case CmpInst::ICMP_ULE:
  case CmpInst::ICMP_SLE: {
    lin_cst_t cst(op0 <= op1);
    if (I.getPredicate() == CmpInst::ICMP_ULE) {
      cst.set_unsigned();
    }
    bb.bool_assign(lhs, cst);
    break;
  }
  default:
    CLAM_ERROR("unexpected problem while translating CmpInst");
  }
}

/* If possible, return a Crab reference constraint from CmpInst */
Optional<ref_cst_t> cmpInstToCrabRef(CmpInst &I, crabLitFactory &lfac,
                                     const bool isNegated) {
  normalizeCmpInst(I);

  const Value &v0 = *I.getOperand(0);
  const Value &v1 = *I.getOperand(1);

  crab_lit_ref_t ref0 = lfac.getLit(v0);
  if (!ref0 || !(ref0->isRef()))
    return llvm::None;

  crab_lit_ref_t ref1 = lfac.getLit(v1);
  if (!ref1 || !(ref1->isRef()))
    return llvm::None;
  
  CmpInst::Predicate op = (isNegated ? I.getInversePredicate() : I.getPredicate());
  switch (op) {
  case CmpInst::ICMP_EQ:
    if (ref0->isVar() && lfac.isRefNull(ref1)) {
      return ref_cst_t::mk_null(ref0->getVar());
    } else if (lfac.isRefNull(ref0) && ref1->isVar()) {
      return ref_cst_t::mk_null(ref1->getVar());
    } else if (ref0->isVar() && ref1->isVar()) {
      return ref_cst_t::mk_eq(ref0->getVar(), ref1->getVar());
    } else {
      return ref_cst_t::mk_true();
    }    
  case CmpInst::ICMP_NE:
    if (ref0->isVar() && lfac.isRefNull(ref1)) {
      return ref_cst_t::mk_not_null(ref0->getVar());
    } else if (lfac.isRefNull(ref0) && ref1->isVar()) {
      return ref_cst_t::mk_not_null(ref1->getVar());
    } else if (ref0->isVar() && ref1->isVar()) {
      return ref_cst_t::mk_not_eq(ref0->getVar(), ref1->getVar());
    } else {
      return ref_cst_t::mk_false();
    }    
  case CmpInst::ICMP_ULT:
  case CmpInst::ICMP_SLT: {
    if (ref0->isVar() && lfac.isRefNull(ref1)) {
      // ref0 < null      
      return ref_cst_t::mk_lt_null(ref0->getVar());
    } else if (lfac.isRefNull(ref0) && ref1->isVar()) {
      // null < ref1 <--> negate(ref1 <= null) <--> ref1 > null
      return ref_cst_t::mk_le_null(ref1->getVar()).negate();
    } else if (ref0->isVar() && ref1->isVar()) {
      // ref0 < ref1      
      return ref_cst_t::mk_lt(ref0->getVar(), ref1->getVar());
    } else {
      break;
    }    
  }
  case CmpInst::ICMP_ULE:
  case CmpInst::ICMP_SLE: {
    if (ref0->isVar() && lfac.isRefNull(ref1)) {
      // ref0 <= null
      return ref_cst_t::mk_le_null(ref0->getVar());      
    } else if (lfac.isRefNull(ref0) && ref1->isVar()) {
      // null <= ref1 <--> negate(ref1 < null) <--> ref1 >= null
      return ref_cst_t::mk_lt_null(ref1->getVar()).negate();
    } else if (ref0->isVar() && ref1->isVar()) {
      // ref0 <= ref1
    } else {
      break;
    }        
  }
  default:;;
  }
  CLAM_WARNING("TODO: unsupported pointer comparison " << I);
}

/* If possible, return a Crab linear constraint from CmpInst */
Optional<lin_cst_t> cmpInstToCrabInt(CmpInst &I, crabLitFactory &lfac,
                                     const bool isNegated = false) {
  normalizeCmpInst(I);

  const Value &v0 = *I.getOperand(0);
  const Value &v1 = *I.getOperand(1);

  crab_lit_ref_t ref0 = lfac.getLit(v0);
  if (!ref0 || !(ref0->isInt()))
    return llvm::None;

  crab_lit_ref_t ref1 = lfac.getLit(v1);
  if (!ref1 || !(ref1->isInt()))
    return llvm::None;

  lin_exp_t op0 = lfac.getExp(ref0);
  lin_exp_t op1 = lfac.getExp(ref1);

  switch (I.getPredicate()) {
  case CmpInst::ICMP_EQ:
    if (!isNegated)
      return lin_cst_t(op0 == op1);
    else
      return lin_cst_t(op0 != op1);
    break;
  case CmpInst::ICMP_NE:
    if (!isNegated)
      return lin_cst_t(op0 != op1);
    else
      return lin_cst_t(op0 == op1);
    break;
  case CmpInst::ICMP_ULT:
  case CmpInst::ICMP_SLT: {
    lin_cst_t cst;
    if (!isNegated)
      cst = lin_cst_t(op0 <= op1 - number_t(1));
    else
      cst = lin_cst_t(op0 >= op1);
    if (I.getPredicate() == CmpInst::ICMP_ULT) {
      cst.set_unsigned();
    }
    return cst;
    break;
  }
  case CmpInst::ICMP_ULE:
  case CmpInst::ICMP_SLE: {
    lin_cst_t cst;
    if (!isNegated)
      cst = lin_cst_t(op0 <= op1);
    else
      cst = lin_cst_t(op0 >= op1 + number_t(1));
    if (I.getPredicate() == CmpInst::ICMP_ULE) {
      cst.set_unsigned();
    }
    return cst;
    break;
  }
  default:;
    ;
  }
  return llvm::None;
}

// This function makes sure that all actual parameters and function
// return values are variables. This is required by crab.
// precondition: v is tracked.
var_t normalizeFuncParamOrRet(Value &v, basic_block_t &bb,
                              crabLitFactory &lfac) {
  if (crab_lit_ref_t ref = lfac.getLit(v)) {
    if (ref->isVar()) {
      return ref->getVar();
    } else {
      // must be constant
      if (ref->isInt()) {
        unsigned bitwidth = v.getType()->getIntegerBitWidth();
        var_t res = lfac.mkIntVar(bitwidth);
        bb.assign(res, lfac.getExp(ref));
        return res;
      } else if (ref->isBool()) {
        var_t res = lfac.mkBoolVar();
        bb.bool_assign(res, lfac.isBoolTrue(ref) ? lin_cst_t::get_true()
                                                 : lin_cst_t::get_false());
        return res;
      } else if (ref->isRef()) {
        var_t res = lfac.mkRefVar();
        bb.assume_ref(ref_cst_t::mk_null(res));
        return res;
      }
    }
  }

  if (isTracked(v, lfac.getCfgBuilderParams())) {
    if (isa<ConstantExpr>(v)) {
      CLAM_WARNING(
          "Clam cfg builder created a fresh variable from constant expr");
      llvm::Optional<var_t> fresh_v = lfac.mkVar(v);
      if (fresh_v.hasValue()) {
        return fresh_v.getValue();
      }
    }
    // we should not reach this point since v is tracked.
    CLAM_ERROR("cannot normalize function parameter or return value");
    // clang complains otherwise
    abort();
  }
}

/* Return a crab memory region from a clam memory region */
crab::memory_region mkCrabRegion(clam::Region rgn) {
  switch (rgn.getRegionInfo().getType()) {
  case region_type_t::BOOL_REGION:
    return crab::memory_region::make_bool_memory_region(rgn.getId());
  case region_type_t::INT_REGION:
    return crab::memory_region::make_int_memory_region(rgn.getId(),
						       rgn.getRegionInfo().getBitwidth());    
  case region_type_t::PTR_REGION:
    return crab::memory_region::make_ref_memory_region(rgn.getId());
  default:
    CRAB_ERROR("Cannot create a crab memory region from an unknown clam region");
  }
}
  
//! Translate PHI nodes
struct CrabPhiVisitor : public InstVisitor<CrabPhiVisitor> {

  crabLitFactory &m_lfac;
  HeapAbstraction &m_mem;
  RegionSet &m_func_regions;
  const DataLayout &m_dl;
  // block where assignment will be inserted
  basic_block_t &m_bb;
  // incoming block of the PHI instruction
  const BasicBlock &m_inc_BB;
  // builder parameters
  const CrabBuilderParams &m_params;

  CrabPhiVisitor(crabLitFactory &lfac, HeapAbstraction &mem, RegionSet &func_regions,
		 const DataLayout &dl, basic_block_t &bb,
                 const BasicBlock &inc_BB, const CrabBuilderParams &params)
    : m_lfac(lfac), m_mem(mem), m_func_regions(func_regions),
      m_dl(dl), m_bb(bb), m_inc_BB(inc_BB), m_params(params) {}

  void visitBasicBlock(BasicBlock &BB) {
    if (!isa<PHINode>(BB.begin())) {
      return;
    }

    // Map an PHI incoming value to a Crab variable
    DenseMap<const Value *, var_t> old_val_map;

    // --- All the phi-nodes must be evaluated atomically. This
    //     means that if one phi node v1 has as incoming value
    //     another phi node v2 in the same block then it should take
    //     the v2's old value (i.e., before v2's evaluation).

    auto curr = BB.begin();
    for (; PHINode *phi = dyn_cast<PHINode>(curr); ++curr) {
      const Value &v = *phi->getIncomingValueForBlock(&m_inc_BB);
      if (!isTracked(v, m_lfac.getCfgBuilderParams())) {
        continue;
      }
      const PHINode *phi_v = dyn_cast<PHINode>(&v);
      if (phi_v && (phi_v->getParent() == &BB)) {
        // -- save the old version of the variable that maps to the
        //    phi node v
        auto it = old_val_map.find(&v);
        if (it == old_val_map.end()) {
          if (crab_lit_ref_t phi_val_ref = m_lfac.getLit(v)) {
            // non-shadow mem phi node: bool, integer, or pointer

            if (phi->getName().startswith("shadow.mem")) {
              // XXX: Ignore PHI shadow mem instructions.
              continue;
            }

            if (phi_val_ref->isBool()) {
              var_t lhs = m_lfac.mkBoolVar();
              if (phi_val_ref->isVar()) {
                m_bb.bool_assign(lhs, phi_val_ref->getVar());
              } else {
                m_bb.bool_assign(lhs, m_lfac.isBoolTrue(phi_val_ref)
                                          ? lin_cst_t::get_true()
                                          : lin_cst_t::get_false());
              }
              old_val_map.insert({&v, lhs});
            } else if (phi_val_ref->isInt()) {
              var_t lhs =
                  m_lfac.mkIntVar(phi_v->getType()->getIntegerBitWidth());
              m_bb.assign(lhs, m_lfac.getExp(phi_val_ref));
              old_val_map.insert({&v, lhs});
            } else if (phi_val_ref->isRef()) {
              var_t lhs = m_lfac.mkRefVar();
              if (phi_val_ref->isVar()) {
		Region rgn_phi_val = getRegion(m_mem, m_func_regions, m_params, *phi_v, *phi_v);
		if (!rgn_phi_val.isUnknown()) {
		  m_bb.gep_ref(lhs, mkCrabRegion(rgn_phi_val),
			       phi_val_ref->getVar(), mkCrabRegion(rgn_phi_val));
		}
              } else {
                m_bb.assume_ref(ref_cst_t::mk_null(lhs));
              }
              old_val_map.insert({&v, lhs});
            } else {
              /* unreachable */
            }
          }
        }
      }
    }

    curr = BB.begin();
    for (; isa<PHINode>(curr); ++curr) {
      const PHINode &phi = *cast<PHINode>(curr);
      if (!isTracked(phi, m_lfac.getCfgBuilderParams())) {
        continue;
      }
      const Value &v = *phi.getIncomingValueForBlock(&m_inc_BB);
      if (phi.getName().startswith("shadow.mem")) {
        // XXX: ignore PHI shadow mem instructions.
        continue;
      }

      /// Regular PHI node: bool, integer, or pointer
      crab_lit_ref_t lhs_ref = m_lfac.getLit(phi);
      if (!lhs_ref || !lhs_ref->isVar()) {
        CLAM_ERROR("unexpected PHI instruction");
      }
      var_t lhs = lhs_ref->getVar();
      auto it = old_val_map.find(&v);
      if (it != old_val_map.end()) {
        // -- use old version if exists
        if (isBool(phi)) {
          m_bb.bool_assign(lhs, it->second);
        } else if (phi.getType()->isIntegerTy()) {
          m_bb.assign(lhs, it->second);
        } else if (isReference(phi, m_lfac.getCfgBuilderParams())) {
	  Region rgn_phi = getRegion(m_mem, m_func_regions, m_params, phi, phi);
	  if (!rgn_phi.isUnknown()) {
	    m_bb.gep_ref(lhs, mkCrabRegion(rgn_phi),
			 it->second, mkCrabRegion(rgn_phi));
	  }
        }
      } else {
        if (crab_lit_ref_t phi_val_ref = m_lfac.getLit(v)) {
          if (phi_val_ref->isBool()) {
            if (phi_val_ref->isVar()) {
              m_bb.bool_assign(lhs, phi_val_ref->getVar());
            } else {
              m_bb.bool_assign(lhs, m_lfac.isBoolTrue(phi_val_ref)
                                        ? lin_cst_t::get_true()
                                        : lin_cst_t::get_false());
            }
          } else if (phi_val_ref->isInt()) {
            m_bb.assign(lhs, m_lfac.getExp(phi_val_ref));
          } else if (phi_val_ref->isRef()) {
            if (phi_val_ref->isVar()) {
	      Region rgn_phi = getRegion(m_mem, m_func_regions, m_params, phi, phi);
	      Region rgn_phi_v = getRegion(m_mem, m_func_regions, m_params, phi, v);
	      // rgn_phi and rgn_phi_v should be same region
	      if (!rgn_phi.isUnknown() && !rgn_phi_v.isUnknown()) {
		m_bb.gep_ref(lhs, mkCrabRegion(rgn_phi),
			     phi_val_ref->getVar(), mkCrabRegion(rgn_phi_v));
	      }
            } else {
              m_bb.assume_ref(ref_cst_t::mk_null(lhs));
            }
          } else {
            /* unreachable*/
          }
        } else {
          // we can be here if the incoming value is a bignum and we
          // don't allow bignums.
          m_bb.havoc(lhs);
        }
      }
    }
  }
};

// Class that adds Crab instructions that initialize memory.
class MemoryInitializer {
  crabLitFactory &m_lfac;
  HeapAbstraction &m_mem;
  RegionSet &m_func_regions;
  const DataLayout &m_dl;
  const CrabBuilderParams &m_params;

  // Function where array stores will be added
  Function &m_fun;
  // Crab basic block where array stores will be added.
  basic_block_t &m_bb;
  // Used to mark the first array store as strong update.
  RegionSet m_initialized_arrays;
  // Maximum number of array stores
  unsigned m_max_stores;

public:
  MemoryInitializer(crabLitFactory &lfac, HeapAbstraction &mem,
		    RegionSet &func_regions, const DataLayout &dl,
                    const CrabBuilderParams &params, Function &fun,
                    basic_block_t &bb, unsigned max_stores = -1)

    : m_lfac(lfac), m_mem(mem), m_func_regions(func_regions),
      m_dl(dl), m_params(params),
      m_fun(fun), m_bb(bb), m_max_stores(max_stores) {}

  void InitGlobalMemory(Value &Base, Constant &C, unsigned offset) {
    if (isa<ConstantPointerNull>(C) || isa<ConstantFP>(C) ||
        isa<UndefValue>(C)) {
      // ignore these cases
    } else if (const ConstantDataSequential *CDS =
                   dyn_cast<ConstantDataSequential>(&C)) {
      // ignore C strings
      if (!(CDS->isString() || CDS->isCString())) {
        Type *IndexedType = CDS->getType()->getElementType();
        unsigned ElemOffset = clam::storageSize(IndexedType, m_dl);
        for (unsigned i = 0, e = CDS->getNumElements(); i < e; ++i) {
          InitGlobalMemory(Base, *(CDS->getElementAsConstant(i)),
                           offset + (i * ElemOffset));
        }
      }
    } else if (const ConstantVector *CP = dyn_cast<ConstantVector>(&C)) {
      Type *IndexedType = CP->getType()->getElementType();
      unsigned ElemOffset = clam::storageSize(IndexedType, m_dl);
      for (unsigned i = 0, e = CP->getNumOperands(); i < e; ++i) {
        InitGlobalMemory(Base, *(CP->getOperand(i)), offset + (i * ElemOffset));
      }
    } else if (ConstantArray *CPA = dyn_cast<ConstantArray>(&C)) {
      Type *IndexedType = CPA->getType()->getElementType();
      unsigned ElemOffset = clam::storageSize(IndexedType, m_dl);
      for (unsigned i = 0, e = CPA->getNumOperands(); i < e; ++i) {
        InitGlobalMemory(Base, *(CPA->getOperand(i)),
                         offset + (i * ElemOffset));
      }
    } else if (ConstantStruct *CPS = dyn_cast<ConstantStruct>(&C)) {
      const StructLayout *SL =
          m_dl.getStructLayout(cast<StructType>(CPS->getType()));
      for (unsigned i = 0, e = CPS->getNumOperands(); i < e; ++i) {
        InitGlobalMemory(Base, *(CPS->getOperand(i)),
                         offset + SL->getElementOffset(i));
      }
    } else if (isa<ConstantAggregateZero>(C)) {
      InitZeroInitializer(Base, *(C.getType()), offset);
    } else if (ConstantInt *CI = dyn_cast<ConstantInt>(&C)) {
      InitInteger(Base, *CI, offset);
    }
  }

  void InitZeroInitializer(Value &Base, Type &Ty, unsigned offset) {
    if (IntegerType *ITy = dyn_cast<IntegerType>(&Ty)) {
      ConstantInt *Zero = ConstantInt::get(ITy, 0);
      InitInteger(Base, *Zero, offset);
    } else if (const StructType *STy = dyn_cast<StructType>(&Ty)) {
      unsigned accumulator = 0;
      for (unsigned i = 0; i < STy->getNumElements(); ++i) {
        Type *ETy = STy->getElementType(i);
        InitZeroInitializer(Base, *ETy, offset + accumulator);
        accumulator += clam::storageSize(ETy, m_dl);
      }
    } else if (ArrayType *ATy = dyn_cast<ArrayType>(&Ty)) {
      Type *IndexedType = ATy->getElementType();
      for (unsigned i = 0; i < ATy->getNumElements(); ++i) {
        InitZeroInitializer(Base, *IndexedType,
                            offset +
                                (i * clam::storageSize(IndexedType, m_dl)));
      }
    } else {
      // ignore the rest of types
    }
  }

  void InitInteger(Value &Base, ConstantInt &Val, unsigned offset) {
    Region rgn;
    if (m_mem.getClassId() == HeapAbstraction::ClassId::SEA_DSA) {
      SeaDsaHeapAbstraction *seaDsaHeapAbs =
          static_cast<SeaDsaHeapAbstraction *>(&m_mem);
      rgn = seaDsaHeapAbs->getRegion(m_fun, Base, offset, *(Val.getType()));
      m_func_regions.insert(rgn);
    }
    
    if (!rgn.isUnknown()) {
      crab_lit_ref_t val_ref = m_lfac.getLit(Val);
      assert(val_ref);

      bool changed = false;
      if (getSingletonValue(rgn, m_params.lower_singleton_aliases)) {
	// Promote the global to an integer/boolean scalar
	var_t a = m_lfac.mkArraySingletonVar(rgn);
	if (isInteger(Val.getType())) {
	  assert(!val_ref->isVar() && val_ref->isInt());
	  m_bb.assign(a, m_lfac.getIntCst(val_ref));
	  changed = true;
	} else if (isBool(Val.getType())) {
	  assert(!val_ref->isVar() && val_ref->isBool());
	  m_bb.bool_assign(a, m_lfac.isBoolTrue(val_ref)
			   ? lin_cst_t::get_true()
			   : lin_cst_t::get_false());
	  changed = true;
	} 
      }

      if (changed) {
	return;
      }
      
      if (m_params.precision_level == CrabBuilderPrecision::SINGLETON_MEM &&
	  rgn.getRegionInfo().containScalar()) {
	uint64_t elem_size = clam::storageSize(Val.getType(), m_dl);
	assert(elem_size > 0);
	bool first = m_initialized_arrays.insert(rgn).second;
	number_t val(0);
	if (val_ref->isInt()) {
	  val = m_lfac.getIntCst(val_ref);
	} else if (val_ref->isBool() && m_lfac.isBoolTrue(val_ref)) {
	  val = number_t(1);
	}
	m_bb.array_store(m_lfac.mkArrayVar(rgn), lin_exp_t(offset),
			 val, elem_size, first /*strong update*/);
      } else if (m_params.precision_level == CrabBuilderPrecision::MEM) {
	CLAM_WARNING("TODO implement global initializer if CrabBuilderPrecision::MEM");
      }
    }
  }
};

//! Translate the rest of instructions
class CrabInstVisitor : public InstVisitor<CrabInstVisitor> {
  crabLitFactory &m_lfac;
  HeapAbstraction &m_mem;
  const DataLayout *m_dl;
  const TargetLibraryInfo *m_tli;
  basic_block_t &m_bb;
  unsigned int m_object_id;
  bool m_has_seahorn_fail;
  const CrabBuilderParams &m_params;
  /****
   * Here state that must survive through future invocations to
   * CrabInstVisitor.
   ****/
  // Regions seen so far
  RegionSet &m_func_regions;
  // map gep to a crab variable
  DenseMap<const GetElementPtrInst *, var_t> &m_gep_map;
  // reverse **partial** map from Crab statements to LLVM instructions
  DenseMap<const statement_t *, const Instruction *> &m_rev_map;
  // HACK: to perform strong updates with SINGLETON_MEMORY
  RegionSet &m_regions_with_store;

  unsigned fieldOffset(const StructType *t, unsigned field) const;
  uint64_t storageSize(const Type *t) const;
  /*
   *  Special function to return an unconstrained array index
   *  variable. This is used when we cannot statically know the
   *  integer offset of a pointer with respect to its memory object.
   */
  var_t getUnconstrainedArrayIdxVar(llvm_variable_factory &vfac,
                                    unsigned bitwidth);
  /* Evaluate the offset of an object pointed to by v statically */
  Optional<z_number> evalOffset(Value &v, LLVMContext &ctx);
  /*
   * Try extra a Crab arithmetic offset from load or store pointer
   * operand.
   */
  lin_exp_t inferArrayIndex(Value *v, LLVMContext &ctx, Region reg,
                            llvm_variable_factory &vfac);
  unsigned getMaxBitWidthFromGepIndexes(GetElementPtrInst &I);
  /*
   *  Insert key-value in the reverse map but only if no CFG
   *  simplifications enabled
   */
  void insertRevMap(const statement_t *s, Instruction &inst);  
  /*  Return true if all uses of V are non-trackable memory accesses.
   *  Useful to avoid translating bitcode that won't have any effect
   *  anyway.
   */
  bool AllUsesAreNonTrackMem(Value *V) const;

  /* Most of the translation work happens in these methods */
  void doBinOp(unsigned op, var_t lhs, lin_exp_t op1, lin_exp_t op2);
  void doArithmetic(crab_lit_ref_t ref, BinaryOperator &i);
  var_t doBoolLogicOp(Instruction::BinaryOps op, crab_lit_ref_t ref,
                      const Value &v1, const Value &v2);
  void doIntLogicOp(crab_lit_ref_t ref, BinaryOperator &i);
  void doAllocFn(Instruction &I);
  void doMemIntrinsic(MemIntrinsic &I);
  void doVerifierCall(CallInst &I);
  void doGep(GetElementPtrInst &I, unsigned bitwidth,
             var_t lhs, llvm::Optional<var_t> base);
  void StoreIntoSingletonMem(StoreInst &I, var_t array_var, crab_lit_ref_t val,
			     Region reg);
  void LoadFromSingletonMem(LoadInst &I, var_t lhs, var_t rhs, Region rhs_region);
  void doCallSite(CallInst &CI);
public:
  CrabInstVisitor(
      crabLitFactory &lfac, HeapAbstraction &mem, 
      const DataLayout *dl, const TargetLibraryInfo *tli, basic_block_t &bb,
      const CrabBuilderParams &params,
      RegionSet &func_regions,
      llvm::DenseMap<const statement_t *, const llvm::Instruction *> &rev_map,
      RegionSet &regions_with_store,
      DenseMap<const GetElementPtrInst *, var_t> &gep_map);

  bool hasSeaHornFail() { return m_has_seahorn_fail; }

  /// skip PHI nodes (processed elsewhere)
  void visitPHINode(PHINode &I) {}

  /// skip BranchInst (processed elsewhere)
  void visitBranchInst(BranchInst &I) {}

  /// skip SwitchInst (processed elsewhere)
  void visitSwitchInst(SwitchInst &I) {}

  /// skip ReturnInst (processed elsewhere)
  void visitReturnInst(ReturnInst &I) {}

  void visitCmpInst(CmpInst &I);
  void visitBinaryOperator(BinaryOperator &I);
  void visitCastInst(CastInst &I);
  void visitSelectInst(SelectInst &I);
  void visitGetElementPtrInst(GetElementPtrInst &I);
  void visitStoreInst(StoreInst &I);
  void visitLoadInst(LoadInst &I);
  void visitAllocaInst(AllocaInst &I);
  void visitCallInst(CallInst &I);
  void visitUnreachableInst(UnreachableInst &I);
  /// base case. if all else fails.
  void visitInstruction(Instruction &I);
}; // end class

CrabInstVisitor::CrabInstVisitor(
  crabLitFactory &lfac, HeapAbstraction &mem, 
  const DataLayout *dl, const TargetLibraryInfo *tli, basic_block_t &bb,
  const CrabBuilderParams &params,
  RegionSet &func_regions,
  llvm::DenseMap<const statement_t *, const llvm::Instruction *> &rev_map,
  RegionSet &regions_with_store,
  DenseMap<const GetElementPtrInst *, var_t> &gep_map)
  : m_lfac(lfac), m_mem(mem), m_dl(dl), m_tli(tli), m_bb(bb),
    m_object_id(0), m_has_seahorn_fail(false), m_params(params),
    m_func_regions(func_regions),
    m_gep_map(gep_map), m_rev_map(rev_map), m_regions_with_store(regions_with_store) {}
  
unsigned CrabInstVisitor::fieldOffset(const StructType *t,
                                      unsigned field) const {
  return m_dl->getStructLayout(const_cast<StructType *>(t))
      ->getElementOffset(field);
}

uint64_t CrabInstVisitor::storageSize(const Type *t) const {
  return clam::storageSize(t, *m_dl);
}

var_t CrabInstVisitor::getUnconstrainedArrayIdxVar(llvm_variable_factory &vfac,
                                                   unsigned bitwidth) {
  #if 0
  static var_t v(vfac.get(), crab::INT_TYPE, bitwidth);
  m_bb.havoc(v);
  #else
  var_t v(vfac.get(), crab::INT_TYPE, bitwidth);
  #endif
  return v;
}

unsigned CrabInstVisitor::getMaxBitWidthFromGepIndexes(GetElementPtrInst &I) {
  unsigned bitwidth = 0;
  for (unsigned i = 1, e = I.getNumOperands(); i < e; ++i) {
    if (IntegerType *ITy = cast<IntegerType>(I.getOperand(i)->getType())) {
      bitwidth = std::max(bitwidth, ITy->getBitWidth());
    } else {
      CLAM_ERROR("Expected gep instruction only with integer indexes: ", I);
    }
  }
  if (bitwidth == 0) {
    CLAM_ERROR("Unexpected gep instruction without indexes: ", I);
  }
  return bitwidth;
}

Optional<z_number> CrabInstVisitor::evalOffset(Value &v, LLVMContext &ctx) {
  llvm::ObjectSizeOpts Opts;
  Opts.RoundToAlign = true;
  Opts.EvalMode = llvm::ObjectSizeOpts::Mode::Max;
  ObjectSizeOffsetVisitor OSOV(*m_dl, m_tli, ctx, Opts);
  auto sizeOffset = OSOV.compute(&v);
  if (OSOV.knownOffset(sizeOffset)) {
    const int64_t offset = sizeOffset.second.getSExtValue();
    return z_number(offset);
  }
  return llvm::None;
}

// This method is key for precision of memory operations if tracked
// precision is SINGLETON_MEM.
//
// v is a pointer operand. It tries to figure out its numerical
// offset. If it can it returns an unconstrained variable.
lin_exp_t CrabInstVisitor::inferArrayIndex(Value *v, LLVMContext &ctx,
                                           Region reg,
                                           llvm_variable_factory &vfac) {
  auto offsetOpt = evalOffset(*v, ctx);
  if (offsetOpt.hasValue()) {
    // we were able to get the offset statically
    return offsetOpt.getValue();
  } else {
    if (const GetElementPtrInst *GEPI = dyn_cast<GetElementPtrInst>(v)) {
      auto it = m_gep_map.find(GEPI);
      if (it == m_gep_map.end()) {
        if (!reg.isUnknown()) {
          // This is unexpected so we print a warning
          CLAM_WARNING("Could not find shadow gep variable for " << *GEPI);
        }
        return getUnconstrainedArrayIdxVar(vfac, 32);
      } else {
        return it->second;
      }
    } else if (const Argument *Arg = dyn_cast<Argument>(v)) {
      CRAB_LOG("cfg-gep",
	       const Function &F = *(Arg->getParent());
	       CLAM_WARNING("cannot infer statically base address of formal input "
			    << *Arg << " at function " << F.getName()));
      return getUnconstrainedArrayIdxVar(vfac, 32);
    } else {
      CRAB_LOG("cfg-gep",
               CLAM_WARNING("cannot infer statically base address of  " << *v));
      return getUnconstrainedArrayIdxVar(vfac, 32);
    }
  }
}

void CrabInstVisitor::insertRevMap(const statement_t *s, Instruction &inst) {
  if (!m_params.simplify) {
    m_rev_map.insert({s, &inst});
  }
}

bool CrabInstVisitor::AllUsesAreNonTrackMem(Value *V) const {
  // XXX: not sure if we should strip pointers here
  V = V->stripPointerCasts();
  for (auto &U : V->uses()) {
    if (StoreInst *SI = dyn_cast<StoreInst>(U.getUser())) {
      if (isa<Instruction>(V)) {
        if (getRegion(m_mem, m_func_regions, m_params, *SI, *SI->getPointerOperand())
                .isUnknown() &&
            (!SI->getValueOperand()->getType()->isPointerTy() ||
             getRegion(m_mem, m_func_regions, m_params, *SI, *SI->getValueOperand())
                 .isUnknown()))
          continue;
      }
      return false;
    } else if (LoadInst *LI = dyn_cast<LoadInst>(U.getUser())) {
      if (Instruction *I = dyn_cast<Instruction>(V)) {
        if (getRegion(m_mem, m_func_regions, m_params, *LI, *LI->getPointerOperand())
                .isUnknown() &&
            (!I->getType()->isPointerTy() ||
             getRegion(m_mem, m_func_regions, m_params, *LI, *LI).isUnknown()))
          continue;
      }
      return false;
    } else if (CallInst *CI = dyn_cast<CallInst>(U.getUser())) {
      CallSite CS(CI);
      Function *callee = CS.getCalledFunction();
      if (callee && (callee->getName().startswith("llvm.dbg") ||
                     callee->getName().startswith("shadow.mem")))
        continue;
      else // conservatively return false
        return false;
    } else
      return false;
  }
  return true;
}

void CrabInstVisitor::doBinOp(unsigned op, var_t lhs, lin_exp_t op1,
                              lin_exp_t op2) {
  switch (op) {
  case BinaryOperator::Add:
    if (op1.get_variable() && op2.get_variable())
      m_bb.add(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.add(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::Sub:
    if (op1.get_variable() && op2.get_variable())
      m_bb.sub(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.sub(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::Mul:
    if (op1.get_variable() && op2.get_variable())
      m_bb.mul(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.mul(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::SDiv:
    if (op1.get_variable() && op2.get_variable())
      m_bb.div(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.div(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::UDiv:
    if (op1.get_variable() && op2.get_variable())
      m_bb.udiv(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.udiv(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::SRem:
    if (op1.get_variable() && op2.get_variable())
      m_bb.rem(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.rem(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::URem:
    if (op1.get_variable() && op2.get_variable())
      m_bb.urem(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.urem(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::And:
    if (op1.get_variable() && op2.get_variable())
      m_bb.bitwise_and(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.bitwise_and(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::Or:
    if (op1.get_variable() && op2.get_variable())
      m_bb.bitwise_or(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.bitwise_or(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::Xor:
    if (op1.get_variable() && op2.get_variable())
      m_bb.bitwise_xor(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.bitwise_xor(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::Shl:
    if (op1.get_variable() && op2.get_variable())
      m_bb.shl(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.shl(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::AShr:
    if (op1.get_variable() && op2.get_variable())
      m_bb.ashr(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.ashr(lhs, (*op1.get_variable()), op2.constant());
    return;
  case BinaryOperator::LShr:
    if (op1.get_variable() && op2.get_variable())
      m_bb.lshr(lhs, (*op1.get_variable()), (*op2.get_variable()));
    else if (op1.get_variable() && op2.is_constant())
      m_bb.lshr(lhs, (*op1.get_variable()), op2.constant());
    return;
  default:;
    ;
  }
  CLAM_ERROR("unexpected problem with binary operator");
}

void CrabInstVisitor::doArithmetic(crab_lit_ref_t ref, BinaryOperator &i) {
  if (!ref || !ref->isVar() || !(ref->isInt())) {
    CLAM_ERROR("lhs of arithmetic operation must be an integer");
  }
  var_t lhs = ref->getVar();

  const Value &v1 = *i.getOperand(0);
  const Value &v2 = *i.getOperand(1);

  crab_lit_ref_t ref1 = m_lfac.getLit(v1);
  if (!ref1 || !(ref1->isInt())) {
    havoc(lhs, m_bb, m_params.include_useless_havoc);
    return;
  }

  crab_lit_ref_t ref2 = m_lfac.getLit(v2);
  if (!ref2 || !(ref2->isInt())) {
    havoc(lhs, m_bb, m_params.include_useless_havoc);
    return;
  }

  lin_exp_t op1 = m_lfac.getExp(ref1);
  lin_exp_t op2 = m_lfac.getExp(ref2);

  if (op1.is_constant() && op2.is_constant()) {
    number_t n1 = op1.constant();
    number_t n2 = op2.constant();
    switch (i.getOpcode()) {
    case BinaryOperator::Add: // m_bb.assign(lhs, n1+n2); break;
    case BinaryOperator::Sub: // m_bb.assign(lhs, n1-n2); break;
    case BinaryOperator::Mul: // m_bb.assign(lhs, n1*n2); break;
    case BinaryOperator::SDiv:
    case BinaryOperator::UDiv:
    case BinaryOperator::SRem:
    case BinaryOperator::URem:
    case BinaryOperator::Shl:
    case BinaryOperator::AShr:
    case BinaryOperator::LShr: {
      var_t t1 = m_lfac.mkIntVar(i.getType()->getIntegerBitWidth());
      var_t t2 = m_lfac.mkIntVar(i.getType()->getIntegerBitWidth());
      m_bb.assign(t1, n1);
      m_bb.assign(t2, n2);
      doBinOp(i.getOpcode(), lhs, t1, t2);
    } break;
    default:
      // this should not happen
      CLAM_ERROR("unexpected instruction");
    }
    return;
  }

  switch (i.getOpcode()) {
  case BinaryOperator::Add:
  case BinaryOperator::Sub:
  case BinaryOperator::Mul:
  case BinaryOperator::SDiv:
  case BinaryOperator::UDiv:
  case BinaryOperator::SRem:
  case BinaryOperator::URem:
  case BinaryOperator::Shl:
  case BinaryOperator::AShr:
  case BinaryOperator::LShr:
    if (op1.is_constant()) {
      // Crab cfg does not support arithmetic operations between a
      // constant and variable.
      var_t t = m_lfac.mkIntVar(i.getType()->getIntegerBitWidth());
      m_bb.assign(t, op1.constant());
      doBinOp(i.getOpcode(), lhs, t, op2);
    } else {
      doBinOp(i.getOpcode(), lhs, op1, op2);
    }
    break;
  default:
    // this should not happen
    CLAM_ERROR("unexpected instruction");
  }
}

var_t CrabInstVisitor::doBoolLogicOp(Instruction::BinaryOps op,
                                     /* ref can be null */
                                     crab_lit_ref_t ref, const Value &v1,
                                     const Value &v2) {

  if (ref && !(ref->isBool())) {
    CLAM_ERROR("lhs of arithmetic operation must be an Boolean");
  }

  var_t lhs = (ref ? ref->getVar() : m_lfac.mkBoolVar());

  crab_lit_ref_t b1 = m_lfac.getLit(v1);
  if (!b1 || !(b1->isBool())) {
    havoc(lhs, m_bb, m_params.include_useless_havoc);
    return lhs;
  }

  crab_lit_ref_t b2 = m_lfac.getLit(v2);
  if (!b2 || !(b2->isBool())) {
    havoc(lhs, m_bb, m_params.include_useless_havoc);
    return lhs;
  }

  switch (op) {
  case BinaryOperator::And:
    if (b1->isVar() && b2->isVar()) {
      m_bb.bool_and(lhs, b1->getVar(), b2->getVar());
    } else if (!b1->isVar() && !b2->isVar()) {
      m_bb.bool_assign(lhs, m_lfac.isBoolTrue(b1) && m_lfac.isBoolTrue(b2)
                                ? lin_cst_t::get_true()
                                : lin_cst_t::get_false());
    } else if (m_lfac.isBoolFalse(b1) || m_lfac.isBoolFalse(b2)) {
      m_bb.bool_assign(lhs, lin_cst_t::get_false());
    } else if (m_lfac.isBoolTrue(b1)) {
      m_bb.bool_assign(lhs, b2->getVar());
    } else if (m_lfac.isBoolTrue(b2)) {
      m_bb.bool_assign(lhs, b1->getVar());
    } else {
      CLAM_ERROR("unexpected uncovered case in doBoolLogicOp And");
    }
    break;
  case BinaryOperator::Or:
    if (b1->isVar() && b2->isVar()) {
      m_bb.bool_or(lhs, b1->getVar(), b2->getVar());
    } else if (!b1->isVar() && !b2->isVar()) {
      m_bb.bool_assign(lhs, m_lfac.isBoolTrue(b1) || m_lfac.isBoolTrue(b2)
                                ? lin_cst_t::get_true()
                                : lin_cst_t::get_false());
    } else if (m_lfac.isBoolTrue(b1) || m_lfac.isBoolTrue(b2)) {
      m_bb.bool_assign(lhs, lin_cst_t::get_true());
    } else if (m_lfac.isBoolFalse(b1)) {
      m_bb.bool_assign(lhs, b2->getVar());
    } else if (m_lfac.isBoolFalse(b2)) {
      m_bb.bool_assign(lhs, b1->getVar());
    } else {
      CLAM_ERROR("unexpected uncovered case in doBoolLogicOp Or");
    }
    break;
  case BinaryOperator::Xor:
    if (b1->isVar() && b2->isVar()) {
      m_bb.bool_xor(lhs, b1->getVar(), b2->getVar());
    } else if (!b1->isVar() && !b2->isVar()) {
      m_bb.bool_assign(lhs,
                       (((m_lfac.isBoolTrue(b1) && m_lfac.isBoolFalse(b2)) ||
                         (m_lfac.isBoolFalse(b1) && m_lfac.isBoolTrue(b2)))
                            ? lin_cst_t::get_true()
                            : lin_cst_t::get_false()));
    } else if (m_lfac.isBoolTrue(b1)) {
      m_bb.bool_assign(lhs, b2->getVar(), true /*negate rhs*/);
    } else if (m_lfac.isBoolFalse(b1)) {
      m_bb.bool_assign(lhs, b2->getVar());
    } else if (m_lfac.isBoolTrue(b2)) {
      m_bb.bool_assign(lhs, b1->getVar(), true /*negate rhs*/);
    } else if (m_lfac.isBoolFalse(b2)) {
      m_bb.bool_assign(lhs, b1->getVar());
    } else {
      CLAM_ERROR("unexpected uncovered case in doBoolLogicOp Xor");
    }
    break;
  default:
    CLAM_WARNING("translation skipped bool logic operation at line "
                 << __LINE__);
    havoc(lhs, m_bb, m_params.include_useless_havoc);
  }
  return lhs;
}

void CrabInstVisitor::doIntLogicOp(crab_lit_ref_t ref, BinaryOperator &i) {
  assert(ref && ref->isVar());

  if (!(ref->isInt())) {
    CLAM_ERROR("lhs of bitwise operation must be an integer");
  }
  var_t lhs = ref->getVar();

  const Value &v1 = *i.getOperand(0);
  const Value &v2 = *i.getOperand(1);

  crab_lit_ref_t ref1 = m_lfac.getLit(v1);
  if (!ref1 || !(ref1->isInt())) {
    havoc(lhs, m_bb, m_params.include_useless_havoc);
    return;
  }

  crab_lit_ref_t ref2 = m_lfac.getLit(v2);
  if (!ref2 || !(ref2->isInt())) {
    havoc(lhs, m_bb, m_params.include_useless_havoc);
    return;
  }

  lin_exp_t op1 = m_lfac.getExp(ref1);
  lin_exp_t op2 = m_lfac.getExp(ref2);

  switch (i.getOpcode()) {
  case BinaryOperator::And:
  case BinaryOperator::Or:
  case BinaryOperator::Xor:
    doBinOp(i.getOpcode(), lhs, op1, op2);
    break;
  default:
    CLAM_WARNING("translation skipped " << i << " at line " << __LINE__);
    havoc(lhs, m_bb, m_params.include_useless_havoc);
  }
}

/* special functions for verification */
void CrabInstVisitor::doVerifierCall(CallInst &I) {
  CallSite CS(&I);

  const Value *calleeV = CS.getCalledValue();
  const Function *callee = dyn_cast<Function>(calleeV->stripPointerCasts());
  if (!callee)
    return;

  if (isErrorFn(*callee)) {
    m_bb.assertion(lin_cst_t::get_false(), getDebugLoc(&I));
    return;
  }

  if (isSeaHornFail(*callee)) {
    // when seahorn inserts a call to "seahorn.fail" means that
    // the program is safe iff the function cannot return.  Note
    // that we cannot add "assert(false)" in the current
    // block. Instead, we need to check whether the exit block of
    // the function is reachable or not.
    m_has_seahorn_fail = true;
    return;
  }

  if (!isAssertFn(*callee) && !isAssumeFn(*callee) && !isNotAssumeFn(*callee))
    return;

  Value *cond = CS.getArgument(0);

  if (!isTracked(*cond, m_params))
    return;

  if (ConstantInt *CI = dyn_cast<ConstantInt>(cond)) {
    // -- cond is a constant
    bool is_bignum;
    z_number cond_val = getIntConstant(CI, m_params, is_bignum);
    if (!is_bignum) {
      if (cond_val > 0) {
        if (isAssertFn(*callee) || isAssumeFn(*callee)) {
          // do nothing
        } else {
          assert(isNotAssumeFn(*callee));
          m_bb.assume(lin_cst_t::get_false());
        }
      } else {
        if (isNotAssumeFn(*callee)) {
          // do nothing
        } else if (isAssumeFn(*callee)) {
          m_bb.assume(lin_cst_t::get_false());
        } else {
          assert(isAssertFn(*callee));
          m_bb.assertion(lin_cst_t::get_false(), getDebugLoc(&I));
        }
      }
    }
  } else {
    crab_lit_ref_t cond_ref = m_lfac.getLit(*cond);
    assert(cond_ref->isVar());
    var_t v = cond_ref->getVar();
    // -- cond is variable
    if (cond_ref->isBool()) {
      if (isNotAssumeFn(*callee))
        m_bb.bool_not_assume(v);
      else if (isAssumeFn(*callee))
        m_bb.bool_assume(v);
      else {
        assert(isAssertFn(*callee));
        m_bb.bool_assert(v, getDebugLoc(&I));
      }
    } else if (cond_ref->isInt()) {

      ZExtInst *ZEI = dyn_cast<ZExtInst>(cond);
      if (ZEI && ZEI->getSrcTy()->isIntegerTy(1)) {
        /* Special case to replace this pattern:
             y:i32 = zext x:i1 to i32
             assume (y>=1);
           with
             bool_assume(x);
             This can help boolean/numerical propagation in the crab domains.
        */
        cond_ref = m_lfac.getLit(*(ZEI->getOperand(0)));
        assert(cond_ref->isVar()); // boolean variable
        v = cond_ref->getVar();
        if (isNotAssumeFn(*callee)) {
          m_bb.bool_not_assume(v);
        } else if (isAssumeFn(*callee)) {
          m_bb.bool_assume(v);
        } else {
          assert(isAssertFn(*callee));
          m_bb.bool_assert(v, getDebugLoc(&I));
        }
      } else {
        if (isNotAssumeFn(*callee)) {
          m_bb.assume(v <= number_t(0));
        } else if (isAssumeFn(*callee)) {
          m_bb.assume(v >= number_t(1));
        } else {
          assert(isAssertFn(*callee));
          m_bb.assertion(v >= number_t(1), getDebugLoc(&I));
        }
      }
    }
  }
}


/// I is already translated if it is the condition of a branch or
/// a select's condition.  Here we cover cases where I is an
/// operand of other instructions.
void CrabInstVisitor::visitCmpInst(CmpInst &I) {

  if (!isTracked(I, m_params))
    return;

  crab_lit_ref_t ref = m_lfac.getLit(I);
  assert(ref->isVar());

  if (isReference(*I.getOperand(0), m_params) &&
      isReference(*I.getOperand(1), m_params)) {

    if (!AllUsesAreBrInst(I)) {
      CLAM_WARNING("translation skipped comparison between pointers");
      havoc(ref->getVar(), m_bb, m_params.include_useless_havoc);
    }
    return;
  }

  // make sure we only translate if both operands are integers or booleans
  if (!I.getOperand(0)->getType()->isIntegerTy() ||
      !I.getOperand(1)->getType()->isIntegerTy()) {
    havoc(ref->getVar(), m_bb, m_params.include_useless_havoc);
    return;
  }

  const Value &v0 = *(I.getOperand(0));
  const Value &v1 = *(I.getOperand(1));

  if (isBool(v0) && isBool(v1)) {
    // we lower it here
    if (I.getPredicate() == CmpInst::ICMP_EQ) { // eq <-> not xor
      var_t tmp = doBoolLogicOp(BinaryOperator::Xor, nullptr, v0, v1);
      m_bb.bool_assign(ref->getVar(), tmp, true);      // not(tmp)
    } else if (I.getPredicate() == CmpInst::ICMP_NE) { // ne <-> xor
      doBoolLogicOp(BinaryOperator::Xor, ref, v0, v1);
    } else {
      CLAM_WARNING("translation skipped " << I << " at line " << __LINE__);
    }
  } else {
    assert(isInteger(v0) && isInteger(v1));
    if (AllUsesAreBrOrIntSelectCondInst(I)) {
      // do nothing: already lowered elsewhere
    } else {
      cmpInstToCrabBool(I, m_lfac, m_bb);
    }
  }
}

void CrabInstVisitor::visitBinaryOperator(BinaryOperator &I) {
  if (!isTracked(I, m_params))
    return;

  crab_lit_ref_t ref = m_lfac.getLit(I);
  if (!ref || !(ref->isVar())) {
    CLAM_ERROR("unexpected lhs of binary operator");
  }

  switch (I.getOpcode()) {
  case BinaryOperator::Add:
  case BinaryOperator::Sub:
  case BinaryOperator::Mul:
  case BinaryOperator::SDiv:
  case BinaryOperator::UDiv:
  case BinaryOperator::SRem:
  case BinaryOperator::URem:
  case BinaryOperator::Shl:
  case BinaryOperator::AShr:
  case BinaryOperator::LShr:
    doArithmetic(ref, I);
    break;
  case BinaryOperator::And:
  case BinaryOperator::Or:
  case BinaryOperator::Xor:
    if (isBool(I))
      doBoolLogicOp(I.getOpcode(), ref, *I.getOperand(0), *I.getOperand(1));
    else
      doIntLogicOp(ref, I);
    break;
  default:
    havoc(ref->getVar(), m_bb, m_params.include_useless_havoc);
  }
}

void CrabInstVisitor::visitCastInst(CastInst &I) {
  if (!isTracked(I, m_params))
    return;

  if (AllUsesAreNonTrackMem(&I) || AllUsesAreIndirectCalls(I)) {
    return;
  }

  if (isa<ZExtInst>(I) && I.getSrcTy()->isIntegerTy(1) &&
      AllUsesAreVerifierCalls(I)) {
    /*
       y:i32 = zext x:i1 to i32
       assume (y>=1);
    */
    return;
  }

  crab_lit_ref_t dst = m_lfac.getLit(I);
  assert(dst && dst->isVar());
  crab_lit_ref_t src = m_lfac.getLit(*(I.getOperand(0)));
  if (!src) {
    havoc(dst->getVar(), m_bb, m_params.include_useless_havoc);
    return;
  }

  // -- INTEGER OR BOOLEAN CAST
  if (I.isIntegerCast()) {
    if (I.getSrcTy() == I.getDestTy()) {
      // assume the frontend removes useless casts.
      CLAM_WARNING("translation does not support non-op integer casts");
      havoc(dst->getVar(), m_bb, m_params.include_useless_havoc);
    } else {
      if (!src->isVar()) {
        // We store the constant into a variable
        if (src->isBool()) {
          var_t tmp = m_lfac.mkBoolVar();
          m_bb.bool_assign(tmp, m_lfac.isBoolTrue(src)
                                    ? lin_cst_t::get_true()
                                    : lin_cst_t::get_false());
          if (isa<SExtInst>(I)) {
            m_bb.sext(tmp, dst->getVar());
          } else if (isa<ZExtInst>(I)) {
            m_bb.zext(tmp, dst->getVar());
          } else {
            CLAM_ERROR("unexpected cast operation on Booleans");
          }
        } else if (src->isInt()) {
          var_t tmp =
              m_lfac.mkIntVar(I.getOperand(0)->getType()->getIntegerBitWidth());
          m_bb.assign(tmp, m_lfac.getIntCst(src));
          if (isa<SExtInst>(I)) {
            m_bb.sext(tmp, dst->getVar());
          } else if (isa<ZExtInst>(I)) {
            m_bb.zext(tmp, dst->getVar());
          } else if (isa<TruncInst>(I)) {
            m_bb.truncate(tmp, dst->getVar());
          } else {
            CLAM_ERROR("unexpected cast operation");
          }
        } else {
          CLAM_ERROR("unexpected cast operand type");
        }
      } else {
        if (isa<SExtInst>(I)) {
          m_bb.sext(src->getVar(), dst->getVar());
        } else if (isa<ZExtInst>(I)) {
          m_bb.zext(src->getVar(), dst->getVar());
        } else if (isa<TruncInst>(I)) {
          m_bb.truncate(src->getVar(), dst->getVar());
        } else {
          CLAM_ERROR("unexpected cast operation");
        }
      }
    }
    return;
  }

  // -- POINTER CAST
  if (isa<IntToPtrInst>(I) || isa<PtrToIntInst>(I) || isa<BitCastInst>(I)) {
    if (isa<PtrToIntInst>(I)) {
      // CLAM_WARNING("translation skipped pointer to integer cast");
    } else if (isa<IntToPtrInst>(I)) {
      // CLAM_WARNING("translation skipped integer to pointer cast");
    } else if (isa<BitCastInst>(I) && isReference(*I.getOperand(0), m_params)) {

      if (src->isRef()) {
        if (m_lfac.isRefNull(src)) {
          m_bb.assume_ref(ref_cst_t::mk_null(dst->getVar()));
	  return;
        } else {
          assert(src->isVar());
	  Region rgn_src = getRegion(m_mem, m_func_regions, m_params, I, *(I.getOperand(0)));
	  Region rgn_dst = getRegion(m_mem, m_func_regions, m_params, I, I);
	  // rgn_src should be the same than rgn_dst
	  if (!rgn_src.isUnknown() && !rgn_dst.isUnknown()) {
	    m_bb.gep_ref(dst->getVar(), mkCrabRegion(rgn_dst),
			 src->getVar(), mkCrabRegion(rgn_src));
	    return;
	  }
        }
      }
      CLAM_WARNING("translation skipped " << I << " at line " << __LINE__);
    }
  }
  havoc(dst->getVar(), m_bb, m_params.include_useless_havoc);
}

// Analysis of select instructions is cumbersome since it requires
// a sequence of assume and join operations. Moreover, if many
// select instructions appear in the same block its analysis can
// be very inefficient due to the high number of joins.
//
// If possible the simplest solution is to get rid of select
// instructions. This can be done by adding option
// --lower-select. This option will remove select instructions at
// the expense of adding new basic blocks although hopefully the
// llvm frontend will simplify them. If this is not possible or
// undesirable then we try to deal with the select instruction
// here.
void CrabInstVisitor::visitSelectInst(SelectInst &I) {
  if (!isTracked(I, m_params))
    return;

  crab_lit_ref_t lhs = m_lfac.getLit(I);
  assert(lhs && lhs->isVar());

  if (isReference(I, m_params)) {
    // We don't even bother with pointers
    CLAM_WARNING("skipped " << I << "\n"
                            << "Enable --lower-select.");
    havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
    return;
  }

  Value &cond = *I.getCondition();
  crab_lit_ref_t c = m_lfac.getLit(cond);
  assert(c);
  crab_lit_ref_t op1 = m_lfac.getLit(*I.getTrueValue());
  assert(op1);
  crab_lit_ref_t op2 = m_lfac.getLit(*I.getFalseValue());
  assert(op2);

  if (isBool(I)) {
    // --- All operands are BOOL
    if (!op1->isBool()) {
      havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      return;
    }
    if (!op2->isBool()) {
      havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      return;
    }

    // -- simple cases first: we know the condition is either true or false
    if (ConstantInt *ci = dyn_cast<ConstantInt>(&cond)) {
      if (ci->isOne()) {
        if (!op1->isVar()) {
          m_bb.bool_assign(lhs->getVar(),
                           (m_lfac.isBoolTrue(op1) ? lin_cst_t::get_true()
                                                   : lin_cst_t::get_false()));
        } else {
          m_bb.bool_assign(lhs->getVar(), op1->getVar());
        }
      } else {
        if (!ci->isZero())
          CLAM_ERROR("unexpected select condition");
        if (!op2->isVar()) {
          m_bb.bool_assign(lhs->getVar(),
                           (m_lfac.isBoolTrue(op2) ? lin_cst_t::get_true()
                                                   : lin_cst_t::get_false()));
        } else {
          m_bb.bool_assign(lhs->getVar(), op2->getVar());
        }
      }
      return;
    }

    assert(c->isVar());

    // -- general case: we don't know whether condition is true or not.
    if (!op1->isVar() && !op2->isVar()) {
      var_t tt_v = m_lfac.mkBoolVar();
      var_t ff_v = m_lfac.mkBoolVar();
      m_bb.bool_assign(tt_v, (m_lfac.isBoolTrue(op1) ? lin_cst_t::get_true()
                                                     : lin_cst_t::get_false()));
      m_bb.bool_assign(ff_v, (m_lfac.isBoolTrue(op2) ? lin_cst_t::get_true()
                                                     : lin_cst_t::get_false()));
      m_bb.bool_select(lhs->getVar(), c->getVar(), tt_v, ff_v);
    } else if (!op1->isVar()) {
      var_t tt_v = m_lfac.mkBoolVar();
      m_bb.bool_assign(tt_v, (m_lfac.isBoolTrue(op1) ? lin_cst_t::get_true()
                                                     : lin_cst_t::get_false()));
      m_bb.bool_select(lhs->getVar(), c->getVar(), tt_v, op2->getVar());
    } else if (!op2->isVar()) {
      var_t ff_v = m_lfac.mkBoolVar();
      m_bb.bool_assign(ff_v, (m_lfac.isBoolTrue(op2) ? lin_cst_t::get_true()
                                                     : lin_cst_t::get_false()));
      m_bb.bool_select(lhs->getVar(), c->getVar(), op1->getVar(), ff_v);
    } else {
      m_bb.bool_select(lhs->getVar(), c->getVar(), op1->getVar(),
                       op1->getVar());
    }
  } else if (isInteger(I)) {

    // --- All operands except the condition are INTEGERS
    if (!op1->isInt()) {
      havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      return;
    }
    if (!op2->isInt()) {
      havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      return;
    }

    lin_exp_t e1 = m_lfac.getExp(op1);
    lin_exp_t e2 = m_lfac.getExp(op2);

    // -- simple cases first: we know the condition is either true or false
    if (ConstantInt *ci = dyn_cast<ConstantInt>(&cond)) {
      if (ci->isOne()) {
        m_bb.assign(lhs->getVar(), e1);
      } else {
        if (!ci->isZero())
          CLAM_ERROR("Unexpected select condition");
        m_bb.assign(lhs->getVar(), e2);
      }
      return;
    }

    assert(c->isVar());

    // -- general case: we don't know whether the condition is true or not
    if (CmpInst *CI = dyn_cast<CmpInst>(&cond)) {
      if (auto cst_opt = cmpInstToCrabInt(*CI, m_lfac)) {
        m_bb.select(lhs->getVar(), *cst_opt, e1, e2);
        return;
      }
    }

// The condition is a boolean but neither select or
// bool_select are the right choice. The latter is only when
// all operands are booleans. The former will have this form
// (select (x:= cond >=1 ? e1: e2). This will be propagated
// only to numerical domain which doesn't know anything about
// cond. One solution is to zext cond to an integer. But maybe
// another solution is to allow select to be a variable rather
// than constraint.
#if 1
    var_t icond = m_lfac.mkIntVar(8 /*any bitwdith >1*/);
    m_bb.zext(c->getVar(), icond);
    m_bb.select(lhs->getVar(), icond, e1, e2);
#else
    CLAM_WARNING(
        "skipped "
        << I << "\n"
        << "Crab select does not support natively boolean conditions.\n"
        << "Meanwhile, enable --lower-select or --crab-bool-as-int");
    havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
#endif
  }
}

  
/* malloc-like functions */
void CrabInstVisitor::doAllocFn(Instruction &I) {

  if (!I.getType()->isVoidTy()) {
    crab_lit_ref_t ref = m_lfac.getLit(I);
    assert(ref->isVar());
    if (isReference(I, m_params)) {
      Region rgn = getRegion(m_mem, m_func_regions, m_params, I, I);
      if (!rgn.isUnknown()) {
	m_bb.make_ref(ref->getVar(), mkCrabRegion(rgn));
      }
    } else if (isTracked(I, m_params)) {
      // -- havoc return value
      havoc(ref->getVar(), m_bb, m_params.include_useless_havoc);
    }
  }
}

/* memcpy/memmove/memset functions */
void CrabInstVisitor::doMemIntrinsic(MemIntrinsic &I) {
  CLAM_WARNING("Skipped memory intrinsics " << I);
}
  
//
// - If precision level is CrabBuilderPrecision::MEM then GEP it is
//   translated as a Crab reference statement.
//
// - If precision level is CrabBuilderPrecision::SINGLETON_MEM then
//   GEP it is translated as a sequence of Crab arithmetic statements
//   to be used by a Crab array statement. If base.hasValue() is false
//   then the base pointer of GEP is zero.
void CrabInstVisitor::doGep(GetElementPtrInst &I, 
                            unsigned max_index_bitwidth, var_t lhs,
                            llvm::Optional<var_t> base) {
  assert(lhs.get_type() == INT_TYPE || lhs.get_type() == REF_TYPE);
  assert(max_index_bitwidth == lhs.get_bitwidth());
  assert(!base.hasValue() ||
         (lhs.get_bitwidth() == base.getValue().get_bitwidth()));

  // -- All Gep operands should have the same region
  Region rgn = getRegion(m_mem, m_func_regions, m_params, I, I);
  if (rgn.isUnknown()) {
    return;
  }
  crab::memory_region crab_rgn = mkCrabRegion(rgn);
  
  // -- translation if the GEP offset is constant
  unsigned bitwidth = m_dl->getPointerTypeSizeInBits(I.getType());
  APInt offset(bitwidth, 0);
  if (I.accumulateConstantOffset(*m_dl, offset)) {
    bool is_bignum = false;
    z_number o(toZNumber(offset, m_params, is_bignum));
    if (is_bignum) {
      m_bb.havoc(lhs);
    } else {
      if (lhs.get_type() == REF_TYPE) {
	// reference statement
	if (!base.hasValue()) {
	  CRAB_ERROR("doGEP expects a base pointer");
	}
	m_bb.gep_ref(lhs, crab_rgn, base.getValue(), crab_rgn, lin_exp_t(o));
	CRAB_LOG("cfg-gep", crab::outs() << "-- " << lhs << ":=" << base.getValue() << "+"
	                        	 << o << "\n");
      } else if (lhs.get_type() == INT_TYPE) {
        // pure arithmetic
        if (base.hasValue()) {
          m_bb.assign(lhs, base.getValue() + lin_exp_t(o));
          CRAB_LOG("cfg-gep",
                   crab::outs() << "-- " << lhs << ":i" << lhs.get_bitwidth()
                                << ":=" << base.getValue() << "+" << o << "\n");
        } else {
          m_bb.assign(lhs, lin_exp_t(o));
          CRAB_LOG("cfg-gep", crab::outs()
                                  << "-- " << lhs << ":i" << lhs.get_bitwidth()
                                  << ":=" << o << "\n");
        }
      }
    }
    return;
  }

  // -- translation if symbolic GEP offset
  // If here, we know that there is at least one non-zero, symbolic index.
  bool already_assigned = false;
  for (auto GTI = gep_type_begin(&I), GTE = gep_type_end(&I); GTI != GTE;
       ++GTI) {
    if (const StructType *st = GTI.getStructTypeOrNull()) {
      if (const ConstantInt *ci =
              dyn_cast<const ConstantInt>(GTI.getOperand())) {
        number_t offset(fieldOffset(st, ci->getZExtValue()));
        if (lhs.get_type() == REF_TYPE) {
	  // reference statement
	  if (!(already_assigned || base.hasValue())) {
	    CRAB_ERROR("doGEP expects a base pointer");
	  }
	  m_bb.gep_ref(lhs, crab_rgn,
		       (!already_assigned) ? base.getValue() : lhs, crab_rgn, offset);
          CRAB_LOG(
              "cfg-gep",
		   if (!already_assigned) {
		     crab::outs() << lhs << "=" << base.getValue() << "+" << offset << "\n";
              } else { crab::outs() << lhs << "+=" << offset << "\n"; });
        } else if (lhs.get_type() == INT_TYPE){
          // pure arithmetic
          if (!already_assigned) {
            if (base.hasValue()) {
              m_bb.assign(lhs, base.getValue() + offset);
              CRAB_LOG("cfg-gep", crab::outs() << "-- " << lhs << ":i"
                                               << lhs.get_bitwidth() << "="
                                               << base.getValue() << "+"
                                               << offset << "\n");
            } else {
              m_bb.assign(lhs, offset);
              CRAB_LOG("cfg-gep", crab::outs() << "-- " << lhs << ":i"
                                               << lhs.get_bitwidth() << "="
                                               << offset << "\n");
            }
          } else {
            m_bb.add(lhs, lhs, offset);
            CRAB_LOG("cfg-gep", crab::outs() << "-- " << lhs << ":i"
                                             << lhs.get_bitwidth()
                                             << "+=" << offset << "\n");
          }
        }
        already_assigned = true;
      } else {
        CLAM_ERROR("GEP index expected only to be an integer");
      }
    } else {
      // otherwise we have a sequential type like an array or vector.
      // Multiply the index by the size of the indexed type.
      if (const ConstantInt *ci =
              dyn_cast<const ConstantInt>(GTI.getOperand())) {
        if (ci->isZero())
          continue;
      }

      crab_lit_ref_t idx = m_lfac.getLit(*GTI.getOperand());
      if (!idx || !idx->isInt()) {
        CLAM_ERROR("unexpected GEP index");
      }

      // Signed-extension of the index if needed.
      llvm::Optional<lin_exp_t> offsetOpt = llvm::None;
      auto Iidx = std::static_pointer_cast<const crabIntLit>(idx);
      if (Iidx->isVar()) {
        unsigned w = Iidx->getVar().get_bitwidth();
        assert(w <= max_index_bitwidth);
        if (w < max_index_bitwidth) {
          var_t sext_idx = m_lfac.mkIntVar(max_index_bitwidth);
          m_bb.sext(Iidx->getVar(), sext_idx);
          offsetOpt = (sext_idx * number_t(storageSize(GTI.getIndexedType())));
        }
      }
      if (!offsetOpt.hasValue()) {
        offsetOpt =
            (m_lfac.getExp(idx) * number_t(storageSize(GTI.getIndexedType())));
      }

      lin_exp_t offset = offsetOpt.getValue();
      if (lhs.get_type() == REF_TYPE) {
	// reference statement
	if (!(already_assigned || base.hasValue())) {
	  CRAB_ERROR("doGEP expects a base pointer");
	}
	m_bb.gep_ref(lhs, crab_rgn,
		     (!already_assigned) ? base.getValue() : lhs, crab_rgn, offset);	
        CRAB_LOG(
            "cfg-gep",
		 if (!already_assigned) {
		   crab::outs() << lhs << "=" << base.getValue() << "+" << offset << "\n";
            } else { crab::outs() << lhs << "+=" << offset << "\n"; });
      } else if (lhs.get_type() == INT_TYPE) {
        // pure arithmetic
        if (!already_assigned) {
          if (base.hasValue()) {
            m_bb.assign(lhs, base.getValue() + offset);
            CRAB_LOG("cfg-gep", crab::outs() << "-- " << lhs << ":i"
                                             << lhs.get_bitwidth() << "="
                                             << base.getValue() << "+" << offset
                                             << "\n");
          } else {
            m_bb.assign(lhs, offset);
            CRAB_LOG("cfg-gep", crab::outs() << "-- " << lhs << ":i"
                                             << lhs.get_bitwidth() << "="
                                             << offset << "\n");
          }
        } else {
          m_bb.assign(lhs, lhs + offset);
          CRAB_LOG("cfg-gep", crab::outs()
                                  << "-- " << lhs << ":i" << lhs.get_bitwidth()
                                  << "+=" << offset << "\n");
        }
      }
      already_assigned = true;
    }
  }
}

/*
 The offset computation is translated to a sequence of Crab linear
 arithmetic operations. In Crab, an arithmetic operation is strongly
 typed which means that all operands must have same bitwidth. GEP
 indexes can have any bitwidth (although fields of struct and vector
 must use always 32 bits). Our solution is to use the same bitwidth
 for all variables in the whole sequence of arithmetic
 operations. This bitwidth is the maximum bitwidth among all GEP
 indices' bitwidths. To generate well-typed Crab operations some of
 the variables are signed extended. If we wouldn't choose the maximum
 bitwidth then we would have truncate operations which can
 overflow. We try to avoid that.
*/
void CrabInstVisitor::visitGetElementPtrInst(GetElementPtrInst &I) {
  if (m_params.precision_level == CrabBuilderPrecision::NUM) {
    return;
  }
  
  CRAB_LOG("cfg-gep", llvm::errs() << "Translating " << I << "\n");
  Region rgn = getRegion(m_mem, m_func_regions, m_params, I, I);
  if (rgn.isUnknown()) {
    // we don't keep track of the memory region, we bail out ...
    return;
  }
  if (getSingletonValue(rgn, m_params.lower_singleton_aliases)) {
    // the memory region is a non-sequence singleton so we bail out
    // because it will translated somewhere else (e.g., next Load or
    // Store)
    return;
  }

  if (evalOffset(I, I.getContext()).hasValue()) {      
    // Skip the GEP instruction because the offset is a known
    // constant. The next Load or Store will call evalOffset again
    // to obtain the constant index.
    return;
  }

  unsigned bitwidth = getMaxBitWidthFromGepIndexes(I);
  if (m_params.precision_level == CrabBuilderPrecision::MEM) {
    crab_lit_ref_t lhs = m_lfac.getLit(I);
    assert(lhs && lhs->isVar());
    crab_lit_ref_t ptr = m_lfac.getLit(*I.getPointerOperand());
    if (!ptr) {
      havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      return;
    }
    if (m_lfac.isRefNull(ptr)) {
      CLAM_WARNING(I << " doing pointer arithmetic with null pointer.");
      havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      return;
    }
    assert(ptr->isVar());
    // Translate GEP as a sequence of arithmetic operations + assign
    doGep(I, bitwidth, lhs->getVar(), ptr->getVar()/*base address*/);
  } else if (m_params.precision_level == CrabBuilderPrecision::SINGLETON_MEM) {
    Value *Ptr = I.getPointerOperand();
    Ptr = Ptr->stripPointerCasts();
    const bool isSingletonMemory = isa<AllocaInst>(Ptr) || isa<GlobalVariable>(Ptr);
    if (isSingletonMemory) {
      llvm::Optional<var_t> baseAddress = llvm::None; /* i.e., zero base address*/
      // Check if the GEP pointer operand is the result of another GEP
      // instruction
      if (GetElementPtrInst *GepPtr = dyn_cast<GetElementPtrInst>(Ptr)) {
        auto it = m_gep_map.find(GepPtr);
        if (it != m_gep_map.end()) {
          var_t gepVar = it->second;
          // Adjust the bitwdith of gepVar wrt bitwitdh which is the
          // max bitwidth of all the GEP's indices.
          if (bitwidth < gepVar.get_bitwidth()) {
            bitwidth = gepVar.get_bitwidth();
          } else if (bitwidth > gepVar.get_bitwidth()) {
            var_t sextGepVar = m_lfac.mkIntVar(bitwidth);
            m_bb.sext(gepVar, sextGepVar);
            baseAddress = sextGepVar;
          }
	}
      }
      var_t shadowV(m_lfac.getVFac().get(), crab::INT_TYPE, bitwidth);
      m_gep_map.insert(std::make_pair(&I, shadowV));
      // Translate GEP as a sequence of arithmetic operations +
      // gep_ref
      doGep(I, bitwidth, shadowV, baseAddress);
    } else {
      // We give up and translate the GEP to an unconstrained Crab
      // variable
      var_t shadowV = getUnconstrainedArrayIdxVar(m_lfac.getVFac(), bitwidth);
      CRAB_LOG("cfg-gep",
               CLAM_WARNING("cannot infer statically base address of  "
                            << *Ptr << " at function "
                            << I.getParent()->getParent()->getName()
                            << ".\nUsing unconstrained crab variable "
                            << shadowV.name().str()););
      m_gep_map.insert(std::make_pair(&I, shadowV));
    } 
  } else { /* do unreachable */
  }
}

/* Translate a StoreInt into a Crab array or assign statement */
void CrabInstVisitor::StoreIntoSingletonMem(StoreInst &I, var_t v,
					    crab_lit_ref_t val, Region reg) {
  assert(m_params.precision_level == CrabBuilderPrecision::SINGLETON_MEM);
  
  lin_exp_t idx = inferArrayIndex(I.getPointerOperand(), I.getContext(), reg,
				  m_lfac.getVFac());
  /**
   * We can help the array domain if we know already that
   * the array store is a strong update.
   **/
  bool is_uninit_region = m_regions_with_store.insert(reg).second;
  Function &func = *(I.getParent()->getParent());
  bool is_strong_update =
    reg.getSingleton() ||
    (func.getName() == "main" &&
     (&(func.getEntryBlock()) == I.getParent()) && is_uninit_region);

  Type *ty = I.getOperand(0)->getType();
  const statement_t *crab_stmt;
  if (val->isVar()) {
    var_t temp_v = val->getVar();
    // Due to heap abstraction imprecisions, it can happen that the
    // region's bitwidth is smaller than value's bitwidth.
    if (reg.getRegionInfo().getBitwidth() < val->getVar().get_bitwidth()) {
      temp_v = m_lfac.mkIntVar(reg.getRegionInfo().getBitwidth());
      // XXX: this truncate operation can overflow but the
      // store instruction does not overflow
      m_bb.truncate(val->getVar(), temp_v);
    }
    crab_stmt = m_bb.array_store(v, idx, temp_v,
				 m_dl->getTypeAllocSize(ty).getFixedSize(),
				 is_strong_update);
  } else {
    if (val->isInt()) {
      crab_stmt = m_bb.array_store(v, idx, m_lfac.getIntCst(val),
				   m_dl->getTypeAllocSize(ty).getFixedSize(),
				   is_strong_update);
    } else if (val->isBool()) {
      crab_stmt = m_bb.array_store(
		  v, idx, m_lfac.isBoolTrue(val) ? number_t(1) : number_t(0),
		  m_dl->getTypeAllocSize(ty).getFixedSize(), is_strong_update);
    } else { /* unreachable */
    }
  }
  
  if (crab_stmt) {
    insertRevMap(crab_stmt, I);
  }
}

void CrabInstVisitor::visitStoreInst(StoreInst &I) {
  /**
   * The LLVM store instruction will be translated to *either*: 
   * (a) crab array store if SINGLETON_MEM, or
   * (b) crab store_to_ref/store_to_arr_ref if MEM
   *
   * If SINGLETON_MEM then we only consider the cases where the stored
   * value is an integer or boolean.
   **/

  if (m_lfac.getTrack() == CrabBuilderPrecision::NUM) {
    return;
  }
  
  if (isa<ConstantExpr>(I.getPointerOperand()) ||
      isa<ConstantExpr>(I.getValueOperand())) {
    // We don't handle constant expressions.
    return;
  }
  
  Region rgn = getRegion(m_mem, m_func_regions, m_params, I, *I.getPointerOperand());
  if (rgn.isUnknown()) {
    // The Heap analysis is imprecise with the region so we bail out
    return;
  }

  crab_lit_ref_t ptr = m_lfac.getLit(*I.getPointerOperand());
  crab_lit_ref_t val = m_lfac.getLit(*I.getValueOperand());

  if (!ptr || !ptr->isRef()) {
    CLAM_ERROR("unexpected pointer operand of store instruction");
  }

  if (m_lfac.isRefNull(ptr)) {
    CLAM_WARNING(I << " is possibly dereferencing a null pointer. "
		   << "Ignoring the instruction");
    return;
  }
  
  if (!val) {
    // XXX: this can happen if we store a ptrtoint instruction
    // For simplicity, we don't deal with this case here and we
    // assume that the client must make sure that all constant
    // expressions are lowered.
    CLAM_ERROR("unexpected value operand of store instruction");
  }

  // -- Lower to a scalar operation if possible
  bool change = false;
  if (getSingletonValue(rgn, m_params.lower_singleton_aliases)) {
    // Promote the singleton global to an integer/boolean scalar
    var_t v = m_lfac.mkArraySingletonVar(rgn);
    if (isInteger(*I.getValueOperand())) {
      assert(val->isInt());
      m_bb.assign(v, m_lfac.getExp(val));
      change = true;
    } else if (isBool(*I.getValueOperand())) {
      assert(val->isBool());
      if (!val->isVar()) {
        m_bb.bool_assign(v, (m_lfac.isBoolTrue(val) ? lin_cst_t::get_true()
                                                    : lin_cst_t::get_false()));
      } else {
        m_bb.bool_assign(v, val->getVar(), false);
      }
      change = true;
    }
  }

  if (change) {
    return;
  }
  
  if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
    // -- value is an integer/bool -> add array statement
    StoreIntoSingletonMem(I, m_lfac.mkArrayVar(rgn), val, rgn);
  } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
    
    if (!rgn.getRegionInfo().isSequence()) {
      if (val->isVar()) {
	m_bb.store_to_ref(ptr->getVar(), mkCrabRegion(rgn), val->getVar());
      } else if (val->isBool()) {
	var_t normalized_val = m_lfac.mkBoolVar();
	m_bb.bool_assign(normalized_val, m_lfac.isBoolTrue(val)
			 ? lin_cst_t::get_true()
			 : lin_cst_t::get_false());
	m_bb.store_to_ref(ptr->getVar(), mkCrabRegion(rgn), normalized_val);	
      } else if (val->isInt()) {
	var_t normalized_val =
	  m_lfac.mkIntVar(I.getValueOperand()->getType()->getIntegerBitWidth());
	m_bb.assign(normalized_val, m_lfac.getIntCst(val));
	m_bb.store_to_ref(ptr->getVar(), mkCrabRegion(rgn), normalized_val);	
      } else if (val->isRef() && m_lfac.isRefNull(val)) {
	// TODO: we ignore for now the case if we store a null
	// pointer.  We should create a fresh reference and add a
	// constraint saying is equal to null.
	CLAM_WARNING("Missing a store of a null value " << I);
      } else { /* unreachable */
      } 
    } else {
      /// TODO: translate to store_to_arr_ref
      /// 
      /// The translation needs to add an ref_to_int statement if the
      /// stored value is a pointer.
      /// 
      /// uint64_t elem_size = clam::storageSize(I.getValueOperand()->getType(), *m_dl);
      /// assert(elem_size > 0);
      ///
      CLAM_WARNING("Missing a store to a sequence region " << I);
    }
  }
}

/*
 * Translate a LoadInst into a Crab array or assign statement.
 *
 * lhs_v and rhs_v are crab typed variables.
 * reg is the region associated with the load's pointer operand.
 */
void CrabInstVisitor::LoadFromSingletonMem(LoadInst &I, var_t lhs_v, var_t rhs_v, Region reg) {
  assert(m_params.precision_level == CrabBuilderPrecision::SINGLETON_MEM);
  
  var_t tmp = lhs_v;
  // Due to heap abstraction imprecisions, it can happen
  // that the region's bitwidth is smaller than lhs_v'
  // bitwidth.
  if (reg.getRegionInfo().getBitwidth() < lhs_v.get_bitwidth()) {
    lhs_v = m_lfac.mkIntVar(reg.getRegionInfo().getBitwidth());
  }
  lin_exp_t idx = inferArrayIndex(I.getPointerOperand(), I.getContext(), reg,
				  m_lfac.getVFac());
  auto const *crab_stmt =
    m_bb.array_load(lhs_v, rhs_v, idx, m_dl->getTypeAllocSize(I.getType()).getFixedSize());
  insertRevMap(crab_stmt, I);
  if (reg.getRegionInfo().getBitwidth() < lhs_v.get_bitwidth()) {
    // XXX: not sure if signed extension is correct.
    // Regions are signed-agnostic so dont know what is the
    // best choice here. Maybe if the regions' bitwidth is
    // different form lhs_v' bitwidth we should ignore the
    // load instruction.
    m_bb.sext(lhs_v, tmp);
  }
}

void CrabInstVisitor::visitLoadInst(LoadInst &I) {
  /*
    This case is symmetric to StoreInst.
   */

  if (!isTracked(I, m_params)) {
    return;
  }

  
  Region rgn = getRegion(m_mem, m_func_regions, m_params, I, *I.getPointerOperand());
  if (rgn.isUnknown()) {
    // The Heap analysis is imprecise with the region so we bail out
    return;
  }
  
  crab_lit_ref_t lhs = m_lfac.getLit(I);  
  if (!lhs || !lhs->isVar()) {
    CLAM_ERROR("unexpected lhs of load instruction");
  }

  if (isa<ConstantExpr>(I.getPointerOperand())) {
    // We don't handle constant expressions.
    havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
    return;
  }
  
  crab_lit_ref_t ptr = m_lfac.getLit(*I.getPointerOperand());
  if (!ptr || !ptr->isRef()) {
    CLAM_ERROR("unexpected pointer operand of load instruction");
  }
  if (m_lfac.isRefNull(ptr)) {
    CLAM_WARNING(I << " is possibly dereferencing a null pointer");
    havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
    return;
  }

  // -- Lower to a scalar operation if possible  
  bool change = false;
  if (getSingletonValue(rgn, m_params.lower_singleton_aliases)) {
    // Promote the global to an integer/boolean scalar
    if (isInteger(I)) {
      m_bb.assign(lhs->getVar(), m_lfac.mkArraySingletonVar(rgn));
      change = true;
    } else if (isBool(I)) {
      m_bb.bool_assign(lhs->getVar(), m_lfac.mkArraySingletonVar(rgn), false);
      change = true;
    } 
  }
  
  if (change) {
    return;
  }
  
  if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
    // -- lhs is an integer/bool -> add array statement
    LoadFromSingletonMem(I, lhs->getVar(), m_lfac.mkArrayVar(rgn), rgn);
    return;
  } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
    if (!rgn.getRegionInfo().isSequence()) {
      m_bb.load_from_ref(lhs->getVar(), ptr->getVar(), mkCrabRegion(rgn));
      return;
    } else {
      /// TODO: translate to load_from_arr_ref
      /// 
      /// The translation needs to add an int_to_ref statement if the
      /// loaded value is a pointer.
      
      /// uint64_t elem_size = clam::storageSize(I.getPointerOperand()->getType(), *m_dl);
      /// assert(elem_size > 0);
      CLAM_WARNING("Missing a load from a sequence region " << I);
    }
  }

  havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
}

void CrabInstVisitor::visitAllocaInst(AllocaInst &I) {
  Region rgn = getRegion(m_mem, m_func_regions, m_params, I, I);
  if (rgn.isUnknown()) {
    return;
  }
  
  if (isReference(I, m_params)) {
    crab_lit_ref_t lhs = m_lfac.getLit(I);
    assert(lhs && lhs->isVar());          
    m_bb.make_ref(lhs->getVar(), mkCrabRegion(rgn));
  } else if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM &&
	     rgn.getRegionInfo().containScalar()) {
    // Memory allocated in the stack is uninitialized.
    // 
    // We assume they are zero initialized so that Crab's array
    // smashing can infer something meaningful. Even Crab's array
    // adaptive domain may benefit from this in case an array is
    // initialized in a loop.
    // 
    // REVISIT/TODO: this can generate two consecutive array stores if
    // the alloca instruction is followed by an array store.  A better
    // solution for array adaptive is to unroll loops one iteration.
    Function *parentF = I.getParent()->getParent();
    MemoryInitializer MI(m_lfac, m_mem, m_func_regions, *m_dl, m_params, *parentF, m_bb);
    Type *ATy = I.getAllocatedType();
    MI.InitZeroInitializer(I, *ATy, 0);
  }
}

/**
 * Translate a LLVM callsite
 *     o := foo(i1,...,i_n)
 *
 * into a crab callsite or intrinsic
 *     (o,a_o1,...,a_om) := foo(i1,...,in,a_i1,...,a_in) where
 *
 *    - a_i1,...,a_in are read-only and modified regions by foo.
 *    - a_o1,...,a_om are modified and new regions created inside foo.
 *
 * The regions are actually translated to array or reference variables.
 **/
void CrabInstVisitor::doCallSite(CallInst &I) {
  CallSite CS(&I);
  const Function *calleeF = dyn_cast<Function>(CS.getCalledValue()->stripPointerCasts());
  assert(calleeF);  
  
  std::vector<var_t> inputs, outputs;

  // -- add the actual parameters of the llvm callsite: i1,...in.
  for (auto &a : llvm::make_range(CS.arg_begin(), CS.arg_end())) {
    Value *v = a.get();
    if (!isTracked(*v, m_params))
      continue;
    inputs.push_back(normalizeFuncParamOrRet(*v, m_bb, m_lfac));
  }

  // -- add the return value of the llvm calliste: o
  if (ShouldCallSiteReturn(I, m_params)) {
    if (DoesCallSiteReturn(I, m_params)) {
      crab_lit_ref_t ret = m_lfac.getLit(I);
      assert(ret && ret->isVar());
      outputs.push_back(ret->getVar());
    } else {
      // The callsite should return something to match with the
      // function signature but it doesn't: we create a fresh
      // return value.
      Type *RT = calleeF->getReturnType();
      if (isBool(RT)) {
        var_t fresh_ret = m_lfac.mkBoolVar();
        outputs.push_back(fresh_ret);
      } else if (isInteger(RT)) {
        unsigned bitwidth = RT->getIntegerBitWidth();
        var_t fresh_ret = m_lfac.mkIntVar(bitwidth);
        outputs.push_back(fresh_ret);
      } else if (isReference(RT, m_params)) {
        var_t fresh_ret = m_lfac.mkRefVar();
        outputs.push_back(fresh_ret);
      } else {
        // do nothing
      }
    }
  } else {
    if (DoesCallSiteReturn(I, m_params)) {
      // LLVM shouldn't allow this.
      CLAM_ERROR(
          "Unexpected type mismatch between callsite and function signature");
    }
  }

  // -- add the input and output parameters a_i1,...,a_in
  // -- and a_o1,...,a_om.
  RegionVec inRegions = getInputRegions(m_mem, m_params, I);
  RegionVec inOutRegions = getInputOutputRegions(m_mem, m_params, I);
  RegionVec outRegions = getOutputRegions(m_mem, m_params, I);
  
  CRAB_LOG("cfg-mem", llvm::errs()
	   << "Callsite " << I << "\n"
	   << "\tInput regions " << inRegions.size()
	   << ": " << inRegions << "\n"
	   << "\tInput/Output regions " << inOutRegions.size() << ": "
	   << inOutRegions << "\n"
	   << "\tOutput regions " << outRegions.size() << ": " << outRegions
	   << "\n");

  // -- add only read regions as input parameters
  for (auto rgn : inRegions) {
    if (getSingletonValue(rgn, m_params.lower_singleton_aliases)) {
      // Promote the global to a scalar
      inputs.push_back(m_lfac.mkArraySingletonVar(rgn));
    } else if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
      inputs.push_back(m_lfac.mkArrayVar(rgn));
    } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
      CLAM_WARNING("Missing read-only region " << rgn << " from callsite " << I);
    }
  }

  // -- add modified regions as both input and output parameters
  for (auto rgn : inOutRegions) {
    if (std::find(outRegions.begin(), outRegions.end(), rgn) != outRegions.end()) {
      continue;
    }
    bool lowerToScalar = getSingletonValue(rgn, m_params.lower_singleton_aliases);
    // -- input version
    if (lowerToScalar) {
      // Promote the global to a scalar
      inputs.push_back(m_lfac.mkArraySingletonVar(rgn));
    } else if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
      inputs.push_back(m_lfac.mkArrayVar(rgn));
    } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
      CLAM_WARNING("Missing modified region " << rgn << " from callsite " << I);      
    }
    
    // -- output version
    if (lowerToScalar) {
      // Promote the global to a scalar
      outputs.push_back(m_lfac.mkArraySingletonVar(rgn));
    } else if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
      outputs.push_back(m_lfac.mkArrayVar(rgn));
    } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
      // TODO
    }
  }
  
  // -- add more output parameters
  for (auto rgn : outRegions) {
    // New regions cannot be singletons so they are directly
    // translated as arrays
    if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {    
      outputs.push_back(m_lfac.mkArrayVar(rgn));
    } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
      CLAM_WARNING("Missing new region " << rgn << " from callsite " << I);            
    }
  }
  
  // -- Finally, add the callsite or crab intrinsic
  if (isCrabIntrinsic(*calleeF)) {
    m_bb.intrinsic(getCrabIntrinsicName(*calleeF), outputs, inputs);
  } else {
    m_bb.callsite(calleeF->getName().str(), outputs, inputs);
  }
}
    
void CrabInstVisitor::visitCallInst(CallInst &I) {
  CallSite CS(&I);
  const Value *calleeV = CS.getCalledValue();
  const Function *callee = dyn_cast<Function>(calleeV->stripPointerCasts());

  if (!callee) {
    if (I.isInlineAsm()) {
      // -- inline asm: do nothing
    } else {
      // -- unresolved indirect call
      CLAM_WARNING("skipped indirect call. "
                   << "Either --devirt-functions was not used or "
                   << "indirect call cannot be resolved.");

      if (DoesCallSiteReturn(I, m_params) &&
          ShouldCallSiteReturn(I, m_params)) {
        // havoc return value
        crab_lit_ref_t lhs = m_lfac.getLit(I);
        assert(lhs && lhs->isVar());
        havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      }
    }
    return;
  }

  if (callee->getName().startswith("shadow.mem") ||
      callee->getName().equals("seahorn.fn.enter")) {
    return;
  }

  if (isVerifierCall(*callee)) {
    doVerifierCall(I);
    return;
  }

  if (isAllocationFn(&I, m_tli)) {
    doAllocFn(I);
    return;
  }

  if (callee->isIntrinsic()) {
    if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(&I)) {
      doMemIntrinsic(*MI);
    } else {
      if (DoesCallSiteReturn(I, m_params) &&
          ShouldCallSiteReturn(I, m_params)) {
        // -- havoc return value of the intrinsics
        crab_lit_ref_t lhs = m_lfac.getLit(I);
        assert(lhs && lhs->isVar());
        havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
      }
    }
    return;
  }

  bool is_external = callee->isDeclaration() || callee->isVarArg() ||
                     !m_params.interprocedural;
  if (is_external && !isCrabIntrinsic(*callee)) {
    /**
     * If external or we don't perform inter-procedural reasoning
     * then we make sure all modified regions and return value of
     * the callsite are havoc'ed.
     **/

    // -- havoc return value
    if (DoesCallSiteReturn(I, m_params) && ShouldCallSiteReturn(I, m_params)) {
      crab_lit_ref_t lhs = m_lfac.getLit(I);
      assert(lhs && lhs->isVar());
      havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
    }
    // -- havoc all modified regions by the callee
    // Note that even if the code is not available for the callee, the
    // pointer analysis might be able to model its pointer semantics.
    RegionVec inOutRegions = getInputOutputRegions(m_mem, m_params, I);
    for (auto rgn : inOutRegions) {
      if (getSingletonValue(rgn, m_params.lower_singleton_aliases))
	m_bb.havoc(m_lfac.mkArraySingletonVar(rgn));
      else if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
	m_bb.havoc(m_lfac.mkArrayVar(rgn));
      } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
	CLAM_WARNING("TODO havoc " << rgn << " from callsite " << I);
      }
    }

    CLAM_WARNING(
        "Call to external function "
        << callee->getName() << ". "
        << "Havocing the return value and possibly its modified memory regions "
        << "if the pointer analysis models the external function");

    // XXX: if we return here we skip the callsite. This is fine
    //      unless there exists an analysis which cares about
    //      external calls.
    //
    //      Note: if we want to add the callsite make sure we add
    //      the prototype for the external function below.
    //
    return;
  }
  
  /* Translate the call to an internal function */
  doCallSite(I);
}

void CrabInstVisitor::visitUnreachableInst(UnreachableInst &I) {
  m_bb.unreachable();
}

/// base case. if all else fails.
void CrabInstVisitor::visitInstruction(Instruction &I) {
  if (!isTracked(I, m_params))
    return;
  CLAM_WARNING("Skipped " << I);
  crab_lit_ref_t lhs = m_lfac.getLit(I);
  if (lhs && lhs->isVar()) {
    havoc(lhs->getVar(), m_bb, m_params.include_useless_havoc);
  }
}

} // end namespace

namespace clam {

class CfgBuilderImpl {
public:
  CfgBuilderImpl(const llvm::Function &func, llvm_variable_factory &vfac,
                 HeapAbstraction &mem, 
                 llvm::TargetLibraryInfoWrapperPass *tli,
                 const CrabBuilderParams &params);

  void buildCfg();

  CfgBuilderImpl(const CfgBuilderImpl &o) = delete;

  CfgBuilderImpl &operator=(const CfgBuilderImpl &o) = delete;

  ~CfgBuilderImpl();

  // return crab control flow graph
  cfg_t &getCfg();

  // map a llvm basic block to a crab basic block label
  basic_block_label_t getCrabBasicBlock(const llvm::BasicBlock *bb) const;

  // map a llvm edge to a crab basic block label.
  // return nullptr if the edge is not translated to a crab basic block.
  const basic_block_label_t *
  getCrabBasicBlock(const llvm::BasicBlock *src,
                    const llvm::BasicBlock *dst) const;

  // Most crab statements have back pointers to LLVM operands so it
  // is always possible to find the corresponding LLVM
  // instruction. Array crab operations are an exception.
  //
  // This method maps an **array** crab statement to its
  // corresponding llvm instruction. Return null if the the array
  // instruction is not mapped to a LLVM instruction.
  const llvm::Instruction *getInstruction(const statement_t &s) const;

private:
  // map from a llvm basic block to a crab basic block id
  using node_to_crab_block_map_t =
      std::unordered_map<const llvm::BasicBlock *, basic_block_label_t>;

  struct pair_hash {
    template <typename T1, typename T2>
    std::size_t operator()(const std::pair<T1, T2> &p) const {
      std::size_t seed = 0;
      boost::hash_combine(seed, p.first);
      boost::hash_combine(seed, p.second);
      return seed;
    }
  };

  // map from a llvm edge to a crab basic block id
  using edge_to_crab_block_map_t = std::unordered_map<
      std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>,
      basic_block_label_t, pair_hash>;

  // keep track whether the crab CFG has been built
  bool m_is_cfg_built;
  // The function should be const because it's never modified.
  llvm::Function &m_func;
  // literal factory
  crabLitFactory m_lfac;
  // heap analysis for memory translation
  HeapAbstraction &m_mem;
  // the crab CFG
  std::unique_ptr<cfg_t> m_cfg;
  // generate unique identifiers for crab basic block ids
  unsigned int m_id;
  // map llvm CFG basic blocks to crab basic block ids
  node_to_crab_block_map_t m_node_to_crab_map;
  // map llvm CFG edges to crab basic block ids
  edge_to_crab_block_map_t m_edge_to_crab_map;
  // Memory regions accessed by m_func
  RegionSet m_func_regions;
  // map Crab statement to its corresponding LLVM instruction
  //
  // In most of the crab statements, their operands have back
  // pointers to their corresponding LLVM values. However, this is
  // not the case for array instructions. For those case, we keep
  // explicitly the reverse mapping.
  llvm::DenseMap<const statement_t *, const llvm::Instruction *> m_rev_map;
  // information about LLVM pointers
  const llvm::DataLayout *m_dl;
  llvm::TargetLibraryInfoWrapperPass *m_tli;
  // cfg builder parameters
  const CrabBuilderParams &m_params;

  /// Helpers for buildCfg

  // Given a llvm basic block return its corresponding crab basic block
  basic_block_t *lookup(const llvm::BasicBlock &bb) const;

  void addBlock(const llvm::BasicBlock &bb);

  void addEdge(const llvm::BasicBlock &src, const llvm::BasicBlock &target);

  basic_block_t *execEdge(const llvm::BasicBlock &src,
                          const llvm::BasicBlock &target);

  void addBlockInBetween(basic_block_t &src, basic_block_t &dst,
                         basic_block_t &between);

  void addFunctionDeclaration(llvm::Optional<var_t> ret_val);
  
  basic_block_label_t makeCrabBasicBlockLabel(const llvm::BasicBlock *bb);

  basic_block_label_t makeCrabBasicBlockLabel(const llvm::BasicBlock *src,
                                              const llvm::BasicBlock *dst);
}; // end class CfgBuilderImpl

CfgBuilderImpl::CfgBuilderImpl(const Function &func,
                               llvm_variable_factory &vfac,
                               HeapAbstraction &mem, 
                               TargetLibraryInfoWrapperPass *tli,
                               const CrabBuilderParams &params)
    : m_is_cfg_built(false),
      // HACK: it's safe to remove constness because we know that the
      // Builder never modifies the bitcode.
      m_func(const_cast<Function &>(func)), m_lfac(vfac, params), m_mem(mem),
      m_cfg(nullptr), m_id(0),
      m_dl(&(func.getParent()->getDataLayout())), m_tli(tli), m_params(params) {
  m_cfg =
      std::make_unique<cfg_t>(makeCrabBasicBlockLabel(&m_func.getEntryBlock()));
}

CfgBuilderImpl::~CfgBuilderImpl() {}

cfg_t &CfgBuilderImpl::getCfg() {
  // it won't build if already built
  buildCfg();
  return *m_cfg;
}

const llvm::Instruction *
CfgBuilderImpl::getInstruction(const statement_t &s) const {
  auto it = m_rev_map.find(&s);
  if (it != m_rev_map.end()) {
    return it->second;
  } else {
    return nullptr;
  }
}

basic_block_label_t
CfgBuilderImpl::getCrabBasicBlock(const BasicBlock *bb) const {
  auto it = m_node_to_crab_map.find(bb);
  if (it == m_node_to_crab_map.end()) {
    CLAM_ERROR("cannot map llvm basic block ", bb->getName(),
               " to crab basic block label");
  }
  return it->second;
}

const basic_block_label_t *
CfgBuilderImpl::getCrabBasicBlock(const BasicBlock *src,
                                  const BasicBlock *dst) const {
  auto it = m_edge_to_crab_map.find(std::make_pair(src, dst));
  if (it != m_edge_to_crab_map.end()) {
    return &(it->second);
  } else {
    return nullptr;
  }
}

// Given a llvm basic block return its corresponding crab basic block
basic_block_t *CfgBuilderImpl::lookup(const BasicBlock &bb) const {
  auto it = m_node_to_crab_map.find(&bb);
  if (it == m_node_to_crab_map.end()) {
    return nullptr;
  }
  return &(m_cfg->get_node(it->second));
}

void CfgBuilderImpl::addBlock(const BasicBlock &bb) {
  auto it = m_node_to_crab_map.find(&bb);
  if (it == m_node_to_crab_map.end()) {
    auto bb_label = makeCrabBasicBlockLabel(&bb);
    m_cfg->insert(bb_label);
  }
}

void CfgBuilderImpl::addEdge(const BasicBlock &src, const BasicBlock &dst) {
  basic_block_t *crab_src = lookup(src);
  basic_block_t *crab_dst = lookup(dst);
  assert(crab_src && crab_dst);
  *crab_src >> *crab_dst;
}

void CfgBuilderImpl::addBlockInBetween(basic_block_t &src, basic_block_t &dst,
                                       basic_block_t &bb) {
  src -= dst;
  src >> bb;
  bb >> dst;
}

basic_block_label_t
CfgBuilderImpl::makeCrabBasicBlockLabel(const BasicBlock *bb) {
  ++m_id;
  basic_block_label_t res(bb, m_id);
  m_node_to_crab_map.insert({bb, res});
  return res;
}

static std::string createBasicBlockName(unsigned id, std::string prefix = "") {
  if (prefix == "")
    prefix = std::string("__@bb_");
  std::string id_str = std::to_string(id);
  return prefix + id_str;
}

basic_block_label_t
CfgBuilderImpl::makeCrabBasicBlockLabel(const BasicBlock *src,
                                        const BasicBlock *dst) {
  ++m_id;
  std::string name = createBasicBlockName(m_id);
  basic_block_label_t res(src, dst, name, m_id);
  m_edge_to_crab_map.insert({{src, dst}, res});
  return res;
}

//! return the new block inserted between src and dest if any
basic_block_t *CfgBuilderImpl::execEdge(const BasicBlock &src,
                                        const BasicBlock &dst) {
  if (const BranchInst *br = dyn_cast<const BranchInst>(src.getTerminator())) {
    if (br->isConditional()) {
      basic_block_t *crab_src = lookup(src);
      basic_block_t *crab_dst = lookup(dst);
      assert(crab_src && crab_dst);

      // Create a new crab block that represents the LLVM edge
      auto bb_label = makeCrabBasicBlockLabel(&src, &dst);
      basic_block_t &bb = m_cfg->insert(bb_label);
      addBlockInBetween(*crab_src, *crab_dst, bb);

      // Populate the new crab block with an assume
      const Value &c = *br->getCondition();
      if (const ConstantInt *ci = dyn_cast<const ConstantInt>(&c)) {
        if ((ci->isOne() && br->getSuccessor(0) != &dst) ||
            (ci->isZero() && br->getSuccessor(1) != &dst)) {
          bb.unreachable();
        }
      } else if (const ConstantExpr *ce = dyn_cast<const ConstantExpr>(&c)) {
        CLAM_WARNING("Clam cfg builder skipped a branch condition with "
                     "constant expression");
      } else {
        bool isNegated = (br->getSuccessor(1) == &dst);
        bool lower_cond_as_bool = false;
        if (CmpInst *CI = dyn_cast<CmpInst>(&const_cast<Value &>(c))) {
          if (isBool(*(CI->getOperand(0))) && isBool(*(CI->getOperand(1)))) {
            lower_cond_as_bool = true;
          } else if (isInteger(*(CI->getOperand(0))) &&
                     isInteger(*(CI->getOperand(1)))) {
            auto cst_opt = cmpInstToCrabInt(*CI, m_lfac, isNegated);
            if (cst_opt.hasValue()) {
              bb.assume(cst_opt.getValue());
            }
          } else if (isReference(*(CI->getOperand(0)), m_params) &&
                     isReference(*(CI->getOperand(1)), m_params)) {
            auto cst_opt = cmpInstToCrabRef(*CI, m_lfac, isNegated);
            if (cst_opt.hasValue()) {
              bb.assume_ref(cst_opt.getValue());
            }
          }
          if (c.hasNUsesOrMore(2)) {
            // If I is used by another instruction apart from a
            // branch condition.
            lower_cond_as_bool = true;
          }
        } else {
          // If the boolean condition is passed directly (e.g.,
          // after optimization) as a function argument.
          lower_cond_as_bool = true;
        }

        if (lower_cond_as_bool) {
          crab_lit_ref_t lhs = m_lfac.getLit(c);
          assert(lhs && lhs->isVar());
          assert(lhs->isBool());
          if (isNegated)
            bb.bool_not_assume(lhs->getVar());
          else
            bb.bool_assume(lhs->getVar());
        }
      }
      return &bb;
    } else {
      // br is unconditional
      addEdge(src, dst);
    }
  } else if (const SwitchInst *SI = dyn_cast<SwitchInst>(src.getTerminator())) {
    // switch <value>, label <defaultdest> [ <val>, label <dest> ... ]
    //
    // TODO: we do not translate precisely switch instructions. We
    // simply add an edge from src to dest.

    // To be precise, we need to create a block between src and dest
    // and add the statement "assume(value == val)" if dest is not
    // the default block. For the default block, we need to add the
    // sequence:
    //      "assume(value != val1); ... ; assume(value != valk);"

    addEdge(src, dst);
  }
  return nullptr;
}

void CfgBuilderImpl::buildCfg() {
  if (m_is_cfg_built) {
    return;
  }
  m_is_cfg_built = true;
  crab::ScopedCrabStats __st__("CFG Construction");

  // Sanity check: pass NameValues must have been executed before
  bool res = checkAllDefinitionsHaveNames(m_func);
  if (!res) {
    CLAM_ERROR("All blocks and definitions must have a name");
  }

  // Create create basic block for each LLVM block
  for (auto &B : m_func) {
    addBlock(B);
  }

  // Lower global initializers into main
  if (m_func.getName().equals("main")) {
    basic_block_t &entry = m_cfg->get_node(m_cfg->entry());
    Module &M = *(m_func.getParent());
    for (GlobalVariable &gv : M.globals()) {
      if (gv.hasInitializer()) {
        MemoryInitializer MI(m_lfac, m_mem, m_func_regions, *m_dl, m_params, m_func, entry);
        MI.InitGlobalMemory(gv, *(gv.getInitializer()), 0);
      }
    }
  }

  basic_block_t *ret_block = nullptr;
  llvm::Optional<var_t> ret_val;
  bool has_seahorn_fail = false;
  // HACK: keep track of which regions have at least one store to it
  RegionSet regions_with_store;
  // Map GEP instruction to a Crab variable
  DenseMap<const GetElementPtrInst *, var_t> gep_map;

  const TargetLibraryInfo *tli = nullptr;
  if (m_tli)
    tli = &m_tli->getTLI(m_func);

  for (auto &B : m_func) {
    basic_block_t *bb = lookup(B);
    if (!bb)
      continue;

    // -- build a CFG block ignoring branches, phi-nodes, and return
    CrabInstVisitor v(m_lfac, m_mem, m_dl, tli, *bb, m_params,
		      m_func_regions, m_rev_map, regions_with_store, gep_map);
                      
    v.visit(B);
    // hook for seahorn
    has_seahorn_fail |= (v.hasSeaHornFail() && m_func.getName().equals("main"));

    if (ReturnInst *RI = dyn_cast<ReturnInst>(B.getTerminator())) {
      // -- process the exit block of the function and its returned value.
      if (ret_block) {
        // UnifyFunctionExitNodes ensures *at most* one return
        // instruction per function.
        CLAM_ERROR("UnifyFunctionExitNodes pass should be run first");
      }

      ret_block = bb;
      m_cfg->set_exit(ret_block->label());
      if (has_seahorn_fail) {
        ret_block->assertion(lin_cst_t::get_false(), getDebugLoc(RI));
      }
      if (m_params.interprocedural) {
        if (Value *RV = RI->getReturnValue()) {
          if (isTracked(*RV, m_params)) {
            ret_val = normalizeFuncParamOrRet(*RV, *ret_block, m_lfac);
            bb->ret(ret_val.getValue());
          }
        }
      }
    } else {
      // process the rest of basic blocks
      std::vector<const BasicBlock *> succs_vector(succs(B).begin(),
                                                   succs(B).end());
      // The default destination of a switch instruction does not
      // count as a successor but we want to consider it as a such.
      if (SwitchInst *SI = dyn_cast<SwitchInst>(B.getTerminator())) {
        succs_vector.push_back(SI->getDefaultDest());
      }
      for (const BasicBlock *dst : succs_vector) {
        // -- move branch condition in bb to a new block inserted
        //    between bb and dst
        basic_block_t *mid_bb = execEdge(B, *dst);

        // -- phi nodes in dst are translated into assignments in
        //    the predecessor
        CrabPhiVisitor v(m_lfac, m_mem, m_func_regions, *m_dl, (mid_bb ? *mid_bb : *bb), B, m_params);
        v.visit(const_cast<BasicBlock &>(*dst));
      }
    }
  }

  if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
    // Initialize regions
    basic_block_t *entry = lookup(m_func.getEntryBlock());
    auto inRegions = getInputRegions(m_mem, m_params, m_func);
    auto inOutRegions = getInputOutputRegions(m_mem, m_params, m_func);
    RegionSet reachRegions(inRegions.begin(), inRegions.end());
    reachRegions.insert(inOutRegions.begin(), inOutRegions.end());
    CRAB_LOG("cfg-mem", llvm::errs() << "Regions for " << m_func.getName() << "{";);
    for (auto rgn: m_func_regions) {
      CRAB_LOG("cfg-mem", llvm::errs() << rgn << ";";);
      if (reachRegions.count(rgn) <= 0) {
	entry->set_insert_point_front();	
	entry->region_init(mkCrabRegion(rgn));
      }
    }
    CRAB_LOG("cfg-mem", llvm::errs() << "}\n";);
  }
  
  /// Add function declaration only if inter-procedural translation
  if (m_params.interprocedural && !m_func.isVarArg()) {
    addFunctionDeclaration(ret_val);
  }

  if (m_cfg->has_exit()) {
    // -- Connect all sink blocks with an unreachable instruction to
    //    the exit block.  For a forward analysis this doesn't have
    //    any impact since unreachable becomes bottom anyway.
    //    However, a backward analysis starting with an invariant that
    //    says the exit is unreachable may incorrectly infer that the
    //    preconditions of the error states is false just because it
    //    never propagates backwards from these special sink blocks.
    basic_block_t &exit = m_cfg->get_node(m_cfg->exit());
    for (auto &B : m_func) {
      if (basic_block_t *b = lookup(B)) {
        if (b->label() == m_cfg->exit())
          continue;

        auto it_pair = b->next_blocks();
        if (it_pair.first == it_pair.second) {
          // block has no successors and it is not the exit block
          for (auto &I : B)
            if (isa<UnreachableInst>(I))
              *b >> exit;
        }
      }
    }
  } else {
    // We did not find an exit block yet:

    // (1) search for this pattern:
    //   entry: goto loop;
    //    loop: goto loop;
    BasicBlock &entry = m_func.getEntryBlock();
    auto entry_next = succs(entry);
    if (std::distance(entry_next.begin(), entry_next.end()) == 1) {
      const BasicBlock *succ = *(entry_next.begin());
      auto succ_next = succs(*succ);
      if (std::distance(succ_next.begin(), succ_next.end()) == 1) {
        if ((*(succ_next.begin())) == succ) {
          if (basic_block_t *exit = lookup(*succ)) {
            m_cfg->set_exit(exit->label());
          }
        }
      }
    }

    if (!m_cfg->has_exit()) {
      // (2) We check if there is a block with an unreachable
      // instruction. The pass UnifyFunctionExitNodes ensures that
      // there is at most one unreachable instruction.
      for (auto &B : m_func) {
        for (auto &I : B) {
          if (isa<UnreachableInst>(I)) {
            if (basic_block_t *b = lookup(B)) {
              m_cfg->set_exit(b->label());
              break;
            }
          }
        }
        if (m_cfg->has_exit()) {
          break;
        }
      }
    }

    if (!m_cfg->has_exit()) {
      // (3) Search for the first block without successors.
      for (auto &B : m_func) {
        if (basic_block_t *b = lookup(B)) {
          auto it_pair = b->next_blocks();
          if (it_pair.first == it_pair.second) {
            m_cfg->set_exit(b->label());
          }
        }
      }
    }
  }

  if (m_params.simplify) {
    // -- Remove dead statements generated by our translation
    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Started CFG dead code elimination\n";);
    cfg_ref_t cfg_ref(*m_cfg);
    crab::transforms::dead_code_elimination<cfg_ref_t> dce;
    dce.run(cfg_ref);
    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Finished CFG dead code elimination\n";);

    // -- Remove empty blocks after dce
    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Started CFG simplification\n";);
    m_cfg->simplify();
    CRAB_VERBOSE_IF(1, crab::get_msg_stream()
                           << "Finished CFG simplification\n";);
  }

  if (m_params.print_cfg) {
    crab::outs() << *m_cfg << "\n";
  }
  return;
}

/**
 * Translate LLVM function declaration
 *   o_ty foo (i1,...,in)
 *
 * into a crab function declaration
 *
 *   o, a_o1,...,a_om foo (i1,...,in,a_i1,...,a_in) where
 *
 *   - o is the **returned value** of the function (translation
 *     ensures there is always one return instruction and the
 *     returned value is a variable, i.e., cannot be a
 *     constant).
 *
 *   - a_i1,...,a_in are read-only and modified regions in function foo
 *
 *   - a_o1,....,a_om are modified and new regions created inside
 *     foo.
 *
 * It ensures that the set {a_i1,...,a_in} is disjoint from
 * {a_o1,....,a_om}, otherwise crab will complain.
 *
 *  The regions are actually translated to either array or reference
 *  crab variables.
 **/
void CfgBuilderImpl::addFunctionDeclaration(llvm::Optional<var_t> ret_val) {
  std::vector<var_t> inputs, outputs;
  basic_block_t &entry = m_cfg->get_node(m_cfg->entry());
  
  if (!ret_val.hasValue()) {
    // special case: function that does not return but in its
    // signature it has a return type. E.g., "int foo() {
    // unreachable; }"
    const Type &RT = *m_func.getReturnType();
    if (isTrackedType(RT, m_params)) {
      if (isBool(&RT)) {
	ret_val = m_lfac.mkBoolVar();
      } else if (isInteger(&RT)) {
	unsigned bitwidth = RT.getIntegerBitWidth();
	ret_val = m_lfac.mkIntVar(bitwidth);
      } else {
	assert(RT.isPointerTy());
	 ret_val = m_lfac.mkRefVar();
      }
    }
  }
  
  // -- add the returned value of the llvm function: o
  if (ret_val.hasValue()) {
    outputs.push_back(ret_val.getValue());
  }
  
  // -- add input parameters i1,...,in
  for (Value &arg : llvm::make_range(m_func.arg_begin(), m_func.arg_end())) {
    if (!isTracked(arg, m_params))
       continue;
    
    /** Make sure the assignments are inserted before a return
     *  instruction in case the entry and the exit blocks are the
     *  same.
     **/
    entry.set_insert_point_front();
    
    crab_lit_ref_t i = m_lfac.getLit(arg);
    assert(i && i->isVar());
    if (ret_val.hasValue() && i->getVar() == ret_val.getValue()) {
      // rename i to avoid having same name as output (crab requirement)
      if (i->isBool()) {
	var_t fresh_i = m_lfac.mkBoolVar();
	entry.bool_assign(i->getVar(), fresh_i);
	inputs.push_back(fresh_i);
      } else if (i->isInt()) {
	unsigned bitwidth = arg.getType()->getIntegerBitWidth();
	var_t fresh_i = m_lfac.mkIntVar(bitwidth);
	entry.assign(i->getVar(), fresh_i);
	inputs.push_back(fresh_i);
      } else if (i->isRef()) {
	Region arg_rgn = getRegion(m_mem, m_func_regions, m_params, m_func, arg);
	if (!arg_rgn.isUnknown()) {
	  var_t fresh_i = m_lfac.mkRefVar();
	  entry.gep_ref(i->getVar(), mkCrabRegion(arg_rgn),
			fresh_i, mkCrabRegion(arg_rgn));
	  inputs.push_back(fresh_i);
	}
      } else {
	CLAM_ERROR("unexpected function parameter type");
       }
    } else {
      inputs.push_back(i->getVar());
    }
  }
  
  if (!m_func.getName().equals("main")) {    
    // -- Purify the function
    RegionVec inRegions = getInputRegions(m_mem, m_params, m_func);
    RegionVec inOutRegions = getInputOutputRegions(m_mem, m_params, m_func);
    RegionVec outRegions = getOutputRegions(m_mem, m_params, m_func);
    
    CRAB_LOG("cfg-mem", llvm::errs() << "Function " << m_func.getName()
	     << "\n\tInput regions "
	     << inRegions.size() << ": " << inRegions
	     << "\n\tInput/Output regions " << inOutRegions.size()
	     << ": " << inOutRegions << "\n\tOutput regions "
	     << outRegions.size() << ": " << outRegions << "\n");
    
    // -- add only read regions as input parameters
    for (auto rgn : inRegions) {
      if (getSingletonValue(rgn, m_params.lower_singleton_aliases)) {
	// Promote the global to a scalar
	inputs.push_back(m_lfac.mkArraySingletonVar(rgn));
      } else if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
	inputs.push_back(m_lfac.mkArrayVar(rgn));
      } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
	CLAM_WARNING("Missing read-only region " << rgn << " from function declaration "
		     << m_func.getName());
      }
    }
      
    // -- add input/output parameters
    for (auto rgn : inOutRegions) {
      if (std::find(outRegions.begin(), outRegions.end(), rgn) != outRegions.end()) {
	continue;
      }
	
      // -- for each parameter `a` we create a fresh version `a_in`
      //    where `a_in` acts as the input version of the parameter
      //    and `a` is the output version.
      
      /** Added in the entry block of the function **/
      entry.set_insert_point_front();

      bool change = false;
      if (const Value *v =
	  getSingletonValue(rgn, m_params.lower_singleton_aliases)) {
	 
	if (rgn.getRegionInfo().containScalar()) {
	  change = true;
	  // Promote the global to a scalar
	  Type *ty = cast<PointerType>(v->getType())->getElementType();
	  var_t s = m_lfac.mkArraySingletonVar(rgn);
	  if (isInteger(ty)) {
	    var_t a_in = m_lfac.mkIntVar(ty->getIntegerBitWidth());
	    inputs.push_back(a_in);	  
	    entry.assign(s, a_in);
	  } else if (isBool(ty)) {
	    var_t a_in = m_lfac.mkBoolVar();
	    inputs.push_back(a_in);	  	  
	    entry.bool_assign(s, a_in, false);
	  }
	  // output version
	  outputs.push_back(m_lfac.mkArraySingletonVar(rgn));
	}
      }
      if (change) {
	continue;
      }
      
      if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
	switch (rgn.getRegionInfo().getType()) {
	case region_type_t::INT_REGION: {
	  var_t a_in = m_lfac.mkIntArrayVar(rgn.getRegionInfo().getBitwidth());
	  inputs.push_back(a_in);	  
	  entry.array_assign(m_lfac.mkArrayVar(rgn), a_in);	  
	  break;
	}
	case region_type_t::BOOL_REGION: {
	  var_t a_in = m_lfac.mkBoolArrayVar();
	  inputs.push_back(a_in);	  
	  entry.array_assign(m_lfac.mkArrayVar(rgn), a_in);	  	  
	  break;
	}
	default: /*unreachable*/;
	}
	// output version
	outputs.push_back(m_lfac.mkArrayVar(rgn));
      } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
	CLAM_WARNING("Missing modified region " << rgn << " from function declaration "
		     << m_func.getName());
      }
    }
    
    // -- add more output parameters
    for (auto rgn : outRegions) {
      if (m_lfac.getTrack() == CrabBuilderPrecision::SINGLETON_MEM) {
	outputs.push_back(m_lfac.mkArrayVar(rgn));
      } else if (m_lfac.getTrack() == CrabBuilderPrecision::MEM) {
	CLAM_WARNING("Missing new region " << rgn << " from function declaration "
		     << m_func.getName());
      }      
    }
  }
  
  // -- Finally, we add the function declaration
   
  // Sanity check
  std::vector<var_t> sorted_ins(inputs.begin(), inputs.end());
  std::vector<var_t> sorted_outs(outputs.begin(), outputs.end());
  std::sort(sorted_ins.begin(), sorted_ins.end());
  std::sort(sorted_outs.begin(), sorted_outs.end());
  std::vector<var_t> intersect;
  std::set_intersection(sorted_ins.begin(), sorted_ins.end(),
			sorted_outs.begin(), sorted_outs.end(),
			std::back_inserter(intersect));
  if (!intersect.empty()) {
    crab::errs() << "INPUTS: {";
    for (auto i : inputs) {
      crab::outs() << i << ";";
    }
    crab::errs() << "}\n";
    crab::errs() << "OUTPUTS: {";
    for (auto o : outputs) {
      crab::outs() << o << ";";
     }
    crab::errs() << "}\n";
    CLAM_ERROR("function inputs and outputs should not intersect");
  }

  typedef function_decl<number_t, varname_t> function_decl_t;
  m_cfg->set_func_decl(
     function_decl_t(m_func.getName().str(), inputs, outputs));
   
}
  
/* CrabBuilderParams class */

void CrabBuilderParams::write(raw_ostream &o) const {
  o << "CFG builder options:\n";
  o << "\tabstraction level: ";
  switch (precision_level) {
  case CrabBuilderPrecision::NUM:
    o << "only integers\n";
    break;
  case CrabBuilderPrecision::SINGLETON_MEM:
    o << "integers and singleton memory objects\n";
    break;
  case CrabBuilderPrecision::MEM:
    o << "integers and all memory objects\n";
    break;
  default:;
    ;
  }
  o << "\tsimplify cfg: " << simplify << "\n";
  o << "\tinterproc cfg: " << interprocedural << "\n";
  o << "\tlower singleton aliases into scalars: "
    << lower_singleton_aliases << "\n";
  o << "\tenable big numbers: " << enable_bignums << "\n";
}

/* CFG Builder class */
CfgBuilder::CfgBuilder(const llvm::Function &func, CrabBuilderManager &man)
    : m_impl(std::make_unique<CfgBuilderImpl>(
          func, man.getVarFactory(), man.getHeapAbstraction(),
          &(man.getTLIWrapper()),
          man.getCfgBuilderParams())),
      m_ls(nullptr) {}

CfgBuilder::~CfgBuilder() {}

void CfgBuilder::buildCfg() { m_impl->buildCfg(); }

cfg_t &CfgBuilder::getCfg() { return m_impl->getCfg(); }

basic_block_label_t
CfgBuilder::getCrabBasicBlock(const llvm::BasicBlock *bb) const {
  return m_impl->getCrabBasicBlock(bb);
}

const basic_block_label_t *
CfgBuilder::getCrabBasicBlock(const llvm::BasicBlock *src,
                              const llvm::BasicBlock *dst) const {
  return m_impl->getCrabBasicBlock(src, dst);
}

const llvm::Instruction *
CfgBuilder::getInstruction(const statement_t &s) const {
  return m_impl->getInstruction(s);
}

void CfgBuilder::computeLiveSymbols() {
  if (!m_ls) {
    auto &cfg = m_impl->getCfg();
    m_ls = std::make_unique<liveness_t>(cfg);
    CRAB_VERBOSE_IF(1, auto fdecl = cfg.get_func_decl();
                    crab::get_msg_stream()
                    << "Running liveness analysis for " << fdecl.get_func_name()
                    << "  ...\n";);
    m_ls->exec();

    unsigned total_live, avg_live_per_blk, max_live_per_blk;
    m_ls->get_stats(total_live, max_live_per_blk, avg_live_per_blk);
    CRAB_VERBOSE_IF(1, crab::outs()
                           << "-- Max number of out live vars per block="
                           << max_live_per_blk << "\n"
                           << "-- Avg number of out live vars per block="
                           << avg_live_per_blk << "\n";);
    crab::CrabStats::count_max("Liveness.count.maxOutVars", max_live_per_blk);
  }
}

const CfgBuilder::liveness_t *CfgBuilder::getLiveSymbols() const {
  return (m_ls ? &*m_ls : nullptr);
}

Optional<CfgBuilder::varset>
CfgBuilder::getLiveSymbols(const BasicBlock *B) const {
  if (!m_ls) {
    return llvm::None;
  } else {
    basic_block_label_t bbl = getCrabBasicBlock(B);
    return m_ls->get(bbl);
  }
}

/* CFG Manager class */
CrabBuilderManager::CrabBuilderManager(CrabBuilderParams params,
                                       llvm::TargetLibraryInfoWrapperPass &tli,
                                       std::unique_ptr<HeapAbstraction> mem)
    : m_params(params), m_tli(tli), m_mem(std::move(mem)) {
  CRAB_VERBOSE_IF(1, m_params.write(llvm::errs()));
}

CrabBuilderManager::~CrabBuilderManager() {}

CrabBuilderManager::CfgBuilderPtr
CrabBuilderManager::mkCfgBuilder(const Function &f) {
  auto it = m_cfg_builder_map.find(&f);
  if (it == m_cfg_builder_map.end()) {
    CfgBuilderPtr builder(new CfgBuilder(f, *this));
    builder->buildCfg();
    m_cfg_builder_map.insert({&f, builder});
    return builder;
  } else {
    return it->second;
  }
}

bool CrabBuilderManager::hasCfg(const Function &f) const {
  return m_cfg_builder_map.find(&f) != m_cfg_builder_map.end();
}

cfg_t &CrabBuilderManager::getCfg(const Function &f) const {
  return getCfgBuilder(f)->getCfg();
}

CrabBuilderManager::CfgBuilderPtr
CrabBuilderManager::getCfgBuilder(const Function &f) const {
  auto it = m_cfg_builder_map.find(&f);
  if (it == m_cfg_builder_map.end()) {
    CLAM_ERROR("Cannot find crab cfg for ", f.getName());
  }
  return it->second;
}

variable_factory_t &CrabBuilderManager::getVarFactory() { return m_vfac; }

const CrabBuilderParams &CrabBuilderManager::getCfgBuilderParams() const {
  return m_params;
}

const llvm::TargetLibraryInfo &
CrabBuilderManager::getTLI(const Function &F) const {
  return m_tli.getTLI(F);
}

llvm::TargetLibraryInfoWrapperPass &CrabBuilderManager::getTLIWrapper() const {
  return m_tli;
}

HeapAbstraction &CrabBuilderManager::getHeapAbstraction() { return *m_mem; }

} // end namespace clam
