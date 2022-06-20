#pragma once

#include "clam/crab/crab_lang.hh"

#include "llvm/ADT/SmallVector.h"

namespace llvm {
class TargetLibraryInfo;
class Function;
class BasicBlock;
class Instruction;
class GetElementPtrInst;
class StoreInst;
class LoadInst;
class MemSetInst;
class MemTransferInst;
class SelectInst;
class CallBase;
} // namespace llvm

namespace clam {

class CrabMakeRefOps;
class CrabRemoveRefOps;
class CrabGepRefOps;
class CrabStoreRefOps;
class CrabLoadRefOps;
class CrabMemsetOps;
class CrabMemTransferOps;
class CrabSelectRefOps;
class CrabIsDerefOps;
/*
 * Allow to add extra Crab statements _before_ and _after_ each of
 * these instructions are translated to CrabIR by CfgBuilder.  This is
 * a working-in-progress API and it is focused on memory region
 * operations.
 */
class CrabIREmitter {
public:
  virtual ~CrabIREmitter() {}
  
  virtual void visitBeforeBasicBlock(llvm::BasicBlock &BB) = 0;
  virtual void visitAfterBasicBlock(llvm::BasicBlock &BB) = 0;

  /**
   * I is AllocaInst or MemoryBuiltins::IsAllocationFn(I, tli) holds
   **/
  virtual void visitBeforeAlloc(llvm::Instruction &I,
                                const llvm::TargetLibraryInfo &tli,
                                CrabMakeRefOps &s) = 0;
  virtual void visitAfterAlloc(llvm::Instruction &I,
                               const llvm::TargetLibraryInfo &tli,
                               CrabMakeRefOps &s) = 0;

  /* MemoryBuiltins::IsFreeCall(I, tli) holds */
  virtual void visitBeforeFree(llvm::Instruction &I,
                               const llvm::TargetLibraryInfo &tli,
                               CrabRemoveRefOps &s) = 0;
  virtual void visitAfterFree(llvm::Instruction &I,
                              const llvm::TargetLibraryInfo &tli,
                              CrabRemoveRefOps &s) = 0;

  /* I can be GetElementPtrInst or other instructions that are
     translated to gep_ref (e.g., PHI nodes, BitCast, etc) */
  virtual void visitBeforeGep(llvm::Instruction &I, CrabGepRefOps &s) = 0;
  virtual void visitAfterGep(llvm::Instruction &I, CrabGepRefOps &s) = 0;

  virtual void visitBeforeStore(llvm::StoreInst &I, CrabStoreRefOps &s) = 0;
  virtual void visitAfterStore(llvm::StoreInst &I, CrabStoreRefOps &s) = 0;

  virtual void visitBeforeLoad(llvm::LoadInst &I, CrabLoadRefOps &s) = 0;
  virtual void visitAfterLoad(llvm::LoadInst &I, CrabLoadRefOps &s) = 0;

  virtual void visitBeforeMemset(llvm::MemSetInst &I, CrabMemsetOps &s) = 0;
  virtual void visitAfterMemset(llvm::MemSetInst &I, CrabMemsetOps &s) = 0;

  virtual void visitBeforeMemTransfer(llvm::MemTransferInst &I,
                                      CrabMemTransferOps &s) = 0;
  virtual void visitAfterMemTransfer(llvm::MemTransferInst &I,
                                     CrabMemTransferOps &s) = 0;
  virtual void visitBeforeRefSelect(llvm::SelectInst &I,
                                    CrabSelectRefOps &s) = 0;
  virtual void visitAfterRefSelect(llvm::SelectInst &I,
                                   CrabSelectRefOps &s) = 0;
  /* is_dereferenceable intrinsics */
  virtual void visitBeforeIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) = 0;
  virtual void visitAfterIsDeref(llvm::CallBase &I, CrabIsDerefOps &s) = 0;  
};

using CrabIREmitterPtr = std::unique_ptr<CrabIREmitter>;
using CrabIREmitterVec = llvm::SmallVector<CrabIREmitterPtr, 4>;

/**
 * This is a convenient wrapper for storing the arguments of CrabIR
 * make_ref statement.
 **/
class CrabMakeRefOps {
  // ref:= make_ref(region, size, as)
  var_t m_ref;
  var_t m_region;
  var_or_cst_t m_size;
  crab::tag m_as;
  basic_block_t &m_bb;

public:
  CrabMakeRefOps(var_t ref, var_t region, var_or_cst_t size, crab::tag as,
                 basic_block_t &bb)
      : m_ref(ref), m_region(region), m_size(size), m_as(as), m_bb(bb) {}

  var_t &getRef() { return m_ref; }
  const var_t &getRef() const { return m_ref; }

  var_t &getRegion() { return m_region; }
  const var_t &getRegion() const { return m_region; }

  var_or_cst_t &getSize() { return m_size; }
  const var_or_cst_t &getSize() const { return m_size; }

  crab::tag getAllocationSite() const { return m_as; }

  basic_block_t &getBasicBlock() { return m_bb; }
};

/**
 * This is a convenient wrapper for storing the arguments of CrabIR
 * remove_ref statement.
 **/
class CrabRemoveRefOps {
  // remove_ref(region, ref)
  var_t m_region;
  var_t m_ref;
  basic_block_t &m_bb;

public:
  CrabRemoveRefOps(var_t region, var_t ref, basic_block_t &bb)
    : m_region(region), m_ref(ref), m_bb(bb) {}

  var_t &getRef() { return m_ref; }
  const var_t &getRef() const { return m_ref; }

  var_t &getRegion() { return m_region; }
  const var_t &getRegion() const { return m_region; }

  basic_block_t &getBasicBlock() { return m_bb; }
};

/**
 * This is a convenient wrapper for storing the arguments of CrabIR
 * gep_ref statement.
 **/
class CrabGepRefOps {
  // (lhs_reg, lhs_ref) := gep(rhs_reg, rhs_ref, offset)
  var_t m_lhs_ref;
  var_t m_lhs_region;
  var_t m_rhs_ref;
  var_t m_rhs_region;
  lin_exp_t m_offset;
  basic_block_t &m_bb;

public:
  CrabGepRefOps(var_t lhs_ref, var_t lhs_region, var_t rhs_ref,
                var_t rhs_region, lin_exp_t offset, basic_block_t &bb)
      : m_lhs_ref(lhs_ref), m_lhs_region(lhs_region), m_rhs_ref(rhs_ref),
        m_rhs_region(rhs_region), m_offset(offset), m_bb(bb) {}

  var_t &getLhsRef() { return m_lhs_ref; }
  const var_t &getLhsRef() const { return m_lhs_ref; }

  var_t &getRhsRef() { return m_rhs_ref; }
  const var_t &getRhsRef() const { return m_rhs_ref; }

  var_t &getLhsRegion() { return m_lhs_region; }
  const var_t &getLhsRegion() const { return m_lhs_region; }

  var_t &getRhsRegion() { return m_rhs_region; }
  const var_t &getRhsRegion() const { return m_rhs_region; }

  lin_exp_t &getOffset() { return m_offset; }
  const lin_exp_t &getOffset() const { return m_offset; }

  basic_block_t &getBasicBlock() { return m_bb; }
};

/**
 * This is a convenient wrapper for storing the arguments of CrabIR
 * store_to_ref statement.
 **/
class CrabStoreRefOps {
  // store_to_ref(region, ref, val)
  var_t m_ref;
  var_t m_region;
  var_or_cst_t m_val;
  basic_block_t &m_bb;

public:
  CrabStoreRefOps(var_t ref, var_t region, var_or_cst_t val, basic_block_t &bb)
      : m_ref(ref), m_region(region), m_val(val), m_bb(bb) {}

  var_t &getRef() { return m_ref; }
  const var_t &getRef() const { return m_ref; }

  var_t &getRegion() { return m_region; }
  const var_t &getRegion() const { return m_region; }

  var_or_cst_t &getVal() { return m_val; }
  const var_or_cst_t &getVal() const { return m_val; }

  basic_block_t &getBasicBlock() { return m_bb; }
};

/**
 * This is a convenient wrapper for storing the arguments of CrabIR
 * load_from_ref statement.
 **/
class CrabLoadRefOps {
  // lhs:= load_from_ref(region, ref)
  var_t m_lhs;
  var_t m_region;
  var_t m_ref;
  basic_block_t &m_bb;

public:
  CrabLoadRefOps(var_t lhs, var_t region, var_t ref, basic_block_t &bb)
      : m_lhs(lhs), m_region(region), m_ref(ref), m_bb(bb) {}

  var_t &getLhs() { return m_lhs; }
  const var_t &getLhs() const { return m_lhs; }

  var_t &getRef() { return m_ref; }
  const var_t &getRef() const { return m_ref; }

  var_t &getRegion() { return m_region; }
  const var_t &getRegion() const { return m_region; }

  basic_block_t &getBasicBlock() { return m_bb; }
};

/**
 * This is a convenient wrapper for storing the arguments of memset.
 * Note that CrabIR does not have a statement for memset but we might
 * want to add assertions about its operands.
 **/
class CrabMemsetOps {
  var_t m_ref;
  var_t m_region;
  var_or_cst_t m_len;
  // ignored intentionally val
  basic_block_t &m_bb;

public:
  CrabMemsetOps(var_t ref, var_t region, var_or_cst_t len, basic_block_t &bb)
      : m_ref(ref), m_region(region), m_len(len), m_bb(bb) {}

  var_t &getRef() { return m_ref; }
  const var_t &getRef() const { return m_ref; }

  var_t &getRegion() { return m_region; }
  const var_t &getRegion() const { return m_region; }

  var_or_cst_t &getLength() { return m_len; }
  const var_or_cst_t &getLength() const { return m_len; }

  basic_block_t &getBasicBlock() { return m_bb; }
};

/**
 * This is a convenient wrapper for storing the arguments of
 * memcpy/memmove Note that CrabIR does not have a statement for
 * memset but we might want to add assertions about its operands.
 **/
class CrabMemTransferOps {
  var_t m_ref_src;
  var_t m_region_src;
  var_t m_ref_dst;
  var_t m_region_dst;
  var_or_cst_t m_len;
  basic_block_t &m_bb;

public:
  CrabMemTransferOps(var_t ref_src, var_t region_src,
		     var_t ref_dst, var_t region_dst, var_or_cst_t len,
                      basic_block_t &bb)
      : m_ref_src(ref_src), m_region_src(region_src), m_ref_dst(ref_dst),
        m_region_dst(region_dst), m_len(len), m_bb(bb) {}

  var_t &getRefSrc() { return m_ref_src; }
  const var_t &getRefSrc() const { return m_ref_src; }

  var_t &getRegionSrc() { return m_region_src; }
  const var_t &getRegionSrc() const { return m_region_src; }

  var_t &getRefDst() { return m_ref_dst; }
  const var_t &getRefDst() const { return m_ref_dst; }

  var_t &getRegionDst() { return m_region_dst; }
  const var_t &getRegionDst() const { return m_region_dst; }

  var_or_cst_t &getLength() { return m_len; }
  const var_or_cst_t &getLength() const { return m_len; }
  
  basic_block_t &getBasicBlock() { return m_bb; }
};

/**
 * This is a convenient wrapper for selecting the arguments of
 * CrabIR select_ref statement.
 **/
class CrabSelectRefOps {
public:
  using opt_pair_var_t = llvm::Optional<std::pair<var_t, var_t>>;

  CrabSelectRefOps(var_t ref_lhs, var_t region_lhs, var_t cond,
                   opt_pair_var_t op1, opt_pair_var_t op2, basic_block_t &bb)
      : m_cond(cond), m_op1(op1), m_op2(op2), 
        m_ref_lhs(ref_lhs), m_region_lhs(region_lhs),  m_bb(bb) {}

  var_t &getRefCond() { return m_cond; }
  const var_t &getRefCond() const { return m_cond; }

  var_t &getRefLhs() { return m_ref_lhs; }
  const var_t &getRefLhs() const { return m_ref_lhs; }

  var_t &getRegionLhs() { return m_region_lhs; }
  const var_t &getRegionLhs() const { return m_region_lhs; }

  opt_pair_var_t &getOp1() { return m_op1; }
  const opt_pair_var_t &getOp1() const { return m_op1; }

  opt_pair_var_t &getOp2() { return m_op2; }
  const opt_pair_var_t &getOp2() const { return m_op2; }

  basic_block_t &getBasicBlock() { return m_bb; }

private:
  var_t m_cond;
  opt_pair_var_t m_op1;
  opt_pair_var_t m_op2;
  var_t m_ref_lhs;
  var_t m_region_lhs;
  basic_block_t &m_bb;
};

/**
 * This is a convenient wrapper for storing the arguments of CrabIR
 * intrinsics is_derefenceable.
 **/
class CrabIsDerefOps {
  // b:= crab_intrinsic(is_dereferenceable, region, ref, size)
  var_t m_lhs;
  var_t m_region;
  var_t m_ref;  
  var_or_cst_t m_size;
  basic_block_t &m_bb;

public:
  CrabIsDerefOps(var_t lhs, var_t region, var_t ref, var_or_cst_t size,
		 basic_block_t &bb)
    : m_lhs(lhs), m_region(region), m_ref(ref), m_size(size), m_bb(bb) {}

  var_t &getLhs() { return m_lhs; }
  const var_t &getLhs() const { return m_lhs; }

  var_t &getRegion() { return m_region; }
  const var_t &getRegion() const { return m_region; }

  var_t &getRef() { return m_ref; }
  const var_t &getRef() const { return m_ref; }
  
  var_or_cst_t &getSize() { return m_size; }
  const var_or_cst_t &getSize() const { return m_size; }

  basic_block_t &getBasicBlock() { return m_bb; }
};

} // end namespace clam
