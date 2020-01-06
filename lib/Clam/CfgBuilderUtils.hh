#pragma once

#include "clam/CfgBuilderParams.hh"
#include "clam/crab/crab_cfg.hh"

namespace llvm {
class Type;
class Value;
class Function;
class Instruction;
class APInt;
class ConstantInt;
class DataLayout;
class CastInt;
class CmpInst;
} // namespace llvm

namespace clam {

// Any integer that cannot be represented by 64 bits is considered a bignum.
bool isSignedBigNum(const llvm::APInt &v);

bool isBool(const llvm::Type *t);

bool isBool(const llvm::Value &v);

bool isInteger(const llvm::Type *t);

bool isInteger(const llvm::Value &v);

bool isPointer(const llvm::Type *t, const CrabBuilderParams &params);

bool isPointer(const llvm::Value &v, const CrabBuilderParams &params);

// Converts v to z_number. Assumes that v is signed
ikos::z_number toZNumber(const llvm::APInt &v, const CrabBuilderParams &params,
                         bool &is_bignum);

// The return value should be z_number and not number_t
ikos::z_number getIntConstant(const llvm::ConstantInt *CI,
                              const CrabBuilderParams &params, bool &is_bignum);

bool isTrackedType(const llvm::Type &ty, const CrabBuilderParams &params);

bool isTracked(const llvm::Value &v, const CrabBuilderParams &params);

// A crab callsite should return a value if the I's callee has a
// tracked return type, regardless whether the LLVM callsite
// returns. In LLVM, a callsite does not need to fully match with
// the function signature but in Crab we require to do so. E.g.,
// LLVM can remove the return value of the callsite if it's dead.
bool ShouldCallSiteReturn(llvm::CallInst &I, const CrabBuilderParams &params);

// Whether the callsite returns a value.
bool DoesCallSiteReturn(llvm::CallInst &I, const CrabBuilderParams &params);

bool hasDebugLoc(const llvm::Instruction *inst);

crab::cfg::debug_info getDebugLoc(const llvm::Instruction *inst);

uint64_t storageSize(const llvm::Type *t, const llvm::DataLayout &dl);

// Convert GT integer comparisons  to GE
void normalizeCmpInst(llvm::CmpInst &I);

bool isIntToBool(const llvm::CastInt &I);

bool isBoolToInt(const llvm::CastInt &I);

bool isBoolArray(const llvm::Type &T);

bool isIntArray(const llvm::Type &T);

bool isAssertFn(const llvm::Function &F);

bool isSeaHornFail(const llvm::Function &F);

bool isErrorFn(const llvm::Function &F);

bool isAssumeFn(const llvm::Function &F);

bool isNotAssumeFn(const llvm::Function &F);

bool isVerifierCall(const llvm::Function &F);

bool isZeroInitializer(const llvm::Function &F);

bool isZeroInitializer(const llvm::CallInst &CI);

bool isIntInitializer(const llvm::Function &F);

bool isIntInitializer(const llvm::CallInst &CI);

// Return true if all uses are BranchInst's
bool AllUsesAreBrInst(llvm::Value &V);

// Return true if all uses are BranchInst's or Select's
bool AllUsesAreBrOrIntSelectCondInst(llvm::Value &V);

// Return true if all uses are the callee at callsites
bool AllUsesAreIndirectCalls(llvm::Value &V);

// Return true if all uses are verifier calls (assume/assert)
bool AllUsesAreVerifierCalls(llvm::Value &V);

// Return true if all uses are GEPs
bool AllUsesAreGEP(llvm::Value &V);

} // namespace clam
