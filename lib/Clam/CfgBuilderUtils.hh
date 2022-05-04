#pragma once

#include "clam/CfgBuilderParams.hh"
#include "clam/crab/crab_lang.hh"

namespace llvm {
class Type;
class Value;
class Function;
class Instruction;
class CallInst;
class SelectInst;
class APInt;
class ConstantInt;
class DataLayout;
class CastInt;
class CmpInst;
class MDNode;
} // namespace llvm

namespace clam {

// Any integer that cannot be represented by 64 bits is considered a bignum.
bool isSignedBigNum(const llvm::APInt &v);

bool isBool(const llvm::Type *t);

bool isBool(const llvm::Value &v);

bool isInteger(const llvm::Type *t);

bool isInteger(const llvm::Value &v);

bool isReference(const llvm::Type *t, const CrabBuilderParams &params);

bool isReference(const llvm::Value &v, const CrabBuilderParams &params);

// Converts v to z_number. Assumes that v is signed
ikos::z_number toZNumber(const llvm::APInt &v, const CrabBuilderParams &params,
			 bool interpretAsSigned, bool &isTooBig);                         

// The return value should be z_number and not number_t
ikos::z_number getIntConstant(const llvm::ConstantInt *CI,
                              const CrabBuilderParams &params,
			      bool interpretAsSigned,
			      bool &isTooBig);

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

crab::cfg::debug_info getDebugLoc(const llvm::Instruction *inst, uint32_t assertion_id);

uint64_t storageSize(const llvm::Type *t, const llvm::DataLayout &dl);

// Convert GT and GE integer comparisons to LE and LT
void normalizeCmpInst(llvm::CmpInst &I);

bool isIntToBool(const llvm::CastInt &I);

bool isBoolToInt(const llvm::CastInt &I);

bool isBoolArray(const llvm::Type &T);

bool isIntArray(const llvm::Type &T);

bool isAssertFn(const llvm::Function &F);

bool isSeaHornFail(const llvm::Function &F);

bool isErrorFn(const llvm::Function &F);

bool isAssumeFn(const llvm::Function &F);

bool isCrabIntrinsic(const llvm::Function &F);

std::string getCrabIntrinsicName(const llvm::Function &F);

bool isNotAssumeFn(const llvm::Function &F);

bool isVerifierCall(const llvm::Function &F);

bool isZeroInitializer(const llvm::Function &F);

bool isZeroInitializer(const llvm::CallInst &CI);

bool isIntInitializer(const llvm::Function &F);

bool isIntInitializer(const llvm::CallInst &CI);

// deprecated
std::string getAssertKindFromMetadata(llvm::MDNode *MDN);

// Return true if any use is a verifier call  
bool AnyUseIsVerifierCall(llvm::Value &V);
  
// Return true if all uses are BranchInst's
bool AllUsesAreBrInst(llvm::Value &V);

// Return true if all uses are either BranchInst's or Select's that
// satisfy selectFilter.
bool AllUsesAreBrOrIntSelectCondInst(llvm::Value &V,
                                     const CrabBuilderParams &params,
				     std::function<bool(llvm::SelectInst*)> selectFilter);
				    
// Return true if all uses are the callee at callsites
bool AllUsesAreIndirectCalls(llvm::Value &V);

// Return true if all uses are verifier calls (assume/assert)
bool AllUsesAreVerifierCalls(llvm::Value &V, bool goThroughIntegerCasts,
                             bool nonBoolCond,
                             llvm::SmallVector<llvm::CallInst *, 4> &,
			     bool onlyAssume = false);
bool AllUsesAreVerifierCalls(llvm::Value &V);

// Return true if all uses are GEPs
bool AllUsesAreGEP(llvm::Value &V);

// Return true if all uses are ignored instructions
bool AllUsesAreIgnoredInst(llvm::Value &V);


/* 
   Reverse topological sort of (possibly cyclic) CFG
 */
void revTopoSort(const llvm::Function &F, std::vector<const llvm::BasicBlock*> &out);

/* 
   Topological sort of (possibly cyclic) CFG
 */
void topoSort(const llvm::Function &F, std::vector<const llvm::BasicBlock*> &out);

} // namespace clam
