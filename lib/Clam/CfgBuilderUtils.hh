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
class TargetLibraryInfo;
} // namespace llvm

namespace seadsa {
class AllocWrapInfo;
} // namespace seadsa

namespace clam {

class HeapAbstraction;

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

/**
 * Allocation functions
 * ====================
 *
 * These predicates mirror the LLVM MemoryBuiltins ones they replace
 * (llvm::isAllocationFn, llvm::isMallocOrCallocLikeFn, ...), but the question
 * "does this call allocate memory?" is answered by sea-dsa's AllocWrapInfo
 * rather than by LLVM.
 *
 * Why: sea-dsa decides what allocates *by name*
 * (AllocWrapInfo::isAllocWrapper, whose set is seeded with malloc/calloc) and
 * marks the resulting DSA node as heap. Clam then asks that very same sea-dsa
 * graph for the region of the allocated pointer. If clam used a different
 * criterion the two would disagree: sea-dsa would build a heap region while
 * clam refused to emit the corresponding make_ref, and every later access to
 * that pointer would fail to find it in the reference map. Asking sea-dsa
 * keeps both sides in sync by construction.
 *
 * That disagreement is not hypothetical. LLVM 15 moved MemoryBuiltins to
 * attribute-based detection (allockind / "alloc-family") and clang does not
 * emit those attributes at -O0, so llvm::isAllocationFn returns false for a
 * plain call to malloc. Worse, llvm::isMallocOrCallocLikeFn returns false even
 * when the attributes *are* present. sea-dsa, being name-based, was unaffected.
 *
 * The signatures below are kept deliberately close to the MemoryBuiltins ones
 * so that if MemoryBuiltins becomes usable again each body can go back to the
 * corresponding llvm:: call without touching any callsite. When AllocWrapInfo
 * is unavailable (--crab-heap-analysis=none) we already fall back to LLVM.
 **/

// sea-dsa's allocation info, or null if the heap analysis is not sea-dsa.
const seadsa::AllocWrapInfo *getAllocWrapInfo(HeapAbstraction &mem);

// Replaces llvm::isAllocationFn(&I, tli).
bool isAllocationFn(const llvm::CallInst &I, HeapAbstraction &mem,
                    const llvm::TargetLibraryInfo *tli);

// Replaces llvm::isMallocOrCallocLikeFn(&I, tli). sea-dsa does not distinguish
// malloc from calloc, which is fine: callers discriminate by name to find out
// where the size operand lives.
bool isMallocOrCallocLikeFn(const llvm::CallInst &I, HeapAbstraction &mem,
                            const llvm::TargetLibraryInfo *tli);

// Replaces llvm::getFreedOperand(&I, tli): returns the freed pointer, or null
// if I does not free memory. Deallocation has exactly the same LLVM 15
// problem as allocation -- llvm::getFreedOperand is driven by the allockind
// attribute, so it returns null for a plain call to free at -O0 -- and
// sea-dsa knows the answer via AllocWrapInfo::isDeallocWrapper.
llvm::Value *getFreedOperand(const llvm::CallInst &I, HeapAbstraction &mem,
                             const llvm::TargetLibraryInfo *tli);

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
