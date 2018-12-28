#pragma once

//===-------------------- Resolve indirect calls --------------------------===//
//
// This class is kind of copy-and-paste version from Devirt in LLVM
// DSA but it has been factorized to be parametric in how the set of
// possible targets used to resolve indirect callsites are computed.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Support/ErrorHandling.h"

namespace crab_llvm {


  class CallSiteResolver {
  protected:
    CallSiteResolver() {}
  public:

    typedef llvm::SmallVector<const llvm::Function *, 8> AliasSet;
    typedef llvm::DenseMap<const llvm::Instruction*, AliasSet> TargetMap;
    
    virtual ~CallSiteResolver() {}
    
    virtual bool useAliasing(void) const = 0;
    
    virtual bool hasTargets(const llvm::Instruction* CS) const = 0;
    
    virtual AliasSet& getTargets(const llvm::Instruction* CS) = 0;
    
    virtual const AliasSet& getTargets(const llvm::Instruction* CS) const  = 0;    
  };

  /* Dummy callsite resolver */
  class NoAliasResolver: public CallSiteResolver {
  public:
    
    NoAliasResolver(): CallSiteResolver() { }
    
    bool useAliasing() const {
      return false;
    }

    bool hasTargets(const llvm::Instruction* CS) const {
      return false;
    }

    AliasSet& getTargets(const llvm::Instruction* CS) {
      llvm::report_fatal_error("Should not be called NoAliasResolver::getTargets()");
    }
    
    const AliasSet& getTargets(const llvm::Instruction* CS) const {
      llvm::report_fatal_error("Should not be called NoAliasResolver::getTargets()");
    }
  };
  
  //
  // Class: DevirtualizeFunctions
  //
  // Description:
  //  This transform pass will look for indirect function calls and transform
  //  them into a switch statement that selects one of several direct function
  //  calls to execute.
  //
  class DevirtualizeFunctions : public llvm::InstVisitor<DevirtualizeFunctions>  {

  private : 

    typedef CallSiteResolver::AliasSet AliasSet;    
    typedef const llvm::PointerType *AliasSetId;
    typedef llvm::DenseMap<AliasSetId, AliasSet> TypeAliasSetMap;
    
    /// map from alias-id to the corresponding alias set
    TypeAliasSetMap m_typeAliasSets;
    
    // Call graph of the program
    llvm::CallGraph* m_cg;    
    
    bool m_allowIndirectCalls;

    /// maps alias set id to an existing bounce function
    llvm::DenseMap<AliasSetId, llvm::Function*> m_bounceMap;
    
    // Worklist of call sites to transform
    llvm::SmallVector<llvm::Instruction*, 32> m_worklist;

    /// returns an AliasId of the called value
    /// requires that CS is an indirect call through a function pointer
    static AliasSetId typeAliasId(llvm::CallSite &CS);

    /// returns an id of an alias set to which this function belongs
    static AliasSetId typeAliasId(const llvm::Function &F);
    
    /// turn the indirect call-site into a direct one
    void mkDirectCall(llvm::CallSite CS, CallSiteResolver* CSR);
    
    /// create a bounce function that calls functions directly
    llvm::Function* mkBounceFn(llvm::CallSite &CS, CallSiteResolver* CSR);

    /// Populate m_typeAliasSets
    void computeTypeAliasSets(llvm::Module& M);
    
   public:
    
    DevirtualizeFunctions(llvm::CallGraph* cg, bool allowIndirectCalls);

    bool resolveCallSites(llvm::Module& M, CallSiteResolver* CSR);
        
    // -- VISITOR IMPLEMENTATION --
    
    void visitCallSite(llvm::CallSite &CS);
    void visitCallInst(llvm::CallInst &CI);
    void visitInvokeInst(llvm::InvokeInst &II);
  };
  
}
