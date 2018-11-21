#pragma once

//===-------------------- Resolve indirect calls --------------------------===//
//
// This class is a copy-and-paste version from Devirt in DSA but it
// has been factorized a bit to be parametric in the set of possible
// targets used to resolve indirect callsites.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/Analysis/CallGraph.h"

namespace crab_llvm {

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
   
    typedef const llvm::PointerType *AliasSetId;
    typedef llvm::SmallVector<const llvm::Function *, 8> AliasSet;
    typedef llvm::DenseMap<AliasSetId, AliasSet> TypeAliasSetMap;
    
  public:
    
    // Map a callsite to a set of targets to resolve the callsite.
    // The callsite must be an indirect one.    
    typedef llvm::DenseMap<llvm::Instruction*, AliasSet> AliasTargetMap;
    
  private:
    
    /// map from alias-id to the corresponding alias set
    TypeAliasSetMap m_typeAliasSets;
    
    // Call graph of the program
    llvm::CallGraph * m_cg;    
    
    bool m_allowIndirectCalls;
   
    // Worklist of call sites to transform
    llvm::SmallVector<llvm::Instruction*, 32> m_worklist;

    /// returns an AliasId of the called value
    /// requires that CS is an indirect call through a function pointer
    static AliasSetId typeAliasId(llvm::CallSite &CS);

    /// returns an id of an alias set to which this function belongs
    static AliasSetId typeAliasId(const llvm::Function &F);
    
    /// maps alias set id to an existing bounce function
    llvm::DenseMap<AliasSetId, llvm::Function*> m_bounceMap;
    
    /// turn the indirect call-site into a direct one
    void mkDirectCall(llvm::CallSite CS, AliasTargetMap& ATM);
    
    /// create a bounce function that calls functions directly
    llvm::Function* mkBounceFn(llvm::CallSite &CS, AliasTargetMap& ATM);

    /// Populate m_typeAliasSets
    void computeTypeAliasSets(llvm::Module& M);
    
   public:
    
    DevirtualizeFunctions(llvm::CallGraph* cg, bool allowIndirectCalls);

    bool resolveCallSites(llvm::Module& M, AliasTargetMap& ATM /*it can be empty*/);
        
    // -- VISITOR IMPLEMENTATION --
    
    void visitCallSite(llvm::CallSite &CS);
    void visitCallInst(llvm::CallInst &CI);
    void visitInvokeInst(llvm::InvokeInst &II);
  };
  
}
