#pragma once

//===-------- Resolve indirect calls using signature match ----------------===//
//
// This class is almost the same than Devirt in DSA but it does not
// use alias analysis to compute the possible targets of an indirect
// call. Instead, it simply selects those functions whose signatures
// match.
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

  public : 
   
    typedef const llvm::PointerType *AliasSetId;
    typedef llvm::SmallVector<const llvm::Function *, 8> AliasSet;
    typedef llvm::DenseMap<AliasSetId, AliasSet> AliasSetMap;

  private:
    
    /// map from alias-id to the corresponding alias set
    AliasSetMap& m_aliasSets;
    
    // Call graph of the program
    llvm::CallGraph * m_cg;    

    bool m_allow_indirect_calls;
   
    // Worklist of call sites to transform
    llvm::SmallVector<llvm::Instruction*, 32> m_worklist;

    /// returns an AliasId of the called value
    /// requires that CS is an indirect call through a function pointer
    static AliasSetId typeAliasId(llvm::CallSite &CS);
    
    /// maps alias set id to an existing bounce function
    llvm::DenseMap<AliasSetId, llvm::Function*> m_bounceMap;
    
    /// turn the indirect call-site into a direct one
    void mkDirectCall(llvm::CallSite CS);
    
    /// create a bounce function that calls functions directly
    llvm::Function* mkBounceFn(llvm::CallSite &CS);
    
   public:
    
    DevirtualizeFunctions(AliasSetMap& m, llvm::CallGraph* cg, bool allow_indirect_calls);
    bool run(llvm::Module& M);

    /// returns an id of an alias set to which this function belongs
    static AliasSetId typeAliasId(const llvm::Function &F);
        
    // -- VISITOR IMPLEMENTATION --
    
    void visitCallSite(llvm::CallSite &CS);
    void visitCallInst(llvm::CallInst &CI);
    void visitInvokeInst(llvm::InvokeInst &II);
  };
  
}
