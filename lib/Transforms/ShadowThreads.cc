#include <Transforms/ShadowThreads.hh>

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "boost/lexical_cast.hpp"

#define THDEBUG

using namespace llvm;

// TODO: shadow pthread_lock and pthread_unlock

namespace llvm_ikos
{

  using namespace llvm;

  char ShadowThreads::ID = 0;

  unsigned ShadowThreads::getThreadId (const Function *n)
  {
    auto it = m_thread_ids.find (n);
    if (it != m_thread_ids.end ()) return it->second;
    unsigned id = m_thread_ids.size ();
    m_thread_ids[n] = id;
    return id;
  }

  unsigned ShadowThreads::getSharedVarId (const Value *n)
  {
    auto it = m_shared_ids.find (n);
    if (it != m_shared_ids.end ()) return it->second;
    unsigned id = m_shared_ids.size ();
    m_shared_ids[n] = id;
    return id;
  }

  struct SharedVarVisitor : 
      public InstVisitor<SharedVarVisitor>
  {
    typedef DenseMap<const Value*, std::set<const Function*> > shared_var_map_t;
    shared_var_map_t& m_shared_map;

    void insertVar (std::pair< const Value*, const Function*> p)
    {
      auto it = m_shared_map.find (p.first);
      if (it != m_shared_map.end ())
      {  
        std::set<const Function*> &threads = m_shared_map [p.first];
        threads.insert (p.second);
      }
      else
      {
        std::set<const Function*> threads;
        threads.insert (p.second);
        m_shared_map.insert (std::make_pair (p.first, threads));
      }
    }

    SharedVarVisitor (shared_var_map_t &shared_map):
        m_shared_map (shared_map) { }              

    void visitPHINode (PHINode &I) {}
    void visitCmpInst (CmpInst &I) {}
    void visitBranchInst (BranchInst &I) {}
    void visitSelectInst(SelectInst &I) {}
    void visitBinaryOperator(BinaryOperator &I) {}
    void visitTruncInst(TruncInst &I) {}             
    void visitZExtInst (ZExtInst &I) {}

    void visitLoadInst (LoadInst &I) {

       Value* ptr = I.getOperand (0);
       if (isa<GlobalVariable> (ptr))
       {
         if (GlobalValue* gv = dyn_cast<GlobalValue> (ptr))
         {
           if (gv->isThreadLocal ()) 
             return;
         }
         const Function * f = I.getParent ()->getParent ();
         if (!f->getName ().equals ("main"))
           insertVar (std::make_pair (ptr, f));
       }
    }  

    void visitStoreInst (StoreInst &I) {

       Value* ptr = I.getOperand (1);
       if (isa<GlobalVariable> (ptr))
       {
         if (GlobalValue* gv = dyn_cast<GlobalValue> (ptr))
         {
           if (gv->isThreadLocal ()) 
             return;
         }
         const Function * f = I.getParent ()->getParent ();
         if (!f->getName ().equals ("main"))
           insertVar (std::make_pair (ptr, f));
       }
    }


    void visitReturnInst (ReturnInst &I) {}
    
    void visitCallInst (CallInst &I)  {}

    /// base case. if all else fails.
    void visitInstruction (Instruction &I)
    { }
    
  };


  bool ShadowThreads::runOnModule (llvm::Module &M)
  {
    if (M.begin () == M.end ()) return false;

    bool change=false;

    LLVMContext &ctx = M.getContext ();
    m_threadCreateFn = M.getOrInsertFunction ("shadow.thread.create", 
                                              Type::getVoidTy (ctx),
                                              Type::getInt32Ty (ctx),
                                              //Type::getInt32Ty (ctx),
                                              (Type*) 0);

    for (auto &f : M) 
      change |= runOnFunction (f); 

    // Filter out all the variables that are not shared by two or more
    // threads
    auto it = m_shared_vars.begin ();
    for(; it != m_shared_vars.end (); ) {
      if (it->second.size () < 2) {
        m_shared_vars.erase(it++);
      } 
      else 
        ++it;
    }

    // Rename the shared variable names
    for (auto &p : m_shared_vars)
    {
#ifdef THDEBUG
      errs () << "Global variable " << p.first->getName () << " is shared by {";
      for (auto t : p.second)
        errs () << t->getName () << ";";
      errs () << "}\n";
#endif 
           
      Value *gv = const_cast<Value*> (p.first);
      
#ifdef THDEBUG
      errs () << "Global variable " << gv->getName () << " renamed to ";
#endif

      std::string name = "@shared_var__" + 
          boost::lexical_cast<std::string> (getSharedVarId (p.first));
      gv->setName (name);

#ifdef THDEBUG
      errs () << gv->getName () << "\n";
#endif 
    }

    return change;
  }

  bool ShadowThreads::runOnFunction (llvm::Function &F)
  {
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    LLVMContext &ctx = F.getContext ();
    IRBuilder<> B (ctx);

    bool change = false;
    for (BasicBlock &bb : F)
      for (Instruction &inst : bb)
      {
        if (CallInst* CI = dyn_cast<CallInst> (&inst))
        {
          CallSite CS (&inst);
          if (CS.getCalledFunction ()->getName ().equals ("pthread_create"))
          {
            if (!F.getName ().equals ("main"))
            {
              errs () << "WARNING: skipping a call to thread_create. "
                      << "Only main can create threads\n";
            }
            else
            {

              if (Function* thread = dyn_cast<Function> (CS.getArgument(2)))
              {
                unsigned id = getThreadId (thread);
#ifdef THDEBUG                
                errs () << "Thread " << thread->getName () << " with id " << id << "\n";
#endif 

                // direct call to the thread
                B.SetInsertPoint (&inst);
                B.CreateCall (thread, CS.getArgument (3));

                // shadow call
                B.CreateCall (m_threadCreateFn,
                              B.getInt32 (getThreadId (thread)));

                // remove pthread_create call ??
                
                change = true;
              }
              else 
              {
                errs () << "WARNING: skipping a call to thread_create. "
                        << "Cannot determine statically the function\n";
              }
            }
          }
        }
      }

      for (BasicBlock &bb : F)
      {
        SharedVarVisitor v (m_shared_vars);
        v.visit (&bb);
      }
      
     return change;
  }

  void ShadowThreads::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();
  } 

} // end namespace llvm_ikos


static llvm::RegisterPass<llvm_ikos::ShadowThreads> 
X ("shadow-threads",
   "Shadow thread operations", 
   false, false);


