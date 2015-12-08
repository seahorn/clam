#include "llvm/Pass.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/CFG.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

#include "crab_llvm/config.h"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/SymEval.hh"
#include "crab_llvm/ConCrabLlvm.hh"
#include "crab_llvm/AbstractDomains.hh"

#include <crab/cfg/ConcSys.hpp>
#include <crab/analysis/ConcAnalyzer.hpp>
#include <crab/domains/domain_traits.hpp>

#ifdef HAVE_DSA
#include "dsa/Steensgaard.hh"
#endif 

/* 
   user-definable options in CrabLlvm.cc 
*/
extern llvm::cl::opt<bool> LlvmCrabPrintAns;
extern llvm::cl::opt<crab_llvm::CrabDomain> LlvmCrabDomain;
extern llvm::cl::opt<bool> LlvmCrabLive;
extern llvm::cl::opt<enum crab::cfg::TrackedPrecision> LlvmCrabTrackLev;
extern llvm::cl::opt<unsigned int> LlvmCrabWideningThreshold;
extern llvm::cl::opt<unsigned int> LlvmCrabNarrowingIters;
extern llvm::cl::opt<unsigned int> LlvmCrabWideningJumpSet;

using namespace llvm;

namespace crab_llvm
{
  inline llvm::raw_ostream& operator<< 
  (llvm::raw_ostream& o, 
   crab::conc::ConcSys< llvm::Function*, crab::cfg_impl::cfg_t> &sys)
  {
    std::ostringstream s;
    s << sys;
    o << s.str ();
    return o;
  }
}

namespace crab { namespace conc_impl {
  template<> inline std::string get_thread_id_str(llvm::Function *f) 
  { return f->getName (); }
}}

namespace crab_llvm
{
  using namespace crab::conc;

  typedef ConcSys< llvm::Function*, cfg_t> conc_sys_t;
  typedef SymEval<VariableFactory, crab::cfg_impl::z_lin_exp_t> sym_eval_t;
  typedef boost::optional<crab::cfg_impl::z_lin_exp_t> z_lin_exp_opt_t;

  char crab_llvm::ConCrabLlvm::ID = 0;

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

    /// base case. if all else fails.
    void visitInstruction (Instruction &I){}
    
  };

  z_lin_exp_opt_t findInitialValue (Module &M, StringRef name, 
                                    sym_eval_t sym_eval)    
  {
    GlobalVariable * gv = M.getGlobalVariable (name);
    if (!gv || !gv->hasInitializer ()) 
      return z_lin_exp_opt_t ();
    
    PointerType *ty = dyn_cast<PointerType> (gv->getType ());
    if (!ty) 
      return z_lin_exp_opt_t ();
    Type *ety = ty->getElementType ();
    // only deal with scalars for now
    if (!ety->isIntegerTy () &&  !ety->isPointerTy ()) 
      return z_lin_exp_opt_t ();
    
    return sym_eval.lookup (*(cast<const Value>(gv->getInitializer ())));
  }

  template<typename AbsDomain>
  void analyzeConcSys (conc_sys_t& sys, VariableFactory& vfac, 
                       Module& M, MemAnalysis& mem, const DataLayout* dl)
  {
    typedef ConcAnalyzer <llvm::Function*, cfg_t, 
                          AbsDomain, VariableFactory> conc_analyzer_t;


    /// --- Initialize shared global state
    AbsDomain init_gv_inv = AbsDomain::top ();
    sym_eval_t sym_eval (vfac, &mem);
    for (auto p: sys)
    {
      // TODO: we should add only the initialization of the global
      // variables that can be accessed by the thread and its callees.
      for (GlobalVariable &gv : boost::make_iterator_range (M.global_begin (),
                                                            M.global_end ()))
      {
        if (z_lin_exp_opt_t val = findInitialValue (M, gv.getName (), sym_eval))
        { 
          z_lin_exp_opt_t idx = sym_eval.lookup (gv);
          if (!idx || !(*idx).get_variable ()) continue;

          Value * gv_value = M.getNamedValue (gv.getName ());
          assert (gv_value);

          int arr_id = mem.getArrayId (*(p.first), gv_value); 
          if (arr_id < 0) continue;

          crab::domain_traits::array_store (init_gv_inv,
                                            sym_eval.symVar (arr_id), 
                                            (*((*idx).get_variable ())).name (), 
                                            *val, 
                                            ikos::z_number (dl->getTypeAllocSize (gv.getType ())),
                                            mem.getSingleton (arr_id) != nullptr);  
        }
      }
    }

    if (LlvmCrabPrintAns)
    {
      errs () << "=========================\n";
      errs () << "Concurrent system\n";
      errs () << "=========================\n";
      errs () << sys << "\n";
      errs () << "Initial global state: " << init_gv_inv << "\n";
    }

    /// --- Global fixpoint 
    conc_analyzer_t a (sys, vfac, LlvmCrabLive, 
                       LlvmCrabWideningThreshold, LlvmCrabNarrowingIters,
                       LlvmCrabWideningJumpSet);

    a.Run (init_gv_inv);
    
    if (LlvmCrabPrintAns)
    {
      errs () << "=========================\n";
      errs () << "Invariants\n";
      errs () << "=========================\n";
      for (auto t : sys)
      {
        errs () << crab::conc_impl::get_thread_id_str (t.first) << "\n";
        auto &inv_map = a.getInvariants (t.first);
        for (auto p: inv_map)
          errs () << "\t" << crab::cfg_impl::get_label_str (p.first) << ": " 
                  << p.second << "\n";
        errs () << "\n";
      }
    }
  }

  bool ConCrabLlvm::runOnModule (llvm::Module &M)
  {

#ifdef HAVE_DSA
    m_mem = MemAnalysis (&getAnalysis<SteensgaardDataStructures> (), ARR);
#endif     

    /// --- Identify threads and shared global variables
    bool change = false;
    for (auto &f : M) 
      change |= processFunction (f); 
#if 0
    // Filter out variables that are not shared by two or more threads
    auto it = m_shared_vars.begin ();
    for(; it != m_shared_vars.end (); ) {
      if (it->second.size () < 2) 
        m_shared_vars.erase(it++);     
      else 
        ++it;
    }
#endif 

    conc_sys_t sys;
    VariableFactory vfac;

    /// --- Build the system with all the threads and their shared
    /// --- variables
    sym_eval_t sym_eval (vfac, &m_mem);
    for (auto f : m_threads)
    {
      CfgBuilder builder (*f, vfac, &m_mem, true);
      auto cfg = builder.makeCfg ();
                           
      vector<varname_t> shared_vars;
      for (auto v : m_shared_vars)
      {
        /// --- we must pass the array associated to the global
        ///     variable.
        int arr_id = m_mem.getArrayId (*f, const_cast<Value*> (v.first));
        if (arr_id < 0) continue;
        shared_vars.push_back (sym_eval.symVar (arr_id));
      }

      sys.add_thread (f, cfg, shared_vars.begin (), shared_vars.end ());
    }

    const DataLayout* dl = M.getDataLayout ();

    /// --- Analyze the concurrent system
    switch (LlvmCrabDomain)
    {
      case INTERVALS_CONGRUENCES: 
        if (LlvmCrabTrackLev == ARR)
          analyzeConcSys <arr_ric_domain_t> (sys, vfac, M, m_mem, dl); 
        else
          analyzeConcSys <ric_domain_t> (sys, vfac, M, m_mem, dl); 
        break;
      case ZONES: 
        if (LlvmCrabTrackLev == ARR)
          analyzeConcSys <arr_dbm_domain_t> (sys, vfac, M, m_mem, dl); 
        else
          analyzeConcSys <dbm_domain_t> (sys, vfac, M, m_mem, dl); 
        break;
      case TERMS: /*TODO*/
      case BOXES: /*TODO*/
        std::cout << "Warning: abstract domain not found."
                  << "Running intervals ...\n"; 
      case INTERVALS:  
      default:
        if (LlvmCrabTrackLev == ARR)
          analyzeConcSys <arr_interval_domain_t> (sys, vfac, M, m_mem, dl); 
        else
          analyzeConcSys <interval_domain_t> (sys, vfac, M, m_mem, dl); 
    }
    return change;
  }

  //! Identify threads and shared global variables
  bool ConCrabLlvm::processFunction (llvm::Function &F)
  {
    // -- skip functions without a body
    if (F.isDeclaration () || F.empty ()) return false;

    bool change = false;
    for (llvm::BasicBlock &bb : F)
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
                m_threads.insert (thread);
              else 
              {
                errs () << "WARNING: skipping a call to thread_create. "
                        << "Cannot determine statically the function\n";
              }
            }
          }
        }
      }

    for (llvm::BasicBlock &bb : F)
    {
      SharedVarVisitor v (m_shared_vars);
      v.visit (&bb);
    }
      
    return change;
  }

  void ConCrabLlvm::getAnalysisUsage (llvm::AnalysisUsage &AU) const
  {
    AU.setPreservesAll ();
#ifdef HAVE_DSA
    AU.addRequiredTransitive<llvm::SteensgaardDataStructures> ();
    AU.addRequired<llvm::DataLayoutPass>();
    AU.addRequired<llvm::UnifyFunctionExitNodes> ();
#endif 
  } 

} // end namespace 


static llvm::RegisterPass<crab_llvm::ConCrabLlvm> 
X ("con-crab-llvm",
   "Infer invariants for concurrent programs using Crab", 
   false, false);


