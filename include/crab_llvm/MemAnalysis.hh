#ifndef __MEMORY_ANALYSIS_HH_
#define __MEMORY_ANALYSIS_HH_

/* Wrapper for a memory analysis*/

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

#include "crab_llvm/config.h"
#include "crab/common/debug.hpp"

#include <set>
#include <boost/unordered_map.hpp>
#include <boost/range/iterator_range.hpp>

namespace crab_llvm {

   template<typename Mem>
   class Region {

     // A region different from unknown should represent a consecutive
     // sequence of bytes in memory that have compatible types and are
     // aligned so the analysis can use it in a safe manner.

     friend class DummyMemAnalysis;
     #ifdef HAVE_DSA
     friend class DSAMemAnalysis;
     #endif 

     Mem* m_mem;
     int m_id;
     
     Region (Mem* mem, int id): m_mem (mem), m_id (id) { }

    public:

     Region (): m_mem (nullptr), m_id (-1) { }
     
     bool isUnknown () const {
       return (m_id < 0);
     }
          
     const llvm::Value* getSingleton () const {
       if (!m_mem) 
         return nullptr;
       else
         return m_mem->getSingleton (m_id);
     }
     
     bool operator<(const Region<Mem> & o) const {
       return (m_id < o.m_id);
     }

     bool operator==(const Region<Mem> & o) const {
       return (m_id == o.m_id);
     }

     int get_id () const {
       return m_id;
     }

     void write (llvm::raw_ostream& o) const {
       if (isUnknown ()) 
         o << "unknown";
       else
         o << "R_" << m_id;
     }

     friend llvm::raw_ostream& operator<<(llvm::raw_ostream &o, const Region<Mem>& r) {
       r.write (o);
       return o;
     }

   };

   template<typename Mem>
   inline llvm::raw_ostream& operator<<(llvm::raw_ostream &o, std::set<Region<Mem> > s) {
     o << "{";
     for (typename std::set<Region<Mem> >::iterator it=s.begin (), et=s.end (); it!=et; ){
       o << *it;
       ++it;
       if (it != et)
           o << ",";
     }
     o << "}";
     return o;
   }

   class MemAnalysis {
    
     template<typename Any>
     friend class Region;

    protected:

     virtual const llvm::Value* getSingleton (int region) const = 0;
     
    public:

     typedef Region<MemAnalysis> region_t;
     typedef std::set<region_t> region_set_t;

    public:

     MemAnalysis () { }
     
     virtual region_t getRegion (llvm::Function&, llvm::Value*) = 0;

     virtual region_set_t getAccessedRegions (llvm::Function& ) = 0;

     virtual region_set_t getOnlyReadRegions (llvm::Function& ) = 0;
     
     virtual region_set_t getModifiedRegions (llvm::Function& ) = 0;
     
     virtual region_set_t getNewRegions (llvm::Function& ) = 0;
     
     virtual region_set_t getAccessedRegions (llvm::CallInst& ) = 0;
     
     virtual region_set_t getOnlyReadRegions (llvm::CallInst& ) = 0;

     virtual region_set_t getModifiedRegions (llvm::CallInst& ) = 0;
     
     virtual region_set_t getNewRegions (llvm::CallInst& ) = 0;

     virtual const char* getMemAnalysisName () const = 0;
   }; 

   typedef typename MemAnalysis::region_t region_t;
   typedef typename MemAnalysis::region_set_t region_set_t;

   struct DummyMemAnalysis: public MemAnalysis {

     using typename MemAnalysis::region_t;
     using typename MemAnalysis::region_set_t;

     DummyMemAnalysis (): MemAnalysis () { }
     
     const llvm::Value* getSingleton (int) const { return nullptr;}
     
     region_t getRegion (llvm::Function&, llvm::Value*) {
       return region_t ();
     }
               
     region_set_t getAccessedRegions (llvm::Function& ) {
       return region_set_t ();
     }

     region_set_t getOnlyReadRegions (llvm::Function& ) {
       return region_set_t ();
     }
     
     region_set_t getModifiedRegions (llvm::Function& ) {
       return region_set_t ();
     }
     
     region_set_t getNewRegions (llvm::Function& ) {
       return region_set_t ();
     }
     
     region_set_t getAccessedRegions (llvm::CallInst& ) {
       return region_set_t ();
     }

     region_set_t getOnlyReadRegions (llvm::CallInst& ) {
       return region_set_t ();
     }
     
     region_set_t getModifiedRegions (llvm::CallInst& ) {
       return region_set_t ();
     }
     
     region_set_t getNewRegions (llvm::CallInst& )  {
       return region_set_t ();
     }

     const char* getMemAnalysisName () const {
       return "DummyMemAnalysis";
     }

   }; 

} // end namespace

#ifdef HAVE_DSA

#include "boost/range/algorithm/set_algorithm.hpp"

#include "llvm/IR/InstIterator.h"

#include "dsa/DataStructure.h"
#include "dsa/DSGraph.h"
#include "dsa/DSNode.h"

namespace crab_llvm {

  using namespace std;
  using namespace llvm;

  /* 
   *  Each DSA node is translated into a region. DSA partitions the
   *  whole heap into disjoint regions (i.e., contiguous sequence of
   *  bytes).
   */
  class DSAMemAnalysis: public MemAnalysis {

   public:

     using typename MemAnalysis::region_t;
     using typename MemAnalysis::region_set_t;

   private:

    Module& m_M;
    DataStructures*  m_dsa;
    
    /// map from DSNode to ids
    DenseMap<const DSNode*, unsigned> m_node_ids;
    boost::unordered_map<unsigned, const DSNode*> m_rev_node_ids;

    /// reach - all reachable nodes from this function
    std::set<const DSNode*> m_reach;
    /// outReach - subset of reach that is only reachable from the
    ///            return node
    std::set<const DSNode*> m_retReach;
    
    DenseMap<const Function*, region_set_t> m_func_accessed;
    DenseMap<const Function*, region_set_t> m_func_mods;
    DenseMap<const Function*, region_set_t> m_func_news;
    //std::set<const Function*> cached_functions;

    DenseMap<const CallInst*, region_set_t> m_callsite_accessed;
    DenseMap<const CallInst*, region_set_t> m_callsite_mods;
    DenseMap<const CallInst*, region_set_t> m_callsite_news;
    //std::set<const CallInst*> cached_callsites;

   private:

    int getId (const DSNode *n) {
      auto it = m_node_ids.find (n);
      if (it != m_node_ids.end ()) return it->second;
      unsigned id = m_node_ids.size ();
      m_node_ids[n] = id;
      m_rev_node_ids[id] = n;
      return id;
    }
    
    template <typename Set>
    void markReachableNodes (const DSNode *n, Set &set) {
      if (!n) return;
      
      assert (!n->isForwarding () && "Cannot mark a forwarded node");
      if (set.insert (n).second)
        for (auto &edg : boost::make_iterator_range (n->edge_begin (), n->edge_end ()))
          markReachableNodes (edg.second.getNode (), set);
    }
    
    template <typename Set>
    void inputReachableNodes (const DSCallSite &cs, DSGraph &dsg, Set &set) {
      markReachableNodes (cs.getVAVal().getNode (), set);
      if (cs.isIndirectCall ()) markReachableNodes (cs.getCalleeNode (), set);
      for (unsigned i = 0, e = cs.getNumPtrArgs (); i != e; ++i)
        markReachableNodes (cs.getPtrArg (i).getNode (), set);
      
      // globals
      DSScalarMap &sm = dsg.getScalarMap ();
      for (auto &gv : boost::make_iterator_range (sm.global_begin(), sm.global_end ()))
        markReachableNodes (sm[gv].getNode (), set);
    }
  
    template <typename Set>
    void retReachableNodes (const DSCallSite &cs, Set &set) 
    {markReachableNodes (cs.getRetVal ().getNode (), set);}
    
    template <typename Set>
    void set_difference (Set &s1, Set &s2) {
      Set s3;
      boost::set_difference (s1, s2, std::inserter (s3, s3.end ()));
      std::swap (s3, s1);
    }
    
    template <typename Set>
    void set_union (Set &s1, Set &s2) {
      Set s3;
      boost::set_union (s1, s2, std::inserter (s3, s3.end ()));
      std::swap (s3, s1);
    }
  
    /// Computes DSNode reachable from the call arguments
    /// reach - all reachable nodes
    /// outReach - subset of reach that is only reachable from the return node
    template <typename Set1, typename Set2>
    void argReachableNodes (DSCallSite CS, DSGraph &dsg, 
                            Set1 &reach, Set2 &outReach) {
      inputReachableNodes (CS, dsg, reach);
      retReachableNodes (CS, outReach);
      set_difference (outReach, reach);
      set_union (reach, outReach);
    }

    template <typename Set1, typename Set2>
    void argReachableNodes (Function&f, Set1 &reach, Set2 &outReach) {
      DSGraph *cdsg = m_dsa->getDSGraph (f);
      if (cdsg) {
        DSCallSite CCS = cdsg->getCallSiteForArguments (f);
        argReachableNodes (CCS, *cdsg, reach, outReach);
      }
    }

    bool isTrackable (const DSNode* n) const {

      if (n->isNodeCompletelyFolded()) {
        CRAB_LOG ("mem-analysis", 
                  llvm::errs () << "\tThe node is not trackable because already folded.\n";);
        return false;
      }

      if (n->isExternalNode() || n->isIncompleteNode()) {
        CRAB_LOG ("mem-analysis", 
                  llvm::errs () << "\tThe node is external or incomplete.\n";);
        //return false;
      }

      if (n->isUnknownNode() || n->isIntToPtrNode() || n->isPtrToIntNode()) {
        CRAB_LOG ("mem-analysis", 
                  llvm::errs () << "\tThe node is unknown or casted from/to an integer.\n";);
        return false;
      }

      //  A node is collapsed if all its uses have incompatible type
      //  or compatible types but misaligned.
      if (n->isCollapsedNode ())  {
        CRAB_LOG ("mem-analysis", 
                  llvm::errs () << "\tThe node has been collapsed.\n";);
        return false;
      }
 
      bool r = (n->isAllocaNode () || n->isHeapNode () || n->isGlobalNode ());
      if (r)
        CRAB_LOG ("mem-analysis", 
                  llvm::errs () << "\tThe node is trackable!\n";);
      else
        CRAB_LOG ("mem-analysis", 
                  llvm::errs () << "\tThe node is untrackable (unknown reason)\n";);
      
      return r;
    }

    // compute and cache the set of read, mod and new nodes of a whole
    // function such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodes (Function& f) {

      if (!m_dsa) return;

      // hook: skip shadow mem functions created by SeaHorn
      // We treat them as readnone functions
      if (f.getName().startswith ("shadow.mem")) return;

      std::set<const DSNode*> reach, retReach;
      argReachableNodes (f, reach, retReach);

      CRAB_LOG("mem-analysis", llvm::errs () << f.getName() << "\n");
          
      region_set_t reads, mods, news;
      for (const DSNode* n : reach) {

        CRAB_LOG ("mem-analysis", n->dump(););

        if (!isTrackable (n)) {
          continue;
        }

        if (!n->isReadNode () && !n->isModifiedNode ())
          continue;

        if ((n->isReadNode () || n->isModifiedNode ()) && retReach.count (n) <= 0) {
          CRAB_LOG ("mem-analysis", llvm::errs () << "\tThe node is read\n";);
          reads.insert (region_t (static_cast<MemAnalysis*> (this), getId (n)));
        }
      
        if (n->isModifiedNode () && retReach.count (n) <= 0) {
          CRAB_LOG ("mem-analysis", llvm::errs () << "\tThe node is modified\n";);
          mods.insert (region_t (static_cast<MemAnalysis*> (this), getId (n)));
        }

        if (n->isModifiedNode () && retReach.count (n)) {
          CRAB_LOG ("mem-analysis", llvm::errs () << "\tThe node is created within the function\n";);
          news.insert (region_t (static_cast<MemAnalysis*> (this), getId (n)));
        }
      }
      CRAB_LOG("mem-analysis", llvm::errs () << "-----------------------\n");

      m_func_accessed [&f] = reads;
      m_func_mods [&f] = mods;
      m_func_news [&f] = news;
    }

    // Compute and cache the set of read, mod and new nodes of a
    // callsite such that mod nodes are a subset of the read nodes and
    // the new nodes are disjoint from mod nodes.
    void cacheReadModNewNodesFromCallSite (CallInst& I) {
      
      if (!m_dsa) return;

      /// ignore inline assembly
      if (I.isInlineAsm ())
        return; 
      
      // hook: skip shadow mem functions created by SeaHorn
      // We treat them as readnone functions
      if (Function* Called = CallSite(&I).getCalledFunction ())
        if (Called->getName ().startswith ("shadow.mem"))
          return;
        
      DSGraph *dsg = m_dsa->getDSGraph (*(I.getParent ()->getParent ()));
      DSCallSite CS = dsg->getDSCallSiteForCallSite (CallSite (&I));
      
      if (!CS.isDirectCall ()) 
        return; 
      
      if (!m_dsa->hasDSGraph (*CS.getCalleeFunc ())) 
        return; 
          
      const Function &CF = *CS.getCalleeFunc ();
      DSGraph *cdsg = m_dsa->getDSGraph (CF);
      if (!cdsg) 
        return; 
      
      // -- compute callee nodes reachable from arguments and returns
      DSCallSite CCS = cdsg->getCallSiteForArguments (CF);
      std::set<const DSNode*> reach;
      std::set<const DSNode*> retReach;
      argReachableNodes (CCS, *cdsg, reach, retReach);
      //DSGraph::NodeMapTy nodeMap;
      //dsg->computeCalleeCallerMapping (CS, CF, *cdsg, nodeMap);
      
      region_set_t reads, mods, news;
      for (const DSNode* n : reach) {

        if (!isTrackable (n))
          continue;

        if (!n->isReadNode () && !n->isModifiedNode ())
          continue;

        if ((n->isReadNode () || n->isModifiedNode ()) && retReach.count (n) <= 0) 
          reads.insert (region_t (static_cast<MemAnalysis*> (this), getId (n)));
      
        if (n->isModifiedNode () && retReach.count (n) <= 0)
          mods.insert (region_t (static_cast<MemAnalysis*> (this), getId (n)));

        if (n->isModifiedNode () && retReach.count (n))
          news.insert (region_t (static_cast<MemAnalysis*> (this), getId (n)));

      }

      // -- add the region of the lhs of the call site
      region_t ret = getRegion (*(I.getParent ()->getParent ()), &I);
      if (!ret.isUnknown ()) 
        mods.insert (ret);

      m_callsite_accessed [&I] = reads; 
      m_callsite_mods [&I] = mods; 
      m_callsite_news [&I] = news; 
    }

    // DSGraph* getGraphForValue(const Value *V) {
    //   if (const Instruction *I = dyn_cast<Instruction>(V))
    //     return m_dsa->getDSGraph(*I->getParent()->getParent());
    //   else if (const Argument *A = dyn_cast<Argument>(V))
    //     return m_dsa->getDSGraph(*A->getParent());
    //   else if (const llvm::BasicBlock *BB = dyn_cast<llvm::BasicBlock>(V))
    //     return m_dsa->getDSGraph(*BB->getParent());
    //   return m_dsa->getDSGraph (*m_M.getFunction("main"));
    //   // return m_dsa->getGlobalsGraph();
    // }

   public:
    
    DSAMemAnalysis (Module& M, DataStructures* dsa)
        : m_M (M), m_dsa (dsa) { 

      // --- Pre-compute all the information per function and
      //     callsites

      CRAB_LOG("mem-analysis", 
               llvm::errs () << "========= MemAnalysis =========\n");

      for (auto &F: boost::make_iterator_range (m_M)) {
        cacheReadModNewNodes (F);

        inst_iterator InstIt = inst_begin(F), InstItEnd = inst_end(F);
        for (; InstIt != InstItEnd; ++InstIt) {
          if (CallInst *Call = dyn_cast<CallInst>(&*InstIt)) {
            cacheReadModNewNodesFromCallSite (*Call);
          }
        }
      }

    }

    // f is used to know in which DSGraph we should search for V
    region_t getRegion (Function& F, Value* V)  {
      // Note each function has its own graph and a copy of the global
      // graph. Nodes in both graphs are merged.  However, m_dsa has
      // its own global graph which seems not to be merged with
      // function's graphs, and thus, it cannot be used here.
      if (!m_dsa) 
        return region_t ();
      
      DSGraph* dsg = m_dsa->getDSGraph (F);
      if (!dsg) 
        return region_t ();

      DSNode* n = dsg->getNodeForValue (V).getNode ();
      if (!n) {
        DSGraph* gDsg = dsg->getGlobalsGraph ();
        n = gDsg->getNodeForValue (V).getNode ();
      }

      if (!n || !isTrackable (n)) 
        return region_t ();
      else
        return region_t (static_cast<MemAnalysis*> (this), getId (n)); 
      
      // DSGraph* dsg = getGraphForValue (V);
      // if (!dsg)
      //   return region_t ();
      // DSNode* n = dsg->getNodeForValue (V).getNode ();
      // if (!n || !isTrackable (n)) 
      //   return region_t ();
      // return region_t (static_cast<MemAnalysis*> (this), getId (n)); 

    }

    //! return a value if the region corresponds to a single-cell
    //  global memory cell, or nullptr otherwise.
    //  TODO: extend to single-cell local pointers.
    const Value* getSingleton (int region) const {
      auto const it = m_rev_node_ids.find (region);
      if (it == m_rev_node_ids.end ()) 
        return nullptr;

      if (const Value* v = it->second->getUniqueScalar ()) {
        if (const GlobalVariable *gv = dyn_cast<const GlobalVariable> (v))
          if (gv->getType ()->getElementType ()->isSingleValueType ())
            return v;
      }
      return nullptr;
    }

    // read and written regions by the function
    region_set_t getAccessedRegions (llvm::Function& F)  {
      return m_func_accessed [&F];
    }

    // only read regions by the function
    region_set_t getOnlyReadRegions (llvm::Function& F)  {
      region_set_t s1 = m_func_accessed [&F];
      region_set_t s2 = m_func_mods [&F];
      set_difference (s1,s2);
      return s1;
    }
     
    // written regions by the function
    region_set_t getModifiedRegions (llvm::Function& F) {
      return m_func_mods [&F];
    }
    
    // regions that are reachable only from the return of the function
    region_set_t getNewRegions (llvm::Function& F) {
      return m_func_news [&F];
    }

    // read and written regions by the callee
    region_set_t getAccessedRegions (llvm::CallInst& I) {
      return m_callsite_accessed [&I];
    }

    // only read regions by the function
    region_set_t getOnlyReadRegions (llvm::CallInst& I)  {
      region_set_t s1 = m_callsite_accessed [&I];
      region_set_t s2 = m_callsite_mods [&I];
      set_difference (s1,s2);
      return s1;
    }
    
    // written regions by the callee
    region_set_t getModifiedRegions (llvm::CallInst& I) {
      return m_callsite_mods [&I];
    }
    
    // regions that are reachable only from the return of the callee
    region_set_t getNewRegions (llvm::CallInst& I)  {
      return m_callsite_news [&I];
    }

    const char* getMemAnalysisName () const {
       return "DSAMemAnalysis";
    }

  }; 
} // end namespace
#endif /* HAVE_DSA */
#endif /*__MEMORY_ANALYSIS_HH_*/
