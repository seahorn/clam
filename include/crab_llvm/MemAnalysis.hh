#ifndef __MEMORY_ANALYSIS_HH_
#define __MEMORY_ANALYSIS_HH_

/* Wrapper for a datastructure analysis*/

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"

#include "boost/tuple/tuple.hpp"

#include "crab_llvm/config.h"
#include <crab/cfg/Cfg.hpp>

#ifdef HAVE_DSA
#include "boost/range/algorithm/set_algorithm.hpp"
#include "dsa/DataStructure.h"
#include "dsa/DSGraph.h"
#include "dsa/DSNode.h"

namespace crab_llvm
{
  using namespace std;
  using namespace llvm;
  using namespace crab::cfg;

  class MemAnalysis
  {
    /* 
     *  The goal of the memory analysis is to split the whole heap into
     *  disjoint arrays (i.e., contiguous sequence of bytes)
     *
     *  Each DSA node is translated into an array. A DSA node may not
     *  correspond directly to a llvm Value so we map DSA node to
     *  id's.
     */

   public:

    typedef int array_id_t; // if < 0 then error

   private:

    DataStructures*  m_dsa;
    TrackedPrecision m_tracklev;
    
    /// map from DSNode to ids
    DenseMap<const DSNode*, unsigned> m_node_ids;
    boost::unordered_map<unsigned, const DSNode*> m_rev_node_ids;

    /// reach - all reachable nodes from this function
    std::set<const DSNode*> m_reach;
    /// outReach - subset of reach that is only reachable from the
    ///            return node
    std::set<const DSNode*> m_retReach;

    // template<typename First, typename Second>
    // struct getSecond : public std::unary_function<pair<First,Second>, Second> {
    //   getSecond () { }
    //   Second operator () (const pair<First,Second> &p) const { return p.second; }
    // }; 

    // typedef boost::transform_iterator< getSecond<const DSNode*,unsigned>, 
    //                                    typename DenseMap<const DSNode*, 
    //                                                      unsigned>::iterator > iterator;

    array_id_t getId (const DSNode *n)
    {
      auto it = m_node_ids.find (n);
      if (it != m_node_ids.end ()) return it->second;
      unsigned id = m_node_ids.size ();
      m_node_ids[n] = id;
      m_rev_node_ids[id] = n;
      return id;
    }

    template <typename Set>
    void markReachableNodes (const DSNode *n, Set &set)
    {
      if (!n) return;
      
      assert (!n->isForwarding () && "Cannot mark a forwarded node");
      if (set.insert (n).second)
        for (auto &edg : boost::make_iterator_range (n->edge_begin (), n->edge_end ()))
          markReachableNodes (edg.second.getNode (), set);
    }
    
    template <typename Set>
    void inputReachableNodes (const DSCallSite &cs, DSGraph &dsg, Set &set)
    {
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
    void set_difference (Set &s1, Set &s2)
    {
      Set s3;
      boost::set_difference (s1, s2, std::inserter (s3, s3.end ()));
      std::swap (s3, s1);
    }
    
    template <typename Set>
    void set_union (Set &s1, Set &s2)
    {
      Set s3;
      boost::set_union (s1, s2, std::inserter (s3, s3.end ()));
      std::swap (s3, s1);
    }
  
    /// Computes DSNode reachable from the call arguments
    /// reach - all reachable nodes
    /// outReach - subset of reach that is only reachable from the return node
    template <typename Set1, typename Set2>
    void argReachableNodes (DSCallSite CS, DSGraph &dsg, 
                            Set1 &reach, Set2 &outReach)
    {
      inputReachableNodes (CS, dsg, reach);
      retReachableNodes (CS, outReach);
      set_difference (outReach, reach);
      set_union (reach, outReach);
    }

    template <typename Set1, typename Set2>
    void argReachableNodes (Function&f, Set1 &reach, Set2 &outReach)
    {
      DSGraph *cdsg = m_dsa->getDSGraph (f);
      if (cdsg)
      {
        DSCallSite CCS = cdsg->getCallSiteForArguments (f);
        argReachableNodes (CCS, *cdsg, reach, outReach);
      }
    }

    // //! return begin iterator to node id's
    // iterator begin () {       
    //     return boost::make_transform_iterator 
    //         (m_node_ids.begin (), 
    //          getSecond<const DSNode*,unsigned> ());
    // }

    // //! return end iterator to node id's
    // iterator end () {       
    //     return boost::make_transform_iterator 
    //         (m_node_ids.end (), 
    //          getSecond<const DSNode*,unsigned> ());
    // }

    
   public:
    
    MemAnalysis ():  m_dsa (0), m_tracklev (INT) { }

    MemAnalysis (DataStructures * dsa, 
                 TrackedPrecision tracklev): 
        m_dsa (dsa), m_tracklev (tracklev) { }
   
    TrackedPrecision getTrackLevel () const { return m_tracklev; }
    
    //! return < 0 if no array associated with ptr is found
    array_id_t getArrayId (Function& f, Value* ptr) 
    {
      if (!m_dsa) return -1;

      DSGraph* dsg = m_dsa->getDSGraph (f);
      if (!dsg) return -1;

      DSGraph* gDsg = dsg->getGlobalsGraph ();
      DSNode* n = dsg->getNodeForValue (ptr).getNode ();
      if (!n) n = gDsg->getNodeForValue (ptr).getNode ();

      if (!n) return -1;

      return getId (n); 
    }

    //! return true if the array corresponds to a single memory cell.
    bool isSingleton (array_id_t array_id)
    {
      auto it = m_rev_node_ids.find (array_id);
      if (it == m_rev_node_ids.end ()) return false;

      return it->second->getUniqueScalar ();
    }

    // Return the set of ref, mod and new nodes such that mod nodes
    // are a subset of the ref nodes and the new nodes are disjoint
    // from mod nodes.
    boost::tuple< set<array_id_t>, set<array_id_t>, set<array_id_t> > 
    getRefModNewArrays (Function& f) {
      std::set<const DSNode*> reach, retReach;
      argReachableNodes (f, reach, retReach);

      set<array_id_t> refs, mods, news;
      for (const DSNode* n : reach) {
        if (!n->isReadNode () && !n->isModifiedNode ())
          continue;

        if ((n->isReadNode () || n->isModifiedNode ()) && retReach.count (n) <= 0) 
          refs.insert (getId (n));
      
        if (n->isModifiedNode () && retReach.count (n) <= 0)
          mods.insert (getId (n));

        if (n->isModifiedNode () && retReach.count (n))
          news.insert (getId (n));
      }
      return boost::make_tuple (refs, mods, news);
    }

    // Return the set of ref, mod and new nodes such that mod nodes
    // are a subset of the ref nodes and the new nodes are disjoint
    // from mod nodes.
    boost::tuple< set<array_id_t>, set<array_id_t>, set<array_id_t> > 
    getRefModNewArrays (CallInst& I) {
      set<array_id_t> refs, mods, news;

      /// ignore inline assembly
      if (I.isInlineAsm ())
        return boost::make_tuple (refs, mods, news); 
      
      DSGraph *dsg = m_dsa->getDSGraph (*(I.getParent ()->getParent ()));
      DSCallSite CS = dsg->getDSCallSiteForCallSite (CallSite (&I));
      
      if (!CS.isDirectCall ()) 
        return boost::make_tuple (refs, mods, news); 
      
      if (!m_dsa->hasDSGraph (*CS.getCalleeFunc ())) 
        return boost::make_tuple (refs, mods, news); 
          
      const Function &CF = *CS.getCalleeFunc ();
      DSGraph *cdsg = m_dsa->getDSGraph (CF);
      if (!cdsg) 
        return boost::make_tuple (refs, mods, news); 
      
      // -- compute callee nodes reachable from arguments and returns
      DSCallSite CCS = cdsg->getCallSiteForArguments (CF);
      std::set<const DSNode*> reach;
      std::set<const DSNode*> retReach;
      argReachableNodes (CCS, *cdsg, reach, retReach);
      DSGraph::NodeMapTy nodeMap;
      dsg->computeCalleeCallerMapping (CS, CF, *cdsg, nodeMap);
      
      for (const DSNode* n : reach) {
        if (!n->isReadNode () && !n->isModifiedNode ())
          continue;

        if ((n->isReadNode () || n->isModifiedNode ()) && retReach.count (n) <= 0) 
          refs.insert (getId (n));
      
        if (n->isModifiedNode () && retReach.count (n) <= 0)
          mods.insert (getId (n));

        if (n->isModifiedNode () && retReach.count (n))
          news.insert (getId (n));

      }
      // -- add the node of the lhs of the call site
      int ret = getArrayId (*(I.getParent ()->getParent ()), &I);
      if (ret >=0) 
        mods.insert (ret);

      return boost::make_tuple (refs, mods, news); 
    }
    
  }; 

} // end namespace
#else
namespace crab_llvm
{
  using namespace crab::cfg;
  struct MemAnalysis {
    typedef int array_id_t; 
    TrackedPrecision m_tracklev;
    MemAnalysis (): m_tracklev (INT) { }
    TrackedPrecision getTrackLevel () const { return m_tracklev; }
    int getArrayId (llvm::Function&, llvm::Value*) { return -1; }
    bool isSingleton (int) { return false; }
    boost::tuple<set<int>,set<int>, set<int> > 
    getRefModNewArrays (llvm::Function&) {  
      return boost::make_tuple (set<int> (), set<int> (), set<int> ()); 
    } 
    boost::tuple<set<int>,set<int>, set<int> > 
    getRefModNewArrays (llvm::CallInst&) {  
      return boost::make_tuple (set<int> (), set<int> (), set<int> ()); 
    } 
  }; 
} // end namespace
#endif 

#endif /*__MEMORY_ANALYSIS_HH_*/
