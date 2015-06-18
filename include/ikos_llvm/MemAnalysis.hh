#ifndef __MEMORY_ANALYSIS_HH_
#define __MEMORY_ANALYSIS_HH_

/* Wrapper for a datastructure analysis*/

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"

#include "ikos_llvm/config.h"

#ifdef HAVE_DSA
#include "boost/range/algorithm/set_algorithm.hpp"
#include "dsa/DataStructure.h"
#include "dsa/DSGraph.h"
#include "dsa/DSNode.h"

namespace llvm_ikos
{
  using namespace std;
  using namespace llvm;
  using namespace cfg;

  class MemAnalysis
  {

    Function* m_f;
    DataStructures*  m_dsa;
    TrackedPrecision m_tracklev;
    
    DenseMap<const DSNode*, unsigned> m_node_ids;
    boost::unordered_map<unsigned, const DSNode*> m_rev_node_ids;

    /// reach - all reachable nodes from this function
    std::set<const DSNode*> m_reach;
    /// outReach - subset of reach that is only reachable from the return node
    std::set<const DSNode*> m_out_reach;

    template<typename First, typename Second>
    struct getSecond : public std::unary_function<pair<First,Second>, Second>
    {
      getSecond () { }
      Second operator () (const pair<First,Second> &p) const { return p.second; }
    }; 

   typedef boost::transform_iterator< getSecond<const DSNode*,unsigned>, 
                                      typename DenseMap<const DSNode*, unsigned>::iterator > iterator;


    unsigned getId (const DSNode *n)
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
    
   public:
    
    MemAnalysis (Function *f): 
        m_f (f), m_dsa (0), m_tracklev (REG) { }

    MemAnalysis (Function *f, DataStructures * dsa, 
                 TrackedPrecision tracklev): 
        m_f (f), m_dsa (dsa), m_tracklev (tracklev) { 
      DSGraph *cdsg = m_dsa->getDSGraph (*m_f);
      if (!cdsg)
      {
        DSCallSite CCS = cdsg->getCallSiteForArguments (*m_f);
        argReachableNodes (CCS, *cdsg, m_reach, m_out_reach);
      }
    }

    TrackedPrecision getTrackLevel () const { return m_tracklev; }

    //! return begin iterator to node id's
    iterator begin () {       
        return boost::make_transform_iterator (m_node_ids.begin (), 
                                               getSecond<const DSNode*,unsigned> ());
    }

    //! return end iterator to node id's
    iterator end () {       
        return boost::make_transform_iterator (m_node_ids.end (), 
                                               getSecond<const DSNode*,unsigned> ());
    }
    
    //! Return -1 if no node associated with ptr is found
    int getNodeId (Value* ptr) 
    {
      DSGraph* dsg = m_dsa->getDSGraph (*m_f);
      if (!dsg) return -1;

      DSGraph* gDsg = dsg->getGlobalsGraph ();

      DSNode* n = dsg->getNodeForValue (ptr).getNode ();
      if (!n) n = gDsg->getNodeForValue (ptr).getNode ();
      
      if (!n) return -1;
      else return getId (n); 
    }

    bool isSingleton (int node_id)
    {
      auto it = m_rev_node_ids.find (node_id);
      if (it != m_rev_node_ids.end ()) return false;

      return it->second->getUniqueScalar ();
    }

    bool isReachable (int node_id) 
    {      
      return m_reach.count (m_rev_node_ids[node_id]) > 0;
    }

  }; 
} // end namespace
#else
namespace llvm_ikos
{
  using namespace cfg;
  class MemAnalysis
  {
    llvm::Function* m_f;
    TrackedPrecision m_tracklev;
    vector<unsigned> m_node_ids;
   public:
    MemAnalysis (llvm::Function *f): m_f (f), m_tracklev (REG) { }
    TrackedPrecision getTrackLevel () const { return m_tracklev; }
    iterator begin () { return m_node_ids.begin(); }       
    iterator end () { return m_node_ids.end (); }      
    // These methods will be never called anyway
    int getNodeId (Value* ptr) { return -1; }
    bool isSingleton (int node_id) { return false; }
    bool isReachable (int node_id) { return false; }     
  }; 
} // end namespace
#endif 

#endif /*__MEMORY_ANALYSIS_HH_*/
