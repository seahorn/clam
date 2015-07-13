#ifndef __MEMORY_ANALYSIS_HH_
#define __MEMORY_ANALYSIS_HH_

/* Wrapper for a datastructure analysis*/

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"

#include "ikos_llvm/config.h"

#include <ikos/cfg/Cfg.hpp>

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

    template<typename First, typename Second>
    struct getSecond : public std::unary_function<pair<First,Second>, Second>
    {
      getSecond () { }
      Second operator () (const pair<First,Second> &p) const { return p.second; }
    }; 

    typedef boost::transform_iterator< getSecond<const DSNode*,unsigned>, 
                                       typename DenseMap<const DSNode*, 
                                                         unsigned>::iterator > iterator;

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

    //! return begin iterator to node id's
    iterator begin () {       
        return boost::make_transform_iterator 
            (m_node_ids.begin (), 
             getSecond<const DSNode*,unsigned> ());
    }

    //! return end iterator to node id's
    iterator end () {       
        return boost::make_transform_iterator 
            (m_node_ids.end (), 
             getSecond<const DSNode*,unsigned> ());
    }

    
   public:
    
    MemAnalysis ():  m_dsa (0), m_tracklev (REG) { }

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

    //! return all the arrays that must be allocated at the
    //! beginning of the function.
    set<array_id_t> getAllocArrays (Function& f)
    {
      std::set<const DSNode*> reach, retReach;
      argReachableNodes (f, reach, retReach);
      
      set<array_id_t> inits;
      for (auto node_id : boost::make_iterator_range (begin (), end ()))
      {
        if (reach.count (m_rev_node_ids [node_id]) <= 0)
        { // -- does not escape the function
          inits.insert (node_id);
        }
      }
      for (const DSNode* n : reach)
      {
        if (!n->isReadNode () && !n->isModifiedNode ()) 
          continue;

        if (n->isModifiedNode () &&retReach.count (n))
            inits.insert (getId (n));
      }
      return inits;
    }
    

    //! return all the in/out arrays for the function
    pair<set<array_id_t>,set<array_id_t> > getInOutArrays (Function& f)
    {
      std::set<const DSNode*> reach, retReach;
      argReachableNodes (f, reach, retReach);

      set<array_id_t> in, out;
      for (const DSNode* n : reach)
      {
        // n is read and is not only return-node reachable (for
        // return-only reachable nodes, there is no initial value
        // because they are created within this function)
        if ((n->isReadNode () || n->isModifiedNode ()) 
            && retReach.count (n) <= 0)
          in.insert (getId (n));
      
        if (n->isModifiedNode ())
          out.insert (getId (n));
      }
      return make_pair (in,out);
    }

    //! return the set of read-only and read/write arrays by the call
    pair<set<array_id_t>,set<array_id_t> >
    getReadModArrays (CallInst& I)
    {
      set<array_id_t> reads_only;
      set<array_id_t> read_writes;

      /// ignore inline assembly
      if (I.isInlineAsm ()) return make_pair (reads_only, read_writes);
      
      DSGraph *dsg = m_dsa->getDSGraph (*(I.getParent ()->getParent ()));
      DSCallSite CS = dsg->getDSCallSiteForCallSite (CallSite (&I));
      
      // TODO: resolve the indirect call
      if (!CS.isDirectCall ()) 
        return make_pair (reads_only, read_writes); 
      
      if (!m_dsa->hasDSGraph (*CS.getCalleeFunc ())) 
        return make_pair (reads_only, read_writes);
          
      const Function &CF = *CS.getCalleeFunc ();
      DSGraph *cdsg = m_dsa->getDSGraph (CF);
      if (!cdsg) 
        return make_pair (reads_only, read_writes);
      
      // -- compute callee nodes reachable from arguments and returns
      DSCallSite CCS = cdsg->getCallSiteForArguments (CF);
      std::set<const DSNode*> reach;
      std::set<const DSNode*> retReach;
      argReachableNodes (CCS, *cdsg, reach, retReach);
      DSGraph::NodeMapTy nodeMap;
      dsg->computeCalleeCallerMapping (CS, CF, *cdsg, nodeMap);
      
      /// classify nodes as mod, ref, new, based on whether the remote
      /// node reads, writes, or creates the corresponding node.
      for (const DSNode* n : reach)
      {
        // skip nodes that are not read/written by the callee
        if (!n->isReadNode () && !n->isModifiedNode ()) 
          continue;

        // -- read only node
        if (n->isReadNode () && !n->isModifiedNode ())
          reads_only.insert (getId (n)); 

        // -- read/write or new node
        else if (n->isModifiedNode ())
        {
          if (retReach.count (n) <= 0)
            read_writes.insert (getId (n));          
          // -- n is new node iff it is reachable only from the return
          // node. We ignore the new nodes
        }
      }
      // -- add the node of the lhs of the call site
      int ret = getArrayId (*(I.getParent ()->getParent ()), &I);
      if (ret >=0) read_writes.insert (ret);

      return make_pair (reads_only, read_writes);
    }
    
  }; 

} // end namespace
#else
namespace llvm_ikos
{
  using namespace cfg;
  struct MemAnalysis
  {
    TrackedPrecision m_tracklev;
    MemAnalysis (): m_tracklev (REG) { }
    TrackedPrecision getTrackLevel () const { return m_tracklev; }
    int getArrayId (llvm::Function&, llvm::Value*) { return -1; }
    bool isSingleton (int) { return false; }
    set<int> getAllocArrays (llvm::Function&) { 
      return set<int> ();  
    }
    pair<set<int>,set<int> > getInOutArrays (llvm::Function&) {  
      return make_pair (set<int> (), set<int> ()); 
    } 
    pair<set<int>,set<int> > getReadModArrays (llvm::CallInst&) {  
      return make_pair (set<int> (), set<int> ()); 
    } 
  }; 
} // end namespace
#endif 

#endif /*__MEMORY_ANALYSIS_HH_*/
