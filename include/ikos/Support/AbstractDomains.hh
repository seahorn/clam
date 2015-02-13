#ifndef __ABSTRACT_DOMAINS_HH_
#define __ABSTRACT_DOMAINS_HH_

/// IO support for abstract domains
#include <ikos_domains/linear_constraints.hpp>                      
#include <ikos_domains/intervals.hpp>                      
#include <ikos_domains/intervals_congruences.hpp>                      
#include <ikos_domains/octagons.hpp>                      
#include <ikos_domains/dbm.hpp>                      

namespace
{

  using namespace std;

  template <typename Number, typename VariableName>
  llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                 ikos::linear_expression<Number,VariableName>& e)
  {
    ostringstream s;
    s << e;
    o << s.str ();
    return o;
  }


  template <typename Number, typename VariableName>
  llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                 ikos::linear_constraint<Number,VariableName>& cst)
  {
    ostringstream s;
    s << cst;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                 ikos::linear_constraint_system<Number,VariableName>& csts)
  {
    ostringstream s;
    s << csts;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                 ikos::interval_domain<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                 ikos::interval_congruence_domain<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  llvm::raw_ostream& operator<< (llvm::raw_ostream& o, ikos::DBM<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  llvm::raw_ostream& operator<< (llvm::raw_ostream& o, ikos::octagon<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

} // end namespace

#endif 
