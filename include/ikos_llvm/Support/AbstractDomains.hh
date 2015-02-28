#ifndef __ABSTRACT_DOMAINS_HH_
#define __ABSTRACT_DOMAINS_HH_

/// IO support for abstract domains
#include <ikos/linear_constraints.hpp>                      
#include <ikos/intervals.hpp>                      
#include <ikos_llvm/config.h>

#if IKOS_MINOR_VERSION >= 2
#include <ikos/domains/intervals_congruences.hpp>                      
#include <ikos/domains/octagons.hpp>                      
#include <ikos/domains/dbm.hpp>                      
#endif 

namespace llvm {}

namespace llvm
{

  using namespace std;

  template <typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        ikos::linear_expression<Number,VariableName>& e)
  {
    ostringstream s;
    s << e;
    o << s.str ();
    return o;
  }


  template <typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        ikos::linear_constraint<Number,VariableName>& cst)
  {
    ostringstream s;
    s << cst;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        ikos::linear_constraint_system<Number,VariableName>& csts)
  {
    ostringstream s;
    s << csts;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        ikos::interval_domain<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }
  
#if IKOS_MINOR_VERSION >= 2
  template <typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        ikos::interval_congruence_domain<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, ikos::DBM<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, ikos::octagon<Number,VariableName>& inv)
  {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }
#endif

} // end namespace

#endif 
