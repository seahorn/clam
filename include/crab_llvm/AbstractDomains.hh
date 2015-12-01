#ifndef __ABSTRACT_DOMAINS_HH__
#define __ABSTRACT_DOMAINS_HH__

#include "crab_llvm/config.h"
#include "crab_llvm/CfgBuilder.hh"

#include "crab/domains/linear_constraints.hpp"                     
#include "crab/domains/intervals.hpp"                      
#include "crab/domains/intervals_congruences.hpp"
#include "crab/domains/dbm.hpp"
#include "crab/domains/array_smashing.hpp"
#include "crab/domains/term_equiv.hpp"
#include "crab/domains/boxes.hpp"

/*
   Definition of the abstract domains and a generic wrapper class (for
   crab-llvm clients) to contain an arbitrary abstract domain.  
*/

namespace crab_llvm {

  //////
  /// Definition of the abstract domains
  //////

  using namespace crab::cfg_impl;
  using namespace crab::domains;
  using namespace ikos;

  /// --- Types for linear constraints and expressions
  typedef ikos::linear_expression<z_number, varname_t> z_lin_exp_t;
  typedef ikos::linear_constraint<z_number, varname_t> z_lin_cst_t;
  typedef ikos::linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;

  /// -- Intervals
  typedef interval_domain< z_number, varname_t> interval_domain_t;
  /// -- RIC: reduced product of intervals with congruences
  typedef interval_congruence_domain<z_number, varname_t> ric_domain_t;
  /// -- DBM (zones)
  typedef DBM<z_number, varname_t> dbm_domain_t;
  /// -- Terms: reduced product of intervals with uninterpreted functions 
  typedef crab::cfg::var_factory_impl::StrVarAlloc_col::varname_t str_varname_t;
  typedef interval_domain<z_number, str_varname_t> str_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_interval_dom_t /*interval_domain_t*/> idom_info;
  typedef anti_unif<idom_info>::anti_unif_t term_domain_t;  
  /// -- Boxes
  #if 1
  // use the reduced of intervals with boxes rather than plain boxes
  typedef rib_domain<z_number, varname_t> boxes_domain_t;
  #else
  typedef boxes_domain<z_number, varname_t> boxes_domain_t;
  #endif 
  /// -- Array smashing functor domain parameterized with above
  ///    abstract domains
  typedef array_smashing<interval_domain_t,z_number,varname_t> arr_interval_domain_t;
  typedef array_smashing<ric_domain_t,z_number,varname_t> arr_ric_domain_t;
  typedef array_smashing<dbm_domain_t,z_number,varname_t> arr_dbm_domain_t;
  typedef array_smashing<term_domain_t,z_number,varname_t> arr_term_domain_t;
  typedef array_smashing<boxes_domain_t,z_number,varname_t> arr_boxes_domain_t;
}

namespace llvm {

  using namespace std;

  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::z_lin_exp_t& e) {
    ostringstream s;
    s << e;
    o << s.str ();
    return o;
  }


  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::z_lin_cst_t& cst) {
    ostringstream s;
    s << cst;
    o << s.str ();
    return o;
  }

  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::z_lin_cst_sys_t& csts) {
    ostringstream s;
    s << csts;
    o << s.str ();
    return o;
  }

  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::interval_domain_t& inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }
  
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::ric_domain_t& inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::dbm_domain_t& inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::term_domain_t& inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab_llvm::boxes_domain_t& inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Base, typename Number, typename VariableName>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::domains::array_smashing
                                        <Base,Number,VariableName> & inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

} // end namespace llvm


namespace crab_llvm {

   //////
   /// Definition of macros
   //////

   #define DEFINE_BASE_DOMAIN(WRAPPER,ABS_DOM,ID)                                 \
   class WRAPPER: public GenericAbsDomWrapper {                                   \
     id_t m_id;                                                                   \
     ABS_DOM m_abs;                                                               \
    public:                                                                       \
     id_t getId () const { return m_id;}                                          \
     WRAPPER (ABS_DOM abs): GenericAbsDomWrapper (), m_id (ID), m_abs (abs) { }   \
     ABS_DOM get () const { return m_abs; }                                       \
     virtual z_lin_cst_sys_t to_linear_constraints () const {                     \
       ABS_DOM res (m_abs);                                                       \
       return res.to_linear_constraint_system ();}                                \
     virtual void write (std::ostream& o) const {                                 \
       ABS_DOM res (m_abs);                                                       \
       res.write (o); }                                                           \
   };                                                                             \
   template <> inline GenericAbsDomWrapperPtr                                     \
   mkGenericAbsDomWrapper (ABS_DOM abs_dom) {                                     \
     GenericAbsDomWrapperPtr res (new WRAPPER(abs_dom));                          \
     return res; }                                                                \
   template <>                                                                    \
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper,                 \
                                 ABS_DOM &abs_dom) {                              \
     auto wrappee = boost::static_pointer_cast<WRAPPER> (wrapper);                \
     abs_dom = wrappee->get (); }                                                 

   #define REGISTER_DOMAIN_ID(ABS_DOMAIN,ID)                  \
   template<>                                                 \
   inline GenericAbsDomWrapper::id_t getAbsDomId(ABS_DOMAIN)  \
   {return GenericAbsDomWrapper::ID;}                        

   #define FORGET_MACRO(ABS_DOMAIN)                              \
   do {                                                          \
     ABS_DOMAIN inv;                                             \
     getAbsDomWrappee (wrapper, inv);                            \
     crab::domain_traits::forget (inv, vs.begin (), vs.end ());  \
     return mkGenericAbsDomWrapper (inv);                        \
   } while (0) ;;


  //////
  // Generic wrapper to encapsulate an arbitrary abstract domain
  //////

  struct GenericAbsDomWrapper {

     typedef enum { intv, 
                    dbm, 
                    term, 
                    ric, 
                    boxes, 
                    arr_intv, 
                    arr_dbm, 
                    arr_term, 
                    arr_ric, 
                    arr_boxes } id_t;

     GenericAbsDomWrapper () { }

     virtual id_t getId () const = 0;
     virtual void write (std::ostream& o) const = 0;
     virtual z_lin_cst_sys_t to_linear_constraints () const = 0;
   };

   typedef boost::shared_ptr<GenericAbsDomWrapper> GenericAbsDomWrapperPtr;

   inline std::ostream& operator<<(std::ostream& o , 
                                   const GenericAbsDomWrapperPtr& v) {
     v->write (o);
     return o;
   }

   inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o , 
                                        const GenericAbsDomWrapperPtr& v) {
     ostringstream s;
     v->write (s);
     o << s.str ();
     return o;
   }

   template <typename T> 
   inline GenericAbsDomWrapperPtr mkGenericAbsDomWrapper (T abs_dom);

   template <typename T> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, T& wrappee);

   template<typename T> 
   inline GenericAbsDomWrapper::id_t getAbsDomId (T inv);

   DEFINE_BASE_DOMAIN(IntervalDomainWrapper,interval_domain_t,intv)
   DEFINE_BASE_DOMAIN(RicDomainWrapper,ric_domain_t,ric)
   DEFINE_BASE_DOMAIN(DbmDomainWrapper,dbm_domain_t,dbm)
   DEFINE_BASE_DOMAIN(TermDomainWrapper,term_domain_t,term)
   DEFINE_BASE_DOMAIN(BoxesDomainWrapper,boxes_domain_t,boxes)

   // Required only for array versions
   REGISTER_DOMAIN_ID(arr_interval_domain_t,arr_intv)
   REGISTER_DOMAIN_ID(arr_ric_domain_t,arr_ric)
   REGISTER_DOMAIN_ID(arr_dbm_domain_t,arr_dbm)
   REGISTER_DOMAIN_ID(arr_term_domain_t,arr_term)
   REGISTER_DOMAIN_ID(arr_boxes_domain_t,arr_boxes)

   template<typename B>
   class ArraySmashingDomainWrapper: public GenericAbsDomWrapper {
    public:
     
     typedef typename B::number_t N;
     typedef typename B::varname_t V;
     
    private:
     
     typedef array_smashing<B,N,V> array_smashing_t;       

     id_t m_id;     
     array_smashing_t m_abs;
     
    public:        

     id_t getId () const { return m_id;}
     
     ArraySmashingDomainWrapper(array_smashing_t abs):  
         GenericAbsDomWrapper (), m_id (getAbsDomId(abs)), m_abs (abs) { }
     
     array_smashing_t get () const {
       return m_abs;
     }
     
     z_lin_cst_sys_t to_linear_constraints () const {
       array_smashing_t res (m_abs);
       return res.to_linear_constraint_system ();
     }
     
     void write (std::ostream& o) const { 
       array_smashing_t res (m_abs);
       res.write (o);
     }
   };
      
   template <typename B> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (array_smashing<B, typename B::number_t, typename B::varname_t> abs_dom) {
     GenericAbsDomWrapperPtr res (new ArraySmashingDomainWrapper<B> (abs_dom));        
     return res;
   }

   template <typename B> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 array_smashing<B, typename B::number_t, typename B::varname_t>&abs_dom) {
     auto wrappee = boost::static_pointer_cast<ArraySmashingDomainWrapper<B> > (wrapper);
     abs_dom = wrappee->get ();
   }

   template<typename Range>
   inline GenericAbsDomWrapperPtr 
   forget (GenericAbsDomWrapperPtr wrapper, Range vs) {
     switch (wrapper->getId ()) {
       case GenericAbsDomWrapper::intv:
         FORGET_MACRO(interval_domain_t)
       case GenericAbsDomWrapper::ric: 
         FORGET_MACRO(ric_domain_t)
       case GenericAbsDomWrapper::dbm: 
         FORGET_MACRO(dbm_domain_t)
       case GenericAbsDomWrapper::term:
         FORGET_MACRO(term_domain_t)
       case GenericAbsDomWrapper::boxes: 
         FORGET_MACRO(boxes_domain_t)
       case GenericAbsDomWrapper::arr_intv:
         FORGET_MACRO(arr_interval_domain_t)
       case GenericAbsDomWrapper::arr_ric:
         FORGET_MACRO(arr_ric_domain_t) 
       case GenericAbsDomWrapper::arr_dbm: 
         FORGET_MACRO(arr_dbm_domain_t) 
       case GenericAbsDomWrapper::arr_term: 
         FORGET_MACRO(arr_term_domain_t) 
       case GenericAbsDomWrapper::arr_boxes: 
         FORGET_MACRO(arr_boxes_domain_t) 
       default: assert (false && "unreachable");
     }
   }

} // end namespace
#endif
