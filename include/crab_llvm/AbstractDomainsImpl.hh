#ifndef __ABSTRACT_DOMAINS_IMPL_HH__
#define __ABSTRACT_DOMAINS_IMPL_HH__

#include "crab_llvm/config.h"
#include "crab_llvm/CfgBuilder.hh"
#include "crab_llvm/Support/AbstractDomains.hh"

/*
 Instantiations of the templated abstract domain parameters and
 generic wrapper class that contains an arbitrary abstract domain.
*/

namespace crab_llvm {

  using namespace crab::cfg_impl;
  using namespace crab::domains;
  using namespace ikos;

  typedef ikos::linear_expression<z_number, varname_t> z_lin_exp_t;
  typedef ikos::linear_constraint<z_number, varname_t> z_lin_cst_t;
  typedef ikos::linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;

  typedef interval_domain< z_number, varname_t > interval_domain_t;
  typedef interval_congruence_domain< z_number, varname_t > ric_domain_t;
  typedef DBM< z_number, varname_t > dbm_domain_t;
  typedef SplitDBM<z_number, varname_t> sdbm_domain_t;
  //typedef ikos::term::TDomInfo<z_number, varname_t, interval_domain_t> idom_info;
  typedef crab::cfg::var_factory_impl::StrVarAlloc_col::varname_t str_varname_t;
  typedef interval_domain< z_number, str_varname_t > str_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef anti_unif<idom_info>::anti_unif_t term_domain_t;  
#if 1
  // Use the reduced of intervals with boxes rather than plain boxes
  typedef rib_domain< z_number, varname_t > boxes_domain_t;
#else
  typedef boxes_domain< z_number, varname_t > boxes_domain_t;
#endif 
  typedef array_smashing<interval_domain_t,z_number,varname_t> arr_interval_domain_t;
  typedef array_smashing<ric_domain_t,z_number,varname_t> arr_ric_domain_t;
  typedef array_smashing<dbm_domain_t,z_number,varname_t> arr_dbm_domain_t;
  typedef array_smashing<sdbm_domain_t,z_number,varname_t> arr_sdbm_domain_t;
  typedef array_smashing<term_domain_t,z_number,varname_t> arr_term_domain_t;
  typedef array_smashing<boxes_domain_t,z_number,varname_t> arr_boxes_domain_t;

   //////
   // Generic wrapper to encapsulate the abstract domain
   //////

   struct GenericAbsDomWrapper {
     typedef enum { intv, dbm, term, ric, boxes, 
                    arr_intv, arr_dbm, arr_term, arr_ric, arr_boxes } id_t;

     GenericAbsDomWrapper () { }

     virtual id_t getId () const = 0;
     virtual void write (std::ostream& o) const = 0;
     virtual z_lin_cst_sys_t to_linear_constraints () const = 0;
   };

   class IntervalDomainWrapper: public GenericAbsDomWrapper {

     id_t m_id;     
     interval_domain_t m_abs;

    public:

     id_t getId () const { return m_id;}

     IntervalDomainWrapper (interval_domain_t abs): 
         GenericAbsDomWrapper (), m_id (intv), m_abs (abs) { }
     
     interval_domain_t get () const {
       return m_abs;
     }
     
     virtual z_lin_cst_sys_t to_linear_constraints () const {
       interval_domain_t res (m_abs);
       return res.to_linear_constraint_system ();
     }
     
     virtual void write (std::ostream& o) const { 
       interval_domain_t res (m_abs);
       res.write (o);
     }

   };

   class RicDomainWrapper: public GenericAbsDomWrapper {

     id_t m_id;          
     ric_domain_t m_abs;
     
    public:

     id_t getId () const { return m_id;}

     RicDomainWrapper (ric_domain_t abs): 
         GenericAbsDomWrapper (), m_id (ric), m_abs (abs) { }
     
     ric_domain_t get () const {
       return m_abs;
     }
     
     z_lin_cst_sys_t to_linear_constraints () const {
       ric_domain_t res (m_abs);
       return res.to_linear_constraint_system ();
     }
  
     void write (std::ostream& o) const { 
       ric_domain_t res (m_abs);
       res.write (o);
     }

   };

   class DbmDomainWrapper: public GenericAbsDomWrapper {

     id_t m_id;       
     dbm_domain_t m_abs;
     
    public:

     id_t getId () const { return m_id;}
     
     DbmDomainWrapper (dbm_domain_t abs): 
         GenericAbsDomWrapper (), m_id (dbm), m_abs (abs) { }
     
     dbm_domain_t get () const {
       return m_abs;
     }
     
     z_lin_cst_sys_t to_linear_constraints () const {
       dbm_domain_t res (m_abs);
          return res.to_linear_constraint_system ();
     }
     
     void write (std::ostream& o) const { 
       dbm_domain_t res (m_abs);
       res.write (o);
     }
   };

   class TermDomainWrapper: public GenericAbsDomWrapper {

     id_t m_id;     
     term_domain_t m_abs;
     
    public:

     id_t getId () const { return m_id;}
     
     TermDomainWrapper (term_domain_t abs): 
         GenericAbsDomWrapper(), m_id (term), m_abs (abs) { }
     
     term_domain_t get () const {
       return m_abs;
     }
     
     z_lin_cst_sys_t to_linear_constraints () const {
       term_domain_t res (m_abs);
       return res.to_linear_constraint_system ();
     }
     
     void write (std::ostream& o) const { 
       term_domain_t res (m_abs);
       res.write (o);
     }
   };

   class BoxesDomainWrapper: public GenericAbsDomWrapper {

     id_t m_id;     
     boxes_domain_t m_abs;
     
    public:

     id_t getId () const { return m_id;}
     
     BoxesDomainWrapper (boxes_domain_t abs): 
         GenericAbsDomWrapper (), m_id (boxes), m_abs (abs) { }
        
     boxes_domain_t get () const {
       return m_abs;
     }
     
     z_lin_cst_sys_t to_linear_constraints () const {
       boxes_domain_t res (m_abs);
          return res.to_linear_constraint_system ();
     }
     
     void write (std::ostream& o) const { 
       boxes_domain_t res (m_abs);
       res.write (o);
     }
   };

   template<typename T> 
   inline GenericAbsDomWrapper::id_t getArrSmashId (T inv);

   template<> 
   inline GenericAbsDomWrapper::id_t getArrSmashId (arr_interval_domain_t /*inv*/) 
   {return GenericAbsDomWrapper::arr_intv;}
   template<> 
   inline GenericAbsDomWrapper::id_t getArrSmashId (arr_ric_domain_t /*inv*/) 
   { return GenericAbsDomWrapper::arr_ric;}
   template<> 
   inline GenericAbsDomWrapper::id_t getArrSmashId (arr_dbm_domain_t /*inv*/) 
   { return GenericAbsDomWrapper::arr_dbm;}
   template<> 
   inline GenericAbsDomWrapper::id_t getArrSmashId (arr_term_domain_t /*inv*/) 
   { return GenericAbsDomWrapper::arr_term;}
   template<> 
   inline GenericAbsDomWrapper::id_t getArrSmashId (arr_boxes_domain_t /*inv*/) 
   { return GenericAbsDomWrapper::arr_boxes;}


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
         GenericAbsDomWrapper (), m_id (getArrSmashId(abs)), m_abs (abs) { }
     
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

   template <typename T> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (T abs_dom);
       
   template <> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (interval_domain_t abs_dom) {
     GenericAbsDomWrapperPtr res (new IntervalDomainWrapper (abs_dom));        
     return res;
   }

   template <> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (ric_domain_t abs_dom) {
     GenericAbsDomWrapperPtr res (new RicDomainWrapper (abs_dom));        
     return res;
   }

   template <> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (dbm_domain_t abs_dom) {
     GenericAbsDomWrapperPtr res (new DbmDomainWrapper (abs_dom));        
     return res;
   }

   template <> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (term_domain_t abs_dom) {
     GenericAbsDomWrapperPtr res (new TermDomainWrapper (abs_dom));        
     return res;
   }

   template <> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (boxes_domain_t abs_dom) {
     GenericAbsDomWrapperPtr res (new BoxesDomainWrapper (abs_dom));        
     return res;
   }

   template <typename B> inline GenericAbsDomWrapperPtr
   mkGenericAbsDomWrapper (array_smashing<B, typename B::number_t, typename B::varname_t> abs_dom) {
     GenericAbsDomWrapperPtr res (new ArraySmashingDomainWrapper<B> (abs_dom));        
     return res;
   }

   template <typename T> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, T& wrappee);

   template <> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 interval_domain_t &abs_dom) {
     assert (wrapper->getId () == intv);

     auto wrappee = boost::static_pointer_cast<IntervalDomainWrapper> (wrapper);
     abs_dom = wrappee->get ();
   }

   template <> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 ric_domain_t &abs_dom) {

     assert (wrapper->getId () == ric);
     auto wrappee = boost::static_pointer_cast<RicDomainWrapper> (wrapper);
     abs_dom = wrappee->get ();
   }

   template <> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 dbm_domain_t &abs_dom) {

     assert (wrapper->getId () == dbm);
     auto wrappee = boost::static_pointer_cast<DbmDomainWrapper> (wrapper);
     abs_dom = wrappee->get ();
   }

   template <> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 term_domain_t &abs_dom) {

     assert (wrapper->getId () == term);
     auto wrappee = boost::static_pointer_cast<TermDomainWrapper> (wrapper);
     abs_dom = wrappee->get ();
   }

   template <> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 boxes_domain_t &abs_dom) {

     assert (wrapper->getId () == boxes);
     auto wrappee = boost::static_pointer_cast<BoxesDomainWrapper> (wrapper);
     abs_dom = wrappee->get ();
   }

   template <typename B> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 array_smashing<B, typename B::number_t, typename B::varname_t>&abs_dom) {

     assert (wrapper->getId () == arr_smash);
     auto wrappee = boost::static_pointer_cast<ArraySmashingDomainWrapper<B> > (wrapper);
     abs_dom = wrappee->get ();
   }

#define MACRO_FORGET(WRAPPER,INV,VS)                           \
  do {                                                         \
    getAbsDomWrappee (WRAPPER, INV);                           \
    crab::domain_traits::forget (INV, VS.begin (), VS.end ()); \
    return mkGenericAbsDomWrapper (INV);                       \
} while (0)

   template<typename Range>
   inline GenericAbsDomWrapperPtr 
   forget (GenericAbsDomWrapperPtr wrapper, Range vs) {
     switch (wrapper->getId ()) {
       case GenericAbsDomWrapper::intv: {
         interval_domain_t inv;
         MACRO_FORGET(wrapper,inv,vs);
       }
       case GenericAbsDomWrapper::ric: {
         ric_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::dbm: {
         dbm_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::term: {
         term_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::boxes: {
         boxes_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::arr_intv: {
         arr_interval_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::arr_ric: {
         arr_ric_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::arr_dbm: {
         arr_dbm_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::arr_term: {
         arr_term_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       case GenericAbsDomWrapper::arr_boxes: {
         arr_boxes_domain_t inv;
         MACRO_FORGET(wrapper, inv, vs);
       }
       default: assert (false && "unreachable");
     }
   }

} // end namespace
#endif
