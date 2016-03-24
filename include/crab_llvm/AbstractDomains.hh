#ifndef __ABSTRACT_DOMAINS_HH__
#define __ABSTRACT_DOMAINS_HH__

#include "llvm/Support/ErrorHandling.h"

#include "crab_llvm/config.h"
#include "crab_llvm/CfgBuilder.hh"

#include "crab/domains/linear_constraints.hpp"                     
#include "crab/domains/intervals.hpp"                      
#include "crab/domains/dis_intervals.hpp"                      
//#include "crab/domains/dbm.hpp"
#include "crab/domains/sparse_dbm.hpp"
#include "crab/domains/split_dbm.hpp"
#include "crab/domains/naive_dbm.hpp"
#include "crab/domains/var_packing_naive_dbm.hpp"
#include "crab/domains/boxes.hpp"
#include "crab/domains/apron_domains.hpp"
#include "crab/domains/array_smashing.hpp"
#include "crab/domains/term_equiv.hpp"
#include "crab/domains/combined_domains.hpp"

/*
   Definition of the abstract domains and a generic wrapper class (for
   crab-llvm clients) to contain an arbitrary abstract domain.  
*/


namespace crab_llvm {

   ////
   // Base (non-array) numerical domains for user options
   ////

   enum CrabDomain { INTERVALS, 
                     INTERVALS_CONGRUENCES, 
                     BOXES,
                     DIS_INTERVALS,
                     ZONES, 
                     SZONES,
                     DZONES,
                     VZONES, 
                     TERMS_INTERVALS,
                     TERMS_DIS_INTERVALS,
                     NUM,
                     INTV_APRON,
                     OCT_APRON,
                     OPT_OCT_APRON,
                     PK_APRON };

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

  //////
  //// Base domains
  //////

  /// -- Intervals
  typedef interval_domain< z_number, varname_t> interval_domain_t;
  /// -- Zones with sparse DBM
  //typedef DBM<z_number, varname_t> dbm_domain_t;
  typedef SparseDBM<z_number, varname_t, SpDBM_impl::DefaultParams> dbm_domain_t;
  /// -- Zones with split DBM
  typedef SplitDBM<z_number, varname_t> sdbm_domain_t;
  /// -- Zones with dense DBM
  typedef naive_dbm< z_number, varname_t > ddbm_domain_t;
  /// -- Zones with dense DBM + var packing 
  typedef var_packing_naive_dbm<z_number, varname_t> vdbm_domain_t;
  /// -- Boxes
  typedef boxes_domain<z_number, varname_t> boxes_domain_t;
  /// -- DisIntervals
  typedef dis_interval_domain <z_number, varname_t> dis_interval_domain_t;
  /// -- Apron domains
  typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_INT > box_apron_domain_t;
  typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_OCT > oct_apron_domain_t;
  typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_OPT_OCT > opt_oct_apron_domain_t;
  typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_PK > pk_apron_domain_t;


  //////
  /// Combination/functor of domains 
  //////

  /// -- Reduced product of intervals with congruences
  typedef numerical_congruence_domain<interval_domain_t> ric_domain_t;
  /// -- Term functor domain with Intervals
  typedef crab::cfg::var_factory_impl::StrVarAlloc_col::varname_t str_varname_t;
  typedef interval_domain<z_number, str_varname_t> str_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_interval_dom_t> idom_info;
  typedef term_domain<idom_info> term_int_domain_t;  
  /// -- Term functor domain with DisIntervals
  typedef dis_interval_domain<z_number, str_varname_t> str_dis_interval_dom_t;
  typedef term::TDomInfo<z_number, varname_t, str_dis_interval_dom_t> dis_idom_info;
  typedef term_domain<dis_idom_info> term_dis_int_domain_t;  
  /// -- Reduced product of Term(DisIntervals) with split zones
  typedef reduced_numerical_domain_product2<term_dis_int_domain_t, sdbm_domain_t> num_domain_t; 
  /// -- Array smashing functor domain 
  typedef array_smashing<interval_domain_t> arr_interval_domain_t;
  typedef array_smashing<ric_domain_t> arr_ric_domain_t;
  typedef array_smashing<dbm_domain_t> arr_dbm_domain_t;
  typedef array_smashing<sdbm_domain_t> arr_sdbm_domain_t;
  typedef array_smashing<vdbm_domain_t> arr_vdbm_domain_t;
  typedef array_smashing<ddbm_domain_t> arr_ddbm_domain_t;
  typedef array_smashing<term_int_domain_t> arr_term_int_domain_t;
  typedef array_smashing<term_dis_int_domain_t> arr_term_dis_int_domain_t;
  typedef array_smashing<boxes_domain_t> arr_boxes_domain_t;
  typedef array_smashing<dis_interval_domain_t> arr_dis_interval_domain_t;
  typedef array_smashing<num_domain_t> arr_num_domain_t;
  typedef array_smashing<box_apron_domain_t> arr_box_apron_domain_t;
  typedef array_smashing<oct_apron_domain_t> arr_oct_apron_domain_t;
  typedef array_smashing<opt_oct_apron_domain_t> arr_opt_oct_apron_domain_t;
  typedef array_smashing<pk_apron_domain_t> arr_pk_apron_domain_t;

}

namespace llvm {

  using namespace std;

  #define DUMP_TO_LLVM_STREAM(T)  \
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, \
                                        T& e) {               \
    ostringstream s;                                          \
    s << e;                                                   \
    o << s.str ();                                            \
    return o; }                                                        

  DUMP_TO_LLVM_STREAM(crab_llvm::z_lin_exp_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::z_lin_cst_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::z_lin_cst_sys_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::interval_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::ric_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::dbm_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::sdbm_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::ddbm_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::vdbm_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::boxes_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::dis_interval_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::num_domain_t)

  template <typename DomInfo>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::domains::term_domain<DomInfo>& inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }


  template <typename N, typename V, crab::domains::apron_domain_id_t D>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::domains::apron_domain 
                                        <N,V,D> & inv) {
    ostringstream s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Base>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::domains::array_smashing <Base> & inv) {
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

   #define DEFINE_BASE_DOMAIN(WRAPPER,ABS_DOM,ID)                              \
   class WRAPPER: public GenericAbsDomWrapper {                                \
     id_t m_id;                                                                \
     ABS_DOM m_abs;                                                            \
    public:                                                                    \
     id_t getId () const { return m_id;}                                       \
                                                                               \
     WRAPPER (ABS_DOM abs): GenericAbsDomWrapper (), m_id (ID), m_abs (abs) { }\
                                                                               \
     ABS_DOM& get () { return m_abs; }                                         \
                                                                               \
     z_lin_cst_sys_t to_linear_constraints () {                                \
       return m_abs.to_linear_constraint_system ();                            \
     }                                                                         \
                                                                               \
     void write (std::ostream& o) {                                            \
       m_abs.write (o);                                                        \
     }                                                                         \
                                                                               \
     void forget (const vector<varname_t>& vars) {                             \
       crab::domains::domain_traits<ABS_DOM>::forget (m_abs,                   \
                                                      vars.begin (),           \
                                                      vars.end ());            \
     }                                                                         \
   };                                                                          \
                                                                               \
   template <> inline GenericAbsDomWrapperPtr                                  \
   mkGenericAbsDomWrapper (ABS_DOM abs_dom) {                                  \
     GenericAbsDomWrapperPtr res (new WRAPPER(abs_dom));                       \
     return res;                                                               \
   }                                                                           \
                                                                               \
   template <>                                                                 \
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper,              \
                                 ABS_DOM &abs_dom) {                           \
     auto wrappee = boost::dynamic_pointer_cast<WRAPPER> (wrapper);            \
     if (!wrappee) {                                                           \
       CRAB_ERROR("Could not cast wrapper to an instance of ",                 \
                  ABS_DOM::getDomainName ());                                  \
     }                                                                         \
     abs_dom = wrappee->get ();                                                \
   }                                                 

   #define REGISTER_DOMAIN_ID(ABS_DOMAIN,ID)                                   \
   template<>                                                                  \
   inline GenericAbsDomWrapper::id_t getAbsDomId(ABS_DOMAIN)                   \
   {return GenericAbsDomWrapper::ID;}                        

  //////
  // Generic wrapper to encapsulate an arbitrary abstract domain
  //////

  struct GenericAbsDomWrapper {

     typedef enum { intv, dbm, sdbm, vdbm, ddbm,
                    term_intv, term_dis_intv, 
                    ric, 
                    boxes, dis_intv,
                    intv_apron, oct_apron, opt_oct_apron, pk_apron,
                    num,
                    arr_intv, 
                    arr_dbm, arr_sdbm, arr_vdbm, arr_ddbm,
                    arr_term_intv, arr_term_dis_intv, 
                    arr_ric, 
                    arr_boxes, arr_dis_intv,
                    arr_intv_apron, arr_oct_apron, arr_opt_oct_apron, arr_pk_apron,
                    arr_num
                  } id_t;

     GenericAbsDomWrapper () { }

     virtual id_t getId () const = 0;

     virtual void write (std::ostream& o) = 0;

     virtual z_lin_cst_sys_t to_linear_constraints () = 0;

     virtual void forget(const vector<varname_t>& vars) = 0;
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
   DEFINE_BASE_DOMAIN(SDbmDomainWrapper,sdbm_domain_t,sdbm)
   DEFINE_BASE_DOMAIN(VDbmDomainWrapper,vdbm_domain_t,vdbm)
   DEFINE_BASE_DOMAIN(DDbmDomainWrapper,ddbm_domain_t,ddbm)
   DEFINE_BASE_DOMAIN(TermIntDomainWrapper,term_int_domain_t,term_intv)
   DEFINE_BASE_DOMAIN(TermDisIntDomainWrapper,term_dis_int_domain_t,term_dis_intv)
   DEFINE_BASE_DOMAIN(BoxesDomainWrapper,boxes_domain_t,boxes)
   DEFINE_BASE_DOMAIN(DisIntervalDomainWrapper,dis_interval_domain_t,dis_intv)
   DEFINE_BASE_DOMAIN(BoxApronDomainWrapper,box_apron_domain_t,intv_apron)
   DEFINE_BASE_DOMAIN(OctApronDomainWrapper,oct_apron_domain_t,oct_apron)
   DEFINE_BASE_DOMAIN(OptOctApronDomainWrapper,opt_oct_apron_domain_t,opt_oct_apron)
   DEFINE_BASE_DOMAIN(PkApronDomainWrapper,pk_apron_domain_t,pk_apron)
   DEFINE_BASE_DOMAIN(NumDomainWrapper,num_domain_t,num)

   // Required only for array versions
   REGISTER_DOMAIN_ID(arr_interval_domain_t,arr_intv)
   REGISTER_DOMAIN_ID(arr_ric_domain_t,arr_ric)
   REGISTER_DOMAIN_ID(arr_dbm_domain_t,arr_dbm)
   REGISTER_DOMAIN_ID(arr_sdbm_domain_t,arr_sdbm)
   REGISTER_DOMAIN_ID(arr_vdbm_domain_t,arr_vdbm)
   REGISTER_DOMAIN_ID(arr_ddbm_domain_t,arr_ddbm)
   REGISTER_DOMAIN_ID(arr_term_int_domain_t,arr_term_intv)
   REGISTER_DOMAIN_ID(arr_term_dis_int_domain_t,arr_term_dis_intv)
   REGISTER_DOMAIN_ID(arr_boxes_domain_t,arr_boxes)
   REGISTER_DOMAIN_ID(arr_dis_interval_domain_t,arr_dis_intv)
   REGISTER_DOMAIN_ID(arr_box_apron_domain_t,arr_intv_apron)
   REGISTER_DOMAIN_ID(arr_oct_apron_domain_t,arr_oct_apron)
   REGISTER_DOMAIN_ID(arr_opt_oct_apron_domain_t,arr_opt_oct_apron)
   REGISTER_DOMAIN_ID(arr_pk_apron_domain_t,arr_pk_apron)
   REGISTER_DOMAIN_ID(arr_num_domain_t,arr_num)

   template<typename B>
   class ArraySmashingDomainWrapper: public GenericAbsDomWrapper {
    public:
     
     typedef typename B::number_t N;
     typedef typename B::varname_t V;
     
    private:
     
     typedef array_smashing<B> array_smashing_t;       

     id_t m_id;     
     array_smashing_t m_abs;
     
    public:        

     id_t getId () const { return m_id;}
     
     ArraySmashingDomainWrapper(array_smashing_t abs):  
         GenericAbsDomWrapper (), m_id (getAbsDomId(abs)), m_abs (abs) { }
     
     array_smashing_t& get () {
       return m_abs;
     }
     
     z_lin_cst_sys_t to_linear_constraints () {
       return m_abs.to_linear_constraint_system ();
     }
     
     void write (std::ostream& o) { 
       m_abs.write (o);
     }

     void forget (const vector<varname_t>& vars) { 
       crab::domains::domain_traits<array_smashing_t>::forget (m_abs,          
                                                               vars.begin (),  
                                                               vars.end ());   
     }                                                                   
   };

   template <typename B> 
   inline GenericAbsDomWrapperPtr 
   mkGenericAbsDomWrapper (array_smashing<B> abs_dom) {
     GenericAbsDomWrapperPtr res (new ArraySmashingDomainWrapper<B> (abs_dom));        
     return res;
   }

   template <typename B> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, 
                                 array_smashing<B>&abs_dom) {
     auto wrappee = boost::dynamic_pointer_cast<ArraySmashingDomainWrapper<B> > (wrapper);
     if (!wrappee)
       CRAB_ERROR("Could not cast to instance of ArraySmashingDomainWrapper");
     abs_dom = wrappee->get ();
   }

} // end namespace
#endif
