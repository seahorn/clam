#ifndef __WRAPPER_DOMAIN_HH__
#define __WRAPPER_DOMAIN_HH__

#include "crab_llvm/config.h"
#include "crab_llvm/crab_domains.hh"

/*
   Definition of a generic wrapper class (for crab-llvm clients) to
   contain an arbitrary abstract domain.
*/


namespace llvm {

  #define DUMP_TO_LLVM_STREAM(T)  \
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, \
                                        T& e) {               \
    crab::crab_string_os s;                                   \
    s << e;                                                   \
    o << s.str ();                                            \
    return o; }                                                        

  DUMP_TO_LLVM_STREAM(crab_llvm::z_lin_exp_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::z_lin_cst_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::z_lin_cst_sys_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::interval_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::ric_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::dbm_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::split_dbm_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::boxes_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::dis_interval_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::num_domain_t)

  template <typename DomInfo>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::domains::term_domain<DomInfo>& inv) {
    crab::crab_string_os s;
    s << inv;
    o << s.str ();
    return o;
  }


  template <typename N, typename V, crab::domains::apron_domain_id_t D>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::domains::apron_domain 
                                        <N,V,D> & inv) {
    crab::crab_string_os s;
    s << inv;
    o << s.str ();
    return o;
  }

  template <typename Base>
  inline llvm::raw_ostream& operator<< (llvm::raw_ostream& o, 
                                        crab::domains::array_smashing <Base> & inv) {
    crab::crab_string_os s;
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
     WRAPPER (ABS_DOM abs, id_t id): GenericAbsDomWrapper(), m_id(id), m_abs(abs) { } \
  									       \
     WRAPPER (ABS_DOM abs): GenericAbsDomWrapper (), m_id (ID), m_abs (abs) { }\
                                                                               \
     GenericAbsDomWrapperPtr clone () const {                                  \
       auto res = boost::make_shared<WRAPPER>(m_abs, m_id); 		       \
       return res;                                                             \
     }                                                                         \
                                                                               \
     ABS_DOM& get () { return m_abs; }                                         \
                                                                               \
     z_lin_cst_sys_t to_linear_constraints () {                                \
       return m_abs.to_linear_constraint_system ();                            \
     }                                                                         \
                                                                               \
     void write (crab::crab_os& o) {                                           \
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

    typedef boost::shared_ptr<GenericAbsDomWrapper> GenericAbsDomWrapperPtr;
    
     typedef enum { intv, dbm, split_dbm, 
                    term_intv, term_dis_intv, 
                    ric, 
                    boxes, dis_intv,
                    opt_oct_apron, pk_apron,
                    num,
                    arr_intv, 
                    arr_dbm, arr_split_dbm, 
                    arr_term_intv, arr_term_dis_intv, 
                    arr_ric, 
                    arr_boxes, arr_dis_intv,
                    arr_opt_oct_apron, arr_pk_apron,
                    arr_num
                  } id_t;

     GenericAbsDomWrapper () { }

     virtual ~GenericAbsDomWrapper () { }

     virtual id_t getId () const = 0;

     virtual GenericAbsDomWrapperPtr clone () const = 0;
    
     virtual void write (crab::crab_os& o) = 0;

     virtual z_lin_cst_sys_t to_linear_constraints () = 0;

     virtual void forget(const vector<varname_t>& vars) = 0;
   };

   typedef GenericAbsDomWrapper::GenericAbsDomWrapperPtr GenericAbsDomWrapperPtr;

   inline crab::crab_os& operator<<(crab::crab_os& o , 
                                    const GenericAbsDomWrapperPtr& v) {
     v->write (o);
     return o;
   }

   inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o , 
                                        const GenericAbsDomWrapperPtr& v) {
     crab::crab_string_os s;
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
   DEFINE_BASE_DOMAIN(SDbmDomainWrapper,split_dbm_domain_t,split_dbm)
   DEFINE_BASE_DOMAIN(TermIntDomainWrapper,term_int_domain_t,term_intv)
   DEFINE_BASE_DOMAIN(TermDisIntDomainWrapper,term_dis_int_domain_t,term_dis_intv)
   DEFINE_BASE_DOMAIN(BoxesDomainWrapper,boxes_domain_t,boxes)
   DEFINE_BASE_DOMAIN(DisIntervalDomainWrapper,dis_interval_domain_t,dis_intv)
   DEFINE_BASE_DOMAIN(OptOctApronDomainWrapper,opt_oct_apron_domain_t,opt_oct_apron)
   DEFINE_BASE_DOMAIN(PkApronDomainWrapper,pk_apron_domain_t,pk_apron)
   DEFINE_BASE_DOMAIN(NumDomainWrapper,num_domain_t,num)

   // Required only for array versions
   REGISTER_DOMAIN_ID(arr_interval_domain_t,arr_intv)
   REGISTER_DOMAIN_ID(arr_ric_domain_t,arr_ric)
   REGISTER_DOMAIN_ID(arr_dbm_domain_t,arr_dbm)
   REGISTER_DOMAIN_ID(arr_split_dbm_domain_t,arr_split_dbm)
   REGISTER_DOMAIN_ID(arr_term_int_domain_t,arr_term_intv)
   REGISTER_DOMAIN_ID(arr_term_dis_int_domain_t,arr_term_dis_intv)
   REGISTER_DOMAIN_ID(arr_boxes_domain_t,arr_boxes)
   REGISTER_DOMAIN_ID(arr_dis_interval_domain_t,arr_dis_intv)
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
     typedef ArraySmashingDomainWrapper<B> this_type;
     
     id_t m_id;     
     array_smashing_t m_abs;
     
    public:        

     id_t getId () const { return m_id;}
     
     ArraySmashingDomainWrapper(array_smashing_t abs):  
         GenericAbsDomWrapper (), m_id (getAbsDomId(abs)), m_abs (abs) { }

     GenericAbsDomWrapperPtr clone () const {
       auto res = boost::make_shared<this_type>(m_abs);
       return res; 
     }
     
     array_smashing_t& get () {
       return m_abs;
     }
     
     z_lin_cst_sys_t to_linear_constraints () {
       return m_abs.to_linear_constraint_system ();
     }
     
     void write (crab::crab_os& o) { 
       m_abs.write (o);
     }

     void forget (const vector<varname_t>& vars) { 
       crab::domains::domain_traits<array_smashing_t>::
	 forget (m_abs,vars.begin (),vars.end ());
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

} // end namespace crab_llvm

#endif
