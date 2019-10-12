#ifndef __WRAPPER_DOMAIN_HH__
#define __WRAPPER_DOMAIN_HH__

#include "crab_llvm/config.h"
#include "crab/config.h"
#include "crab_llvm/crab_domains.hh"

#include <memory>

/**
 *  Definition of a generic wrapper class (for crab-llvm clients) to
 *  contain an arbitrary abstract domain.
 *
 *  A crab-llvm client should only access to:
 *  - class GenericAbsDomWrapper and its public methods. 
 *  - getAbsDomWrappee to get the underlying domain from an
 *    GenericAbsDomWrapper object.
*/


namespace llvm {

  #define DUMP_TO_LLVM_STREAM(T)  \
  inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o, \
                                       T& e) {               \
    crab::crab_string_os s;                                  \
    s << e;                                                  \
    o << s.str ();                                           \
    return o; }                                                        

  DUMP_TO_LLVM_STREAM(crab_llvm::lin_exp_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::lin_cst_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::lin_cst_sys_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::interval_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::wrapped_interval_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::ric_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::split_dbm_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::boxes_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::dis_interval_domain_t)
  DUMP_TO_LLVM_STREAM(crab_llvm::num_domain_t)

  template <typename DomInfo>
  inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o, 
                                       crab::domains::term_domain<DomInfo>& inv) {
    crab::crab_string_os s;
    s << inv;
    o << s.str ();
    return o;
  }

  #ifdef HAVE_APRON  
  template <typename N, typename V, crab::domains::apron_domain_id_t D>
  inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o, 
  				       crab::domains::apron_domain 
  				       <N,V,D> & inv) {
    crab::crab_string_os s;
    s << inv;
    o << s.str ();
    return o;
  }
  #else 
  template <typename N, typename V, crab::domains::elina_domain_id_t D>
  inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o, 
				       crab::domains::elina_domain 
				       <N,V,D> & inv) {
    crab::crab_string_os s;
    s << inv;
    o << s.str ();
    return o;
  }
  #endif
  
  template <typename Base>
  inline llvm::raw_ostream& operator<<(llvm::raw_ostream& o, 
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

   #define DEFINE_WRAPPER(WRAPPER,ABS_DOM,ID)                        \
   class WRAPPER: public GenericAbsDomWrapper {                      \
     id_t m_id;                                                      \
     ABS_DOM m_abs;                                                  \
    public:                                                          \
    id_t getId() const { return m_id;}				     \
    								     \
    WRAPPER(ABS_DOM abs, id_t id):				     \
      GenericAbsDomWrapper(), m_id(id), m_abs(abs) { }		     \
    								     \
    WRAPPER(ABS_DOM abs):					     \
      GenericAbsDomWrapper(), m_id (ID), m_abs (abs) { }	     \
    								     \
    GenericAbsDomWrapperPtr clone() const {			     \
      auto res = std::make_shared<WRAPPER>(m_abs, m_id); 	     \
      return res;						     \
    }								     \
    								     \
    ABS_DOM& get() { return m_abs; }				     \
                                                                     \
    bool is_bottom() {						     \
      return m_abs.is_bottom();					     \
    }								     \
								     \
    bool is_top() {						     \
      return m_abs.is_top();					     \
    }								     \
  								     \
    void forget(const std::vector<var_t>& vars) {		     \
      m_abs.forget(vars);					     \
    }								     \
    								     \
    void project(const std::vector<var_t>& vars) {		     \
      m_abs.project(vars);					     \
    }								     \
  								     \
    lin_cst_sys_t to_linear_constraints() {			     \
      return m_abs.to_linear_constraint_system();		     \
    }								     \
    								     \
    void write(crab::crab_os& o) {				     \
      m_abs.write (o);						     \
    }								     \
   };                                                                \
                                                                     \
   template <> inline GenericAbsDomWrapperPtr                        \
   mkGenericAbsDomWrapper (ABS_DOM abs_dom) {                        \
     GenericAbsDomWrapperPtr res (new WRAPPER(abs_dom));             \
     return res;                                                     \
   }                                                                 \
                                                                     \
   template <>                                                       \
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper,    \
                                 ABS_DOM &abs_dom) {                 \
     auto wrappee = std::static_pointer_cast<WRAPPER> (wrapper);     \
     abs_dom = wrappee->get ();                                      \
   }                                                 

  //////
  // Generic wrapper to encapsulate an arbitrary abstract domain
  //////

  struct GenericAbsDomWrapper {

    typedef std::shared_ptr<GenericAbsDomWrapper> GenericAbsDomWrapperPtr;
    
    typedef enum { intv, split_dbm, 
		   term_intv, term_dis_intv, 
		   ric, 
		   boxes, dis_intv,
		   oct, pk,
		   num,
		   w_intv} id_t;
    
    GenericAbsDomWrapper() { }
    
    virtual ~GenericAbsDomWrapper() { }

    virtual id_t getId() const = 0;
    
    virtual GenericAbsDomWrapperPtr clone() const = 0;
    
    virtual void write(crab::crab_os& o) = 0;

    virtual bool is_bottom() = 0;

    virtual bool is_top() = 0;
      
    virtual lin_cst_sys_t to_linear_constraints() = 0;
    
    virtual void forget(const std::vector<var_t>& vars) = 0;
    
    virtual void project(const std::vector<var_t>& vars) = 0;    
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

   // Internal crab-llvm use: convert a crab domain into wrapper
   template <typename T> 
   inline GenericAbsDomWrapperPtr mkGenericAbsDomWrapper (T abs_dom);

   // For crab-llvm clients: convert a wrapper into the underlying crab domain
   template <typename T> 
   inline void getAbsDomWrappee (GenericAbsDomWrapperPtr wrapper, T& wrappee);

   DEFINE_WRAPPER(IntervalDomainWrapper,interval_domain_t,intv)
   DEFINE_WRAPPER(WrappedIntervalDomainWrapper,wrapped_interval_domain_t,w_intv)
   DEFINE_WRAPPER(RicDomainWrapper,ric_domain_t,ric)
   DEFINE_WRAPPER(SDbmDomainWrapper,split_dbm_domain_t,split_dbm)
   DEFINE_WRAPPER(TermIntDomainWrapper,term_int_domain_t,term_intv)
   DEFINE_WRAPPER(TermDisIntDomainWrapper,term_dis_int_domain_t,term_dis_intv)
   DEFINE_WRAPPER(BoxesDomainWrapper,boxes_domain_t,boxes)
   DEFINE_WRAPPER(DisIntervalDomainWrapper,dis_interval_domain_t,dis_intv)
   DEFINE_WRAPPER(OctApronDomainWrapper,oct_domain_t,oct)
   DEFINE_WRAPPER(PkApronDomainWrapper,pk_domain_t,pk)
   DEFINE_WRAPPER(NumDomainWrapper,num_domain_t,num)

} // end namespace crab_llvm

#endif
