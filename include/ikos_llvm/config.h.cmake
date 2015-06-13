#ifndef _IKOS_LLVM_CONFIG_H_
#define _IKOS_LLVM_CONFIG_H_

/* Define whether ikos-core is available */
#cmakedefine HAVE_IKOS ${HAVE_IKOS}

/** Major version of IKOS library */
#cmakedefine IKOS_MAJOR_VERSION ${IKOS_MAJOR_VERSION}

/** Minor version of IKOS library */
#cmakedefine IKOS_MINOR_VERSION ${IKOS_MINOR_VERSION}

/** Define whether DSA library is available */
#cmakedefine HAVE_DSA ${HAVE_DSA}

#endif
