#ifndef _CRAB_LLVM_CONFIG_H_
#define _CRAB_LLVM_CONFIG_H_

/* Define whether crab is available */
#cmakedefine HAVE_CRAB ${HAVE_CRAB}

/** Major version of crab library */
#cmakedefine CRAB_MAJOR_VERSION ${CRAB_MAJOR_VERSION}

/** Minor version of crab library */
#cmakedefine CRAB_MINOR_VERSION ${CRAB_MINOR_VERSION}

/** Define whether DSA library is available */
#cmakedefine HAVE_DSA ${HAVE_DSA}

/** Define whether llvm-seahorn is available */
#cmakedefine HAVE_LLVM_SEAHORN ${HAVE_LLVM_SEAHORN}

#endif
