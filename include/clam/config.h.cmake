#pragma once

/** Define whether llvm-dsa library is available */
#cmakedefine HAVE_DSA ${HAVE_DSA}

/** Define whether sea-dsa library is available */
#cmakedefine HAVE_SEA_DSA ${HAVE_SEA_DSA}

/** Define whether llvm-seahorn is available */
#cmakedefine HAVE_LLVM_SEAHORN ${HAVE_LLVM_SEAHORN}

/** Include all abstract domains */
#cmakedefine HAVE_ALL_DOMAINS ${HAVE_ALL_DOMAINS}

/** Include inter-procedural analysis */
#cmakedefine HAVE_INTER ${HAVE_INTER}

